#' @title Summary of mean values by group
#' @name means_by_group
#'
#' @description Computes summary table of means by groups.
#'
#' @param x A vector or a data frame.
#' @param group If `x` is a numeric vector, `group` should be a factor that
#' indicates the group-classifying categories. If `x` is a data frame, `group`
#' should be a character string, naming the variable in `x` that is used for
#' grouping. Numeric vectors are coerced to factors. Not that `group` should
#' only refer to a single variable.
#' @param ci Level of confidence interval for mean estimates. Default is `0.95`.
#' Use `ci = NA` to suppress confidence intervals.
#' @param weights If `x` is a numeric vector, `weights` should be a vector of
#' weights that will be applied to weight all observations. If `x` is a data
#' frame, `weights` can also be a character string indicating the name of the
#' variable in `x` that should be used for weighting. Default is `NULL`, so no
#' weights are used.
#' @param digits Optional scalar, indicating the amount of digits after decimal
#' point when rounding estimates and values.
#' @param ... Currently not used
#' @inheritParams find_columns
#'
#' @return A data frame with information on mean and further summary statistics
#' for each sub-group.
#'
#' @details This function is comparable to `aggregate(x, group, mean)`, but provides
#' some further information, including summary statistics from a One-Way-ANOVA
#' using `x` as dependent and `group` as independent variable. [`emmeans::contrast()`]
#' is used to get p-values for each sub-group. P-values indicate whether each
#' group-mean is significantly different from the total mean.
#'
#' @examples
#' data(efc)
#' means_by_group(efc, "c12hour", "e42dep")
#'
#' data(iris)
#' means_by_group(iris, "Sepal.Width", "Species")
#'
#' # weighting
#' efc$weight <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))
#' means_by_group(efc, "c12hour", "e42dep", weights = "weight")
#' @export
means_by_group <- function(x, ...) {
  UseMethod("means_by_group")
}


#' @export
means_by_group.default <- function(x, ...) {
  insight::format_error("`means_by_group()` does not work for objects of class `", class(x)[1], "`.")
}


#' @rdname means_by_group
#' @export
means_by_group.numeric <- function(x,
                                   group = NULL,
                                   ci = 0.95,
                                   weights = NULL,
                                   digits = NULL,
                                   ...) {
  # sanity check for arguments

  # "group" must be provided
  if (is.null(group)) {
    insight::format_error("Argument `group` is missing.")
  }

  # group must be of same length as x
  if (length(group) != length(x)) {
    insight::format_error("Argument `group` must be of same length as `x`.")
  }

  # if weights are provided, must be of same length as x
  if (!is.null(weights) && length(weights) != length(x)) {
    insight::format_error("Argument `weights` must be of same length as `x`.")
  }

  # if weights are NULL, set weights to 1
  if (is.null(weights)) weights <- rep(1, length(x))

  # retrieve labels
  var_mean_label <- attr(x, "label", exact = TRUE)
  var_grp_label <- attr(group, "label", exact = TRUE)

  # if no labels present, use variable names directly
  if (is.null(var_mean_label)) {
    var_mean_label <- deparse(substitute(x))
  }
  if (is.null(var_grp_label)) {
    var_grp_label <- deparse(substitute(group))
  }

  # coerce group to factor if numeric, or convert labels to levels, if factor
  if (is.factor(group)) {
    group <- tryCatch(labels_to_levels(group, verbose = FALSE), error = function(e) group)
  } else {
    group <- to_factor(group)
  }

  data <- stats::na.omit(data.frame(
    x = x,
    group = group,
    weights = weights,
    stringsAsFactors = FALSE
  ))

  # get grouped means table
  out <- .means_by_group(data, ci = ci)

  # attributes
  attr(out, "var_mean_label") <- var_mean_label
  attr(out, "var_grp_label") <- var_grp_label
  attr(out, "digits") <- digits

  class(out) <- c("dw_groupmeans", "data.frame")
  out
}


#' @rdname means_by_group
#' @export
means_by_group.data.frame <- function(x,
                                      select = NULL,
                                      group = NULL,
                                      ci = 0.95,
                                      weights = NULL,
                                      digits = NULL,
                                      exclude = NULL,
                                      ignore_case = FALSE,
                                      regex = FALSE,
                                      verbose = TRUE,
                                      ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  if (is.null(weights)) {
    w <- NULL
  } else if (is.character(weights)) {
    w <- x[[weights]]
  } else {
    w <- weights
  }

  out <- lapply(select, function(i) {
    # if no labels present, use variable names directy
    if (is.null(attr(x[[i]], "label", exact = TRUE))) {
      attr(x[[i]], "label") <- i
    }
    if (is.null(attr(x[[group]], "label", exact = TRUE))) {
      attr(x[[group]], "label") <- group
    }
    # compute means table
    means_by_group(x[[i]], group = x[[group]], ci = ci, weights = w, digits = digits, ...)
  })

  class(out) <- c("dw_groupmeans_list", "list")
  out
}


#' @keywords internal
.means_by_group <- function(data, ci = 0.95) {
  # compute anova statistics for mean table
  if (is.null(data$weights) || all(data$weights == 1)) {
    fit <- stats::lm(x ~ group, data = data)
  } else {
    fit <- stats::lm(x ~ group, weights = data$weights, data = data)
  }

  # summary table data
  groups <- split(data$x, data$group)
  group_weights <- split(data$weights, data$group)
  out <- do.call(rbind, Map(function(x, w) {
    data.frame(
      Mean = weighted_mean(x, weights = w),
      SD = weighted_sd(x, weights = w),
      N = round(sum(w)),
      stringsAsFactors = FALSE
    )
  }, groups, group_weights))

  # add group names
  out$Category <- levels(data$group)
  out$p <- out$CI_high <- out$CI_low <- NA

  # p-values of contrast-means
  if (insight::check_if_installed("emmeans", quietly = TRUE)) {
    # create summary table of contrasts, for p-values and confidence intervals
    predicted <- emmeans::emmeans(fit, specs = "group", level = ci)
    contrasts <- emmeans::contrast(predicted, method = "eff")
    # add p-values and confidence intervals to "out"
    if (!is.null(ci) && !is.na(ci)) {
      summary_table <- as.data.frame(predicted)
      out$CI_low <- summary_table$lower.CL
      out$CI_high <- summary_table$upper.CL
    }
    summary_table <- as.data.frame(contrasts)
    out$p <- summary_table$p.value
  }

  # reorder columns
  out <- out[c("Category", "Mean", "N", "SD", "CI_low", "CI_high", "p")]

  # finally, add total-row
  out <- rbind(
    out,
    data.frame(
      Category = "Total",
      Mean = weighted_mean(data$x, weights = data$weights),
      N = nrow(data),
      SD = weighted_sd(data$x, weights = data$weights),
      CI_low = NA,
      CI_high = NA,
      p = NA,
      stringsAsFactors = FALSE
    )
  )

  # get anova statistics for mean table
  sum.fit <- summary(fit)

  # r-squared values
  r2 <- sum.fit$r.squared
  r2.adj <- sum.fit$adj.r.squared

  # F-statistics
  fstat <- sum.fit$fstatistic
  pval <- stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)

  # copy as attributes
  attr(out, "r2") <- r2
  attr(out, "ci") <- ci
  attr(out, "adj.r2") <- r2.adj
  attr(out, "fstat") <- fstat[1]
  attr(out, "p.value") <- pval

  out
}


# methods -----------------

#' @export
format.dw_groupmeans <- function(x, digits = NULL, ...) {
  if (is.null(digits)) {
    digits <- attr(x, "digits", exact = TRUE)
  }
  if (is.null(digits)) {
    digits <- 2
  }
  x$N <- insight::format_value(x$N, digits = 0)
  insight::format_table(remove_empty_columns(x), digits = digits, ...)
}

#' @export
print.dw_groupmeans <- function(x, digits = NULL, ...) {
  out <- format(x, digits = digits, ...)

  # caption
  l1 <- attributes(x)$var_mean_label
  l2 <- attributes(x)$var_grp_label
  if (!is.null(l1) && !is.null(l2)) {
    caption <- c(paste0("# Mean of ", l1, " by ", l2), "blue")
  } else {
    caption <- NULL
  }

  # footer
  footer <- paste0(
    "\nAnova: R2=", insight::format_value(attributes(x)$r2, digits = 3),
    "; adj.R2=", insight::format_value(attributes(x)$adj.r2, digits = 3),
    "; F=", insight::format_value(attributes(x)$fstat, digits = 3),
    "; ", insight::format_p(attributes(x)$p.value, whitespace = FALSE),
    "\n"
  )

  cat(insight::export_table(out, caption = caption, footer = footer, ...))
}

#' @export
print.dw_groupmeans_list <- function(x, digits = NULL, ...) {
  for (i in seq_along(x)) {
    if (i > 1) cat("\n")
    print(x[[i]], digits = digits, ...)
  }
}
