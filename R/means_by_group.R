#' @title Summary of mean values by group
#' @name means_by_group
#'
#' @description Computes mean, sd and se for each sub-group (indicated by \code{grp})
#'                of \code{dv}.
#'
#' @param x A vector or a data frame.
#' @param select to do Name of the dependent variable, for which the mean value, grouped
#'   by \code{grp}, is computed.
#' @param group Factor with the cross-classifying variable, where \code{dv} is
#'   grouped into the categories represented by \code{grp}. Numeric vectors
#'   are coerced to factors.
#' @param weights Name of variable in \code{x} that indicated the vector of
#'   weights that will be applied to weight all observations. Default is
#'   \code{NULL}, so no weights are used.
#' @param digits Numeric, amount of digits after decimal point when rounding
#'   estimates and values.
#' @param ...
#'
#' @return For non-grouped data frames, \code{means_by_group()} returns a data frame with
#'   following columns: \code{term}, \code{mean}, \code{N}, \code{std.dev},
#'   \code{std.error} and \code{p.value}. For grouped data frames, returns
#'   a list of such data frames.
#'
#' @details This function performs a One-Way-Anova with \code{dv} as dependent
#'   and \code{grp} as independent variable, by calling
#'   \code{lm(count ~ as.factor(grp))}. Then \code{\link[emmeans]{contrast}}
#'   is called to get p-values for each sub-group. P-values indicate whether
#'   each group-mean is significantly different from the total mean.
#'
#' @examples
#' data(efc)
#' means_by_group(efc, c12hour, e42dep)
#'
#' data(iris)
#' means_by_group(iris, Sepal.Width, Species)
#'
#' # also works for grouped data frames
#' if (require("dplyr")) {
#'   efc %>%
#'     group_by(c172code) %>%
#'     means_by_group(c12hour, e42dep)
#' }
#'
#' # weighting
#' efc$weight <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))
#' means_by_group(efc, c12hour, e42dep, weights = weight)
#' @export
means_by_group <- function(x, ...) {
  UseMethod("means_by_group")
}


#' @export
means_by_group.default <- function(x, ...) {
  insight::format_error("`means_by_group()` does not work for objects of class `", class(x)[1], "`.")
}


#' @export
means_by_group.numeric <- function(x, group = NULL, weights = NULL, digits = NULL, ...) {
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

  # create string with variable names
  data <- stats::na.omit(data.frame(
    x = x,
    group = to_factor(group),
    weights = weights,
    stringsAsFactors = FALSE
  ))

  # get grouped means table
  out <- .means_by_group(data)

  # retrieve labels
  var_mean_label <- attr(x, "label", exact = TRUE)
  var_grp_label <- attr(group, "label", exact = TRUE)

  # if no labels present, use variable names directlf y
  if (is.null(var_mean_label)) {
    var_mean_label <- deparse(substitute(x))
  }
  if (is.null(var_grp_label)) {
    var_grp_label <- deparse(substitute(group))
  }

  # attributes
  attr(out, "var_mean_label") <- var_mean_label
  attr(out, "var_grp_label") <- var_grp_label
  attr(out, "digits") <- digits

  class(out) <- c("dw_groupmeans", "data.frame")
  out
}


#' @export
means_by_group.data.frame <- function(x,
                                      select = NULL,
                                      group = NULL,
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

  out <- lapply(select, function(i) {
    means_by_group(x[[i]], group = x[[group]], weights = x[[weights]], digits = digits, ...)
  })

  out
}


#' @keywords internal
.means_by_group <- function(data) {
  # compute anova statistics for mean table
  if (is.null(data$weights) || all(data$weights == 1)) {
    fit <- stats::lm(x ~ group, data = data)
  } else {
    fit <- stats::lm(x ~ group, weights = weights, data = data)
  }

  # summary table data
  out <- stats::aggregate(data["x"], data["group"], weighted_mean, weights = data$weights)
  out_sd <- stats::aggregate(data["x"], data["group"], weighted_sd, weights = data$weights)
  out_weights <- stats::aggregate(data["weights"], data["group"], sum)

  colnames(out) <- c("Category", "Mean")
  out$N <- out_weights[[2]]
  out$SD <- out_sd[[2]]

  # p-values of contrast-means
  # means.p <- fit %>%
  #   emmeans::emmeans(specs = "grp") %>%
  #   emmeans::contrast(method = "eff") %>%
  #   summary() %>%
  #   dplyr::pull("p.value")

  # finally, add total-row
  out <- rbind(
    out,
    data.frame(
      Category = "Total",
      Mean = weighted_mean(data$x, weights = data$weights),
      N = nrow(data),
      SD = weighted_sd(data$x, weights = data$weights),
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
  insight::format_table(x, digits = digits, ...)
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
    "\nAnova: R2=", insight::format_value(attributes(out)$r2, digits = 3),
    "; adj.R2=", insight::format_value(attributes(out)$adj.r2, digits = 3),
    "; F=", insight::format_value(attributes(out)$fstat, digits = 3),
    "; ", insight::format_p(attributes(out)$p.value, whitespace = FALSE),
    "\n"
  )

  cat(insight::export_table(x, caption = caption, footer = footer, ...))
}
