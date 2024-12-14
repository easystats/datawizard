#' Describe a distribution
#'
#' This function describes a distribution by a set of indices (e.g., measures of
#' centrality, dispersion, range, skewness, kurtosis).
#'
#' @param x A numeric vector, a character vector, a data frame, or a list. See
#' `Details`.
#' @param range Return the range (min and max).
#' @param quartiles Return the first and third quartiles (25th and 75pth
#'   percentiles).
#' @param include_factors Logical, if `TRUE`, factors are included in the
#'   output, however, only columns for range (first and last factor levels) as
#'   well as n and missing will contain information.
#' @param ci Confidence Interval (CI) level. Default is `NULL`, i.e. no
#'   confidence intervals are computed. If not `NULL`, confidence intervals
#'   are based on bootstrap replicates (see `iterations`). If
#'   `centrality = "all"`, the bootstrapped confidence interval refers to
#'   the first centrality index (which is typically the median).
#' @param iterations The number of bootstrap replicates for computing confidence
#'   intervals. Only applies when `ci` is not `NULL`.
#' @param iqr Logical, if `TRUE`, the interquartile range is calculated
#'   (based on [stats::IQR()], using `type = 6`).
#' @param verbose Toggle warnings and messages.
#' @inheritParams bayestestR::point_estimate
#' @inheritParams extract_column_names
#'
#' @details If `x` is a data frame, only numeric variables are kept and will be
#' displayed in the summary.
#'
#' If `x` is a list, the behavior is different whether `x` is a stored list. If
#' `x` is stored (for example, `describe_distribution(mylist)` where `mylist`
#' was created before), artificial variable names are used in the summary
#' (`Var_1`, `Var_2`, etc.). If `x` is an unstored list (for example,
#' `describe_distribution(list(mtcars$mpg))`), then `"mtcars$mpg"` is used as
#' variable name.
#'
#' @note There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#'   implemented in the
#'   \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @return A data frame with columns that describe the properties of the variables.
#'
#' @examplesIf require("bayestestR", quietly = TRUE)
#' describe_distribution(rnorm(100))
#'
#' data(iris)
#' describe_distribution(iris)
#' describe_distribution(iris, include_factors = TRUE, quartiles = TRUE)
#' describe_distribution(list(mtcars$mpg, mtcars$cyl))
#' @export
describe_distribution <- function(x, ...) {
  UseMethod("describe_distribution")
}


#' @export
describe_distribution.default <- function(x, verbose = TRUE, ...) {
  if (verbose) {
    insight::format_warning(
      paste0("Can't describe variables of class `", class(x)[1], "`.")
    )
  }
  NULL
}


#' @export
describe_distribution.list <- function(x,
                                       centrality = "mean",
                                       dispersion = TRUE,
                                       iqr = TRUE,
                                       range = TRUE,
                                       quartiles = FALSE,
                                       ci = NULL,
                                       include_factors = FALSE,
                                       iterations = 100,
                                       threshold = 0.1,
                                       verbose = TRUE,
                                       ...) {
  factor_el <- which(vapply(x, is.factor, FUN.VALUE = logical(1L)))
  num_el <- which(vapply(x, is.numeric, FUN.VALUE = logical(1L)))

  # get elements names as is
  # ex: `list(mtcars$mpg, mtcars$cyl) -> c("mtcars$mpg", "mtcars$cyl")`
  nm <- vapply(sys.call()[[2]], insight::safe_deparse, FUN.VALUE = character(1L))[-1]

  if (isTRUE(include_factors)) {
    x <- x[c(num_el, factor_el)]
    if (length(nm) != 0) {
      nm <- nm[c(num_el, factor_el)]
    }
  } else {
    x <- x[num_el]
    if (length(nm) != 0) {
      nm <- nm[num_el]
    }
  }

  # Not possible to obtain elements names if they are stored in
  # an object
  if (length(nm) == 0) {
    nm <- paste0("Var_", seq_along(x))
  }

  # The function currently doesn't support descriptive summaries for character
  # or factor types.
  out <- do.call(rbind, lapply(x, function(i) {
    if ((include_factors && is.factor(i)) || (!is.character(i) && !is.factor(i))) {
      describe_distribution(
        i,
        centrality = centrality,
        dispersion = dispersion,
        iqr = iqr,
        range = range,
        quartiles = quartiles,
        ci = ci,
        iterations = iterations,
        threshold = threshold,
        verbose = verbose
      )
    }
  }))


  if (is.null(names(x))) {
    new_names <- nm
  } else {
    empty_names <- which(!nzchar(names(x), keepNA = TRUE))
    new_names <- names(x)
    new_names[empty_names] <- nm[empty_names]
  }

  out$Variable <- new_names
  row.names(out) <- NULL
  out <- out[c("Variable", setdiff(colnames(out), "Variable"))]

  class(out) <- unique(c("parameters_distribution", "see_parameters_distribution", class(out)))
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  attr(out, "ci") <- ci
  attr(out, "threshold") <- threshold
  if (centrality == "all") attr(out, "first_centrality") <- colnames(out)[2]
  out
}


#' @rdname describe_distribution
#' @export
describe_distribution.numeric <- function(x,
                                          centrality = "mean",
                                          dispersion = TRUE,
                                          iqr = TRUE,
                                          range = TRUE,
                                          quartiles = FALSE,
                                          ci = NULL,
                                          iterations = 100,
                                          threshold = 0.1,
                                          verbose = TRUE,
                                          ...) {
  insight::check_if_installed("bayestestR")
  out <- data.frame(.temp = 0)

  # Missing
  n_missing <- sum(is.na(x))
  x <- stats::na.omit(x)

  # Point estimates
  out <- cbind(
    out,
    bayestestR::point_estimate(
      x,
      centrality = centrality,
      dispersion = dispersion,
      threshold = threshold,
      ...
    )
  )


  # interquartile range, type same as minitab and SPSS
  if (iqr) {
    out$IQR <- stats::IQR(x, na.rm = TRUE, type = 6)
  }


  # Confidence Intervals
  if (!is.null(ci)) {
    insight::check_if_installed("boot")
    results <- tryCatch(
      {
        boot::boot(
          data = x,
          statistic = .boot_distribution,
          R = iterations,
          centrality = centrality
        )
      },
      error = function(e) {
        msg <- conditionMessage(e)
        if (!is.null(msg) && msg == "sample is too sparse to find TD") {
          insight::format_warning(
            "When bootstrapping CIs, sample was too sparse to find TD. Returning NA for CIs."
          )
          list(t = c(NA_real_, NA_real_))
        }
      }
    )
    out_ci <- bayestestR::ci(results$t, ci = ci, verbose = FALSE)
    out <- cbind(out, data.frame(CI_low = out_ci$CI_low[1], CI_high = out_ci$CI_high[1]))
  }


  # Range
  if (range) {
    out <- cbind(
      out,
      data.frame(
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE)
      )
    )
  }

  # Quartiles
  if (quartiles) {
    out <- cbind(
      out,
      data.frame(
        Q1 = stats::quantile(x, probs = 0.25, na.rm = TRUE),
        Q3 = stats::quantile(x, probs = 0.75, na.rm = TRUE)
      )
    )
  }

  # Skewness
  out <- cbind(
    out,
    data.frame(
      Skewness = as.numeric(skewness(x, verbose = verbose)),
      Kurtosis = as.numeric(kurtosis(x, verbose = verbose))
    )
  )

  out$n <- length(x)
  out$n_Missing <- n_missing
  out$.temp <- NULL

  class(out) <- unique(c("parameters_distribution", "see_parameters_distribution", class(out)))
  attr(out, "data") <- x
  attr(out, "ci") <- ci
  attr(out, "threshold") <- threshold
  if (centrality == "all") attr(out, "first_centrality") <- colnames(out)[1]
  out
}


#' @rdname describe_distribution
#' @export
describe_distribution.factor <- function(x,
                                         dispersion = TRUE,
                                         range = TRUE,
                                         verbose = TRUE,
                                         ...) {
  # Missing
  n_missing <- sum(is.na(x))
  x <- stats::na.omit(x)

  out <- data.frame(
    Mean = NA,
    SD = NA,
    CI_low = NA,
    CI_high = NA,
    IQR = NA,
    Min = levels(x)[1],
    Max = levels(x)[nlevels(x)],
    Q1 = NA,
    Q3 = NA,
    Skewness = as.numeric(skewness(x, verbose = verbose)),
    Kurtosis = as.numeric(kurtosis(x, verbose = verbose)),
    n = length(x),
    n_Missing = n_missing,
    stringsAsFactors = FALSE
  )


  if (!dispersion) {
    out$SD <- NULL
  }


  dot_args <- list(...)

  if (is.null(dot_args[["ci"]])) {
    out$CI_low <- NULL
    out$CI_high <- NULL
  }

  if (is.null(dot_args[["iqr"]]) || isFALSE(dot_args[["iqr"]])) {
    out$IQR <- NULL
  }

  if (is.null(dot_args[["quartiles"]]) || isFALSE(dot_args[["quartiles"]])) {
    out$Q1 <- NULL
    out$Q3 <- NULL
  }

  if (!range) {
    out$Min <- NULL
    out$Max <- NULL
  }

  class(out) <- unique(c("parameters_distribution", "see_parameters_distribution", class(out)))
  attr(out, "data") <- x
  out
}


#' @export
describe_distribution.character <- function(x,
                                            dispersion = TRUE,
                                            range = TRUE,
                                            verbose = TRUE,
                                            ...) {
  # Missing
  n_missing <- sum(is.na(x))
  x <- stats::na.omit(x)
  values <- unique(x)

  out <- data.frame(
    Mean = NA,
    SD = NA,
    IQR = NA,
    CI_low = NA,
    CI_high = NA,
    Min = values[1],
    Max = values[length(values)],
    Q1 = NA,
    Q3 = NA,
    Skewness = as.numeric(skewness(x, verbose = verbose)),
    Kurtosis = as.numeric(kurtosis(x, verbose = verbose)),
    n = length(x),
    n_Missing = n_missing,
    stringsAsFactors = FALSE
  )


  if (!dispersion) {
    out$SD <- NULL
  }


  dot_args <- list(...)
  if (is.null(dot_args[["ci"]])) {
    out$CI_low <- NULL
    out$CI_high <- NULL
  }

  if (is.null(dot_args[["iqr"]]) || isFALSE(dot_args[["iqr"]])) {
    out$IQR <- NULL
  }

  if (is.null(dot_args[["quartiles"]]) || isFALSE(dot_args[["quartiles"]])) {
    out$Q1 <- NULL
    out$Q3 <- NULL
  }

  if (!range) {
    out$Min <- NULL
    out$Max <- NULL
  }

  class(out) <- unique(c("parameters_distribution", "see_parameters_distribution", class(out)))
  attr(out, "data") <- x
  out
}


#' @rdname describe_distribution
#' @export
describe_distribution.data.frame <- function(x,
                                             select = NULL,
                                             exclude = NULL,
                                             centrality = "mean",
                                             dispersion = TRUE,
                                             iqr = TRUE,
                                             range = TRUE,
                                             quartiles = FALSE,
                                             include_factors = FALSE,
                                             ci = NULL,
                                             iterations = 100,
                                             threshold = 0.1,
                                             ignore_case = FALSE,
                                             regex = FALSE,
                                             verbose = TRUE,
                                             ...) {
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )
  # The function currently doesn't support descriptive summaries for character
  # or factor types.
  out <- do.call(rbind, lapply(x[select], function(i) {
    if ((include_factors && is.factor(i)) || (!is.character(i) && !is.factor(i))) {
      describe_distribution(
        i,
        centrality = centrality,
        dispersion = dispersion,
        iqr = iqr,
        range = range,
        quartiles = quartiles,
        ci = ci,
        iterations = iterations,
        threshold = threshold,
        verbose = verbose
      )
    }
  }))

  if (is.null(out)) {
    return(NULL)
  }

  out$Variable <- row.names(out)
  row.names(out) <- NULL
  out <- out[c("Variable", setdiff(colnames(out), "Variable"))]

  class(out) <- unique(c("parameters_distribution", "see_parameters_distribution", class(out)))
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  attr(out, "ci") <- ci
  attr(out, "threshold") <- threshold
  if (centrality == "all") attr(out, "first_centrality") <- colnames(out)[2]
  out
}


#' @export
describe_distribution.grouped_df <- function(x,
                                             select = NULL,
                                             exclude = NULL,
                                             centrality = "mean",
                                             dispersion = TRUE,
                                             iqr = TRUE,
                                             range = TRUE,
                                             quartiles = FALSE,
                                             include_factors = FALSE,
                                             ci = NULL,
                                             iterations = 100,
                                             threshold = 0.1,
                                             ignore_case = FALSE,
                                             regex = FALSE,
                                             verbose = TRUE,
                                             ...) {
  group_vars <- setdiff(colnames(attributes(x)$groups), ".rows")
  group_data <- expand.grid(lapply(x[group_vars], function(i) unique(sort(i))))
  groups <- split(x, x[group_vars])
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  out <- do.call(rbind, lapply(seq_along(groups), function(i) {
    d <- describe_distribution.data.frame(
      groups[[i]][select],
      centrality = centrality,
      dispersion = dispersion,
      iqr = iqr,
      range = range,
      quartiles = quartiles,
      include_factors = include_factors,
      ci = ci,
      iterations = iterations,
      threshold = threshold,
      ...
    )


    d[[".group"]] <-
      paste(sprintf(
        "%s=%s",
        group_vars,
        vapply(group_data[i, ], as.character, FUN.VALUE = character(1L))
      ), collapse = " | ")

    d
  }))

  class(out) <- unique(c("parameters_distribution", "see_parameters_distribution", class(out)))
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  attr(out, "ci") <- ci
  attr(out, "threshold") <- threshold
  if (centrality == "all") attr(out, "first_centrality") <- colnames(out)[2]
  out
}


# methods ------------------

#' @export
print.parameters_distribution <- function(x, digits = 2, ...) {
  formatted_table <- format(
    x,
    digits = digits,
    format = "text",
    ci_width = NULL,
    ci_brackets = TRUE,
    ...
  )
  cat(insight::export_table(formatted_table, format = "text", digits = digits, ...))
  invisible(x)
}


# bootstrapping CIs ----------------------------------

.boot_distribution <- function(data, indices, centrality) {
  out <- datawizard::describe_distribution(
    data[indices],
    centrality = centrality,
    dispersion = FALSE,
    iqr = FALSE,
    range = FALSE,
    quartiles = FALSE,
    ci = NULL
  )
  out[[1]]
}
