# distribution_mode ----------------------------------

#' Compute mode for a statistical distribution
#'
#' @param x An atomic vector, a list, or a data frame.
#'
#' @return
#'
#' The value that appears most frequently in the provided data.
#' The returned data structure will be the same as the entered one.
#'
#' @seealso For continuous variables, the
#'   **Highest Maximum a Posteriori probability estimate (MAP)** may be
#'   a more useful way to estimate the most commonly-observed value
#'   than the mode. See [bayestestR::map_estimate()].
#'
#' @examples
#'
#' distribution_mode(c(1, 2, 3, 3, 4, 5))
#' distribution_mode(c(1.5, 2.3, 3.7, 3.7, 4.0, 5))
#'
#' @export
distribution_mode <- function(x) {
  # TODO: Add support for weights, trim, binned (method)
  uniqv <- unique(x)
  tab <- tabulate(match(x, uniqv))
  idx <- which.max(tab)
  uniqv[idx]
}

#' Compute the coefficient of variation
#'
#' Compute the coefficient of variation (CV, ratio of the standard deviation to
#' the mean, \eqn{\sigma/\mu}) for a set of numeric values.
#'
#' @return The computed coefficient of variation for `x`.
#' @export
#'
#' @examples
#' coef_var(1:10)
#' coef_var(c(1:10, 100), method = "median_mad")
#' coef_var(c(1:10, 100), method = "qcd")
#' coef_var(mu = 10, sigma = 20)
#' coef_var(mu = 10, sigma = 20, method = "unbiased", n = 30)
coef_var <- function(x, ...) {
  UseMethod("coef_var")
}
#' @name distribution_cv
#' @rdname coef_var
#' @export
distribution_coef_var <- coef_var

#' @export
coef_var.default <- function(x, verbose = TRUE, ...) {
  if (verbose) {
    insight::format_warning(
      paste0("Can't compute the coefficient of variation objects of class `", class(x)[1], "`.")
    )
  }
  NULL
}

#' @param x A numeric vector of ratio scale (see details), or vector of values than can be coerced to one.
#' @param mu A numeric vector of mean values to use to compute the coefficient
#'   of variation. If supplied, `x` is not used to compute the mean.
#' @param sigma A numeric vector of standard deviation values to use to compute the coefficient
#'   of variation. If supplied, `x` is not used to compute the SD.
#' @param method Method to use to compute the CV. Can be `"standard"` to compute
#'   by dividing the standard deviation by the mean, `"unbiased"` for the
#'   unbiased estimator for normally distributed data, or one of two robust
#'   alternatives: `"median_mad"` to divide the median by the [stats::mad()],
#'   or `"qcd"` (quartile coefficient of dispersion, interquartile range divided
#'   by the sum of the quartiles \[twice the midhinge\]: \eqn{(Q_3 - Q_1)/(Q_3 + Q_1)}.
#' @param trim the fraction (0 to 0.5) of values to be trimmed from
#'   each end of `x` before the mean and standard deviation (or other measures)
#'   are computed. Values of `trim` outside the range of (0 to 0.5) are taken
#'   as the nearest endpoint.
#' @param remove_na Logical. Should `NA` values be removed before computing (`TRUE`)
#'   or not (`FALSE`, default)?
#' @param n If `method = "unbiased"` and both `mu` and `sigma` are provided (not
#'   computed from `x`), what sample size to use to adjust the computed CV
#'   for small-sample bias?
#' @param ... Further arguments passed to computation functions.
#'
#' @details
#' CV is only applicable of values taken on a ratio scale: values that have a
#' *fixed* meaningfully defined 0 (which is either the lowest or highest
#' possible value), and that ratios between them are interpretable For example,
#' how many sandwiches have I eaten this week? 0 means "none" and 20 sandwiches
#' is 4 times more than 5 sandwiches. If I were to center the number of
#' sandwiches, it will no longer be on a ratio scale (0 is no "none" it is the
#' mean, and the ratio between 4 and -2 is not meaningful). Scaling a ratio
#' scale still results in a ratio scale. So I can re define "how many half
#' sandwiches did I eat this week ( = sandwiches * 0.5) and 0 would still mean
#' "none", and 20 half-sandwiches is still 4 times more than 5 half-sandwiches.
#'
#' This means that CV is **NOT** invariant to shifting, but it is to scaling:

#' ```{r}
#' sandwiches <- c(0, 4, 15, 0, 0, 5, 2, 7)
#' coef_var(sandwiches)
#'
#' coef_var(sandwiches / 2) # same
#'
#' coef_var(sandwiches + 4) # different! 0 is no longer meaningful!
#' ````
#'
#' @rdname coef_var
#'
#' @export
coef_var.numeric <- function(x, mu = NULL, sigma = NULL,
                             method = c("standard", "unbiased", "median_mad", "qcd"),
                             trim = 0, remove_na = FALSE, n = NULL, ...) {
  # TODO: Support weights
  if (!missing(x) && all(c(-1, 1) %in% sign(x))) {
    insight::format_error("Coefficient of variation only applicable for ratio scale variables.")
  }
  method <- match.arg(method, choices = c("standard", "unbiased", "median_mad", "qcd"))
  if (is.null(mu) || is.null(sigma)) {
    if (isTRUE(remove_na)) {
      x <- .drop_na(x)
    }
    n <- length(x)
    x <- .trim_values(x, trim = trim, n = n)
  }
  if (is.null(mu)) {
    mu <- switch(method,
      standard = ,
      unbiased = mean(x, ...),
      median_mad = stats::median(x, ...),
      qcd = unname(sum(stats::quantile(x, probs = c(0.25, 0.75), ...)))
    )
  }
  if (is.null(sigma)) {
    sigma <- switch(method,
      standard = ,
      unbiased = stats::sd(x, ...),
      median_mad = stats::mad(x, center = mu, ...),
      qcd = unname(diff(stats::quantile(x, probs = c(0.25, 0.75), ...)))
    )
  }
  out <- sigma / mu
  if (method == "unbiased") {
    if (is.null(n)) {
      insight::format_error(
        "A value for `n` must be provided when `method = \"unbiased\"` and both `mu` and `sigma` are provided."
      )
    }
    # from DescTools::CoefVar
    out <- out * (1 - 1 / (4 * (n - 1)) + 1 / n * out^2 + 1 / (2 * (n - 1)^2))
  }
  return(out)
}




# descriptives helpers

.drop_na <- function(x) {
  x[!is.na(x)]
}

.trim_values <- function(x, trim = 0, n = NULL, weights = NULL) {
  # TODO: Support weights
  if (!is.numeric(trim) || length(trim) != 1L) {
    insight::format_error("`trim` must be a single numeric value.")
  }
  if (is.null(n)) {
    n <- length(x)
  }
  if (trim > 0 && n) {
    if (anyNA(x)) {
      return(NA_real_)
    }
    if (trim >= 0.5) {
      return(stats::median(x, na.rm = FALSE))
    }
    lo <- floor(n * trim) + 1
    hi <- n + 1 - lo
    x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
  }
  x
}
