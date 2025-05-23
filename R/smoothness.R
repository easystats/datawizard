#' Quantify the smoothness of a vector
#'
#' @param x Numeric vector (similar to a time series).
#' @param method Can be `"diff"` (the standard deviation of the standardized
#'   differences) or `"cor"` (default, lag-one autocorrelation).
#' @param lag An integer indicating which lag to use. If less than `1`, will be
#'   interpreted as expressed in percentage of the length of the vector.
#' @inheritParams skewness
#'
#' @examples
#' x <- (-10:10)^3 + rnorm(21, 0, 100)
#' plot(x)
#' smoothness(x, method = "cor")
#' smoothness(x, method = "diff")
#' @return Value of smoothness.
#' @references https://stats.stackexchange.com/questions/24607/how-to-measure-smoothness-of-a-time-series-in-r
#'
#' @export
smoothness <- function(x,
                       method = "cor",
                       lag = 1,
                       iterations = NULL,
                       ...) {
  UseMethod("smoothness")
}


#' @export
smoothness.numeric <- function(x,
                               method = "cor",
                               lag = 1,
                               iterations = NULL,
                               ...) {
  if (lag < 1) {
    lag <- round(lag * length(x))
  }
  if (lag <= 0) {
    insight::format_error("'lag' cannot be that small.")
  }

  if (method == "cor") {
    smooth_data <- stats::cor(utils::head(x, length(x) - lag), utils::tail(x, length(x) - lag))
  } else {
    smooth_data <- stats::sd(diff(x, lag = lag)) / abs(mean(diff(x, lag = lag)))
  }

  if (!is.null(iterations)) {
    if (requireNamespace("boot", quietly = TRUE)) {
      results <- boot::boot(
        data = x,
        statistic = .boot_smoothness,
        R = iterations,
        method = method,
        lag = lag
      )
      out_se <- stats::sd(results$t, na.rm = TRUE)
      smooth_data <- data.frame(Smoothness = smooth_data, SE = out_se)
    } else {
      insight::format_warning("Package 'boot' needed for bootstrapping SEs.")
    }
  }

  class(smooth_data) <- unique(c("parameters_smoothness", class(smooth_data)))
  smooth_data
}


#' @export
smoothness.data.frame <- function(x,
                                  method = "cor",
                                  lag = 1,
                                  iterations = NULL,
                                  ...) {
  .smoothness <-
    lapply(
      x,
      smoothness,
      method = method,
      lag = lag,
      iterations = iterations
    )
  .smoothness <- cbind(Parameter = names(.smoothness), do.call(rbind, .smoothness))
  class(.smoothness) <- unique(c("parameters_smoothness", class(.smoothness)))
  .smoothness
}


#' @export
smoothness.default <- function(x,
                               method = "cor",
                               lag = 1,
                               iterations = NULL,
                               ...) {
  smoothness(
    .factor_to_numeric(x),
    method = method,
    lag = lag,
    iterations = iterations
  )
}


# bootstrapping -----------------------------------

.boot_smoothness <- function(data, indices, method, lag) {
  datawizard::smoothness(
    x = data[indices],
    method = method,
    lag = lag,
    iterations = NULL
  )
}


# methods -----------------------------------------

#' @export
as.numeric.parameters_smoothness <- function(x, ...) {
  if (is.data.frame(x)) {
    x$Smoothness
  } else {
    as.vector(x)
  }
}

#' @export
as.double.parameters_smoothness <- as.numeric.parameters_smoothness
