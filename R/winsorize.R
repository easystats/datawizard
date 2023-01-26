#' Winsorize data
#'
#' @details
#'
#' Winsorizing or winsorization is the transformation of statistics by limiting
#' extreme values in the statistical data to reduce the effect of possibly
#' spurious outliers. The distribution of many statistics can be heavily
#' influenced by outliers. A typical strategy is to set all outliers (values
#' beyond a certain threshold) to a specified percentile of the data; for
#' example, a `90%` winsorization would see all data below the 5th percentile set
#' to the 5th percentile, and data above the 95th percentile set to the 95th
#' percentile. Winsorized estimators are usually more robust to outliers than
#' their more standard forms.
#'
#' @return
#'
#' A data frame with winsorized columns or a winsorized vector.
#'
#' @param data data frame or vector.
#' @param threshold The amount of winsorization, depends on the value of `method`:
#' - For `method = "percentile"`: the amount to winsorize from *each* tail.
#'   The value of `threshold` must be between 0 and 0.5 and of length 1.
#' - For `method = "zscore"`: the number of *SD*/*MAD*-deviations from the
#'   *mean*/*median* (see `robust`). The value of `threshold` must be greater
#'   than 0 and of length 1.
#' - For `method = "raw"`: a vector of length 2 with the lower and upper bound
#'   for winsorization.
#' @param method One of "percentile" (default), "zscore", or "raw".
#' @param robust Logical, if TRUE, winsorizing through the "zscore" method is
#'   done via the median and the median absolute deviation (MAD); if FALSE, via
#'   the mean and the standard deviation.
#' @param ... Currently not used.
#' @param verbose Not used anymore since `datawizard` 0.6.6.
#'
#' @examples
#' hist(iris$Sepal.Length, main = "Original data")
#'
#' hist(winsorize(iris$Sepal.Length, threshold = 0.2),
#'   xlim = c(4, 8), main = "Percentile Winsorization"
#' )
#'
#' hist(winsorize(iris$Sepal.Length, threshold = 1.5, method = "zscore"),
#'   xlim = c(4, 8), main = "Mean (+/- SD) Winsorization"
#' )
#'
#' hist(winsorize(iris$Sepal.Length, threshold = 1.5, method = "zscore", robust = TRUE),
#'   xlim = c(4, 8), main = "Median (+/- MAD) Winsorization"
#' )
#'
#' hist(winsorize(iris$Sepal.Length, threshold = c(5, 7.5), method = "raw"),
#'   xlim = c(4, 8), main = "Raw Thresholds"
#' )
#'
#' # Also works on a data frame:
#' winsorize(iris, threshold = 0.2)
#'
#' @inherit data_rename seealso
#' @export
winsorize <- function(data, ...) {
  UseMethod("winsorize")
}


#' @export
winsorize.factor <- function(data, ...) {
  data
}

#' @export
winsorize.character <- winsorize.factor

#' @export
winsorize.logical <- winsorize.factor

#' @export
winsorize.data.frame <- function(data,
                                 threshold = 0.2,
                                 method = "percentile",
                                 robust = FALSE,
                                 verbose = TRUE,
                                 ...) {
  data[] <- lapply(
    data,
    winsorize,
    threshold = threshold,
    method = method,
    robust = robust,
    verbose = verbose
  )
  data
}

#' @rdname winsorize
#' @export
winsorize.numeric <- function(data,
                              threshold = 0.2,
                              method = "percentile",
                              robust = FALSE,
                              verbose = TRUE,
                              ...) {
  method <- match.arg(method, choices = c("percentile", "zscore", "raw"))

  if (method == "raw" && length(threshold) != 2L) {
    insight::format_error(
      "`threshold` must be of length 2 for lower and upper bound."
    )
  }

  if (method == "percentile") {
    if (threshold < 0 || threshold > 0.5) {
      insight::format_error(
        "`threshold` for winsorization must be a scalar between 0 and 0.5."
      )
    }

    y <- sort(data)
    n <- length(data)
    ibot <- floor(threshold * n) + 1
    itop <- length(data) - ibot + 1

    threshold <- c(y[ibot], y[itop])
  }

  if (method == "zscore") {
    if (threshold <= 0) {
      insight::format_error(
        "'threshold' for winsorization must be a scalar greater than 0."
      )
    }

    if (isTRUE(robust)) {
      centeral <- stats::median(data, na.rm = TRUE)
      deviation <- stats::mad(data, center = centeral, na.rm = TRUE)
    } else {
      centeral <- mean(data, na.rm = TRUE)
      deviation <- stats::sd(data, na.rm = TRUE)
    }

    threshold <- centeral + c(-1, 1) * deviation * threshold
  }


  data[data < threshold[1]] <- threshold[1]
  data[data > threshold[2]] <- threshold[2]
  return(data)
}
