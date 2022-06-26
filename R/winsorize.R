#' Winsorize data
#'
#' @details
#'
#' Winsorizing or winsorization is the transformation of statistics by limiting
#' extreme values in the statistical data to reduce the effect of possibly
#' spurious outliers. The distribution of many statistics can be heavily
#' influenced by outliers. A typical strategy is to set all outliers (values
#' beyond a certain threshold) to a specified percentile of the data; for
#' example, a 90\% winsorization would see all data below the 5th percentile set
#' to the 5th percentile, and data above the 95th percentile set to the 95th
#' percentile. Winsorized estimators are usually more robust to outliers than
#' their more standard forms.
#'
#' @return
#'
#' A dataframe with winsorized columns or a winsorized vector.
#'
#' @param data Dataframe or vector.
#' @param threshold The amount of winsorization.
#' @param verbose Toggle warnings.
#' @param method One of "percentile" or "zscore".
#' @param robust Logical, if TRUE, winsorizing through the "zscore" method is done via the median and the median absolute deviation (MAD); if FALSE, via the mean and the standard deviation.
#' @param ... Currently not used.
#'
#' @examples
#' winsorize(iris$Sepal.Length, threshold = 0.2)
#' winsorize(iris$Sepal.Length, threshold = 2, method = "zscore")
#' winsorize(iris$Sepal.Length, threshold = 2, method = "zscore", robust = TRUE)
#' winsorize(iris, threshold = 0.2)
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
winsorize.data.frame <- function(data, threshold = 0.2, verbose = TRUE, robust = FALSE, ...) {
  out <- sapply(data, winsorize, threshold = threshold, verbose = verbose, robust = robust)
  as.data.frame(out)
}

#' @rdname winsorize
#' @export
winsorize.numeric <- function(data, threshold = 0.2, verbose = TRUE, method = "percentile", robust = FALSE, ...) {
  if(method == "percentile") {

    if (threshold < 0 || threshold > 0.5) {
      if (isTRUE(verbose)) {
        warning("'threshold' for winsorization must be a scalar between 0 and 0.5. Did not winsorize data.", call. = FALSE)
        }
      return(data)
      }

    y <- sort(data)
    n <- length(data)
    ibot <- floor(threshold * n) + 1
    itop <- length(data) - ibot + 1
    xbot <- y[ibot]
    xtop <- y[itop]

    winval <- data
    winval[winval <= xbot] <- xbot
    winval[winval >= xtop] <- xtop
    return(winval)
  }

  if(method == "zscore") {

    if (threshold <= 0) {
      if (isTRUE(verbose)) {
        warning("'threshold' for winsorization must be a scalar greater than 0. Did not winsorize data.", call. = FALSE)
      }
      return(data)
    }

    if(isTRUE(robust)) {
      med <- stats::median(data, na.rm = TRUE)
      y <- data - med
      winval <- stats::mad(y, center = 0, na.rm = TRUE) * threshold
      y[y > winval] <- winval
      y[y < -winval] <- -winval
      return(y + med)
    }

    if(isFALSE(robust)) {
      m <- mean(data, na.rm = TRUE)
      y <- data - m
      winval <- stats::sd(y, na.rm = TRUE) * threshold
      y[y > winval] <- winval
      y[y < -winval] <- -winval
      y + m
    }
  }
}
