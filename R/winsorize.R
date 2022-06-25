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
#' @param robust Logical, if TRUE, winsorizing is done via the median absolute deviation (MAD).
#' @param ... Currently not used.
#'
#' @examples
#' winsorize(iris$Sepal.Length, threshold = 0.2)
#' winsorize(iris$Sepal.Length, threshold = 3, robust = TRUE)
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
winsorize.numeric <- function(data, threshold = 0.2, verbose = TRUE, robust = FALSE, ...) {
  if(robust == FALSE) {

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

  if(robust == TRUE) {

    if (threshold <= 0) {
      if (isTRUE(verbose)) {
        warning("'threshold' for winsorization must be a scalar greater than 0. Did not winsorize data.", call. = FALSE)
      }
      return(data)
    }

    med <- stats::median(data, na.rm = TRUE)
    y <- data - med
    sc <- stats::mad(y, center = 0, na.rm = TRUE) * threshold
    y[y > sc] <- sc
    y[y < -sc] <- -sc
    y + med
  }
}
