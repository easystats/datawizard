#' @title Row means with minimum amount of valid values
#' @name rowmean_n
#' @description This function is similar to the SPSS `MEAN.n` function and computes
#' row means from a data frame or matrix if at least `n` values of a row are
#' valid (and not `NA`).
#'
#' @param data A data frame with at least two columns, where row means are applied.
#' @param n A numeric value of length 1. May either be
#' - a numeric value that indicates the amount of valid values per row to
#'   calculate the row mean;
#' - or a value between 0 and 1, indicating a proportion of valid values per
#'   row to calculate the row mean (see 'Details').
#'
#' If a row's sum of valid values is less than `n`, `NA` will be returned.
#' @param digits Numeric value indicating the number of decimal places to be
#' used for rounding mean values. Negative values are allowed (see 'Details').
#' By default, `digits = NULL` and no rounding is used.
#' @param verbose Toggle warnings.
#'
#' @return A vector with row means for those rows with at least `n` valid values.
#'
#' @details Rounding to a negative number of `digits` means rounding to a power of
#' ten, for example `rowmean_n(df, 3, digits = -2)` rounds to the nearest hundred.
#' For `n`, must be a numeric value from `0` to `ncol(data)`. If a row in the
#' data frame has at least `n` non-missing values, the row mean is returned. If
#' `n` is a non-integer value from 0 to 1, `n` is considered to indicate the
#' proportion of required non-missing values per row. E.g., if `n = 0.75`, a
#' row must have at least `ncol(data) * n` non-missing values for the row mean
#' to be calculated. See 'Examples'.
#'
#' @examples
#' dat <- data.frame(
#'   c1 = c(1, 2, NA, 4),
#'   c2 = c(NA, 2, NA, 5),
#'   c3 = c(NA, 4, NA, NA),
#'   c4 = c(2, 3, 7, 8)
#' )
#'
#' # needs at least 4 non-missing values per row
#' rowmean_n(dat, 4) # 1 valid return value
#'
#' # needs at least 3 non-missing values per row
#' rowmean_n(dat, 3) # 2 valid return values
#'
#' # needs at least 2 non-missing values per row
#' rowmean_n(dat, 2)
#'
#' # needs at least 1 non-missing value per row
#' rowmean_n(dat, 1) # all means are shown
#'
#' # needs at least 50% of non-missing values per row
#' rowmean_n(dat, 0.5) # 3 valid return values
#'
#' # needs at least 75% of non-missing values per row
#' rowmean_n(dat, 0.75) # 2 valid return values
#'
#' @export
rowmean_n <- function(data, n, digits = NULL, verbose = TRUE) {
  # check if data is data frame or matrix
  if (!is.data.frame(data) && !is.matrix(data)) {
    insight::format_error("`data` must be a data frame or matrix.")
  }

  # coerce matrix to data frame
  if (is.matrix(data)) {
    data <- as.data.frame(data)
  }

  # n must be a numeric, non-missing value
  if (is.null(n) || all(is.na(n)) || !is.numeric(n) || length(n) > 1) {
    insight::format_error("`n` must be a numeric value of length 1.")
  }

  # make sure we only have numeric values
  numeric_columns <- vapply(data, is.numeric, TRUE)
  if (!all(numeric_columns)) {
    if (verbose) {
      insight::format_alert("Only numeric columns are considered for calculation.")
    }
    data <- data[numeric_columns]
  }

  # check if we have a data framme with at least two columns
  if (ncol(data) < 2) {
    insight::format_error("`data` must be a data frame with at least two columns.")
  }

  # is 'n' indicating a proportion?
  decimals <- n %% 1
  if (decimals != 0) {
    n <- round(ncol(data) * decimals)
  }

  # n may not be larger as df's amount of columns
  if (ncol(data) < n) {
    insight::format_error("`n` must be smaller or equal to number of columns in data frame.")
  }

  # row means
  out <- apply(data, 1, function(x) ifelse(sum(!is.na(x)) >= n, mean(x, na.rm = TRUE), NA))

  # round, if requested
  if (!is.null(digits) && !all(is.na(digits))) {
    out <- round(out, digits = digits)
  }
  out
}
