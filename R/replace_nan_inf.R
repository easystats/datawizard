#' @title Convert infinite or `NaN` values into `NA`
#' @name replace_nan_inf
#'
#' @description
#' Replaces all infinite (`Inf` and `-Inf`) or `NaN` values with `NA`.
#'
#' @param data A vector or a data frame.
#'
#' @return
#' Data with `Inf`, `-Inf`, and `NaN` converted to `NA`.
#'
#' @examples
#' # a vector
#' x <- c(1, 2, NA, 3, NaN, 4, NA, 5, Inf, -Inf, 6, 7)
#' replace_nan_inf(x)
#'
#' # a data frame
#' df <- data.frame(
#'   x = c(1, NA, 5, Inf, 2, NA),
#'   y = c(3, NaN, 4, -Inf, 6, 7),
#'   stringsAsFactors = FALSE
#' )
#' replace_nan_inf(df)
#' @export

replace_nan_inf <- function(data) {
  if (is.data.frame(data)) {
    # iterate variables of data frame
    data[] <- lapply(data, function(i) {
      # convert `NaN` and `Inf` to missing
      i[is.nan(i)] <- NA
      i[is.infinite(i)] <- NA
      i
    })
  } else {
    data[is.nan(data)] <- NA
    data[is.infinite(data)] <- NA
  }

  data
}
