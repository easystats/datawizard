#' @title Find the value at a specific position in a variable
#' @name value_at
#'
#' @description This function can be used to compute summary statistics for a
#' data frame or a matrix.
#'
#' @param x A vector or factor.
#' @param position An integer or a vector of integers, indicating the position(s)
#' of the value(s) to be returned. Negative values are counted from the end of
#' the vector. If `NA`, an error is thrown.
#' @param remove_na Logical, if `TRUE`, missing values are removed before
#' computing the position. If `FALSE`, missing values are included in the
#' computation.
#' @param default The value to be returned if the position is out of range.
#'
#' @return A vector with the value(s) at the specified position(s).
#'
#' @examples
#' data(mtcars)
#' # 5th value
#' value_at(mtcars$mpg, 5)
#' # last value
#' value_at(mtcars$mpg, -1)
#' # out of range, return default
#' value_at(mtcars$mpg, 150)
#' # return 2nd and fifth value
#' value_at(mtcars$mpg, c(2, 5))
#' @export
value_at <- function(x, position = 1, default = NULL, remove_na = FALSE) {
  if (remove_na) {
    x <- x[!is.na(x)]
  }
  n <- length(x)
  unlist(lapply(position, .values_at, x = x, n = n, default = default), use.names = FALSE)
}

# helper ----

.values_at <- function(x, position, n, default) {
  if (is.na(position)) {
    insight::format_error("`position` can't be `NA`.")
  }
  if (position < 0L) {
    position <- position + n + 1
  }
  if (position <= 0 || position > n) {
    return(default)
  }
  x[position]
}
