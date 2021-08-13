#' Find row indices of a data frame matching a specific condition
#'
#' Find row indices of a data frame that match a specific condition.
#'
#' @param x A data frame.
#' @param to A data frame matching the specified conditions.
#' @inheritParams data_rename
#'
#' @return
#'
#' A dataframe containing rows that match the specified configuration.
#'
#' @examples
#' matching_rows <- data_match(mtcars, data.frame(vs = 0, am = 1))
#' mtcars[matching_rows, ]
#'
#' matching_rows <- data_match(mtcars, data.frame(vs = 0, am = c(0, 1)))
#' mtcars[matching_rows, ]
#' @export
data_match <- function(x, to, ...) {

  # Input checks
  if (!is.data.frame(to)) to <- as.data.frame(to)

  # Find matching rows
  idx <- 1:nrow(x)
  for (col in names(to)) {
    if (col %in% names(x)) {
      idx <- idx[x[[col]][idx] %in% to[[col]]]
    }
  }

  to_numeric(row.names(x)[idx])
}
