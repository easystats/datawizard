#' Transpose a dataframe
#'
#' Transpose a dataframe. It's the equivalent of using \code{t()} but restores the data.frame class, and prints a warning if the data type is modified (see example).
#'
#' @inheritParams data_rename
#' @param verbose Silence warnings and/or messages by setting it to \code{FALSE}.
#'
#' @examples
#' transposed <- data_transpose(iris)
#' transposed[1:5]
#'
#' transposed <- data_transpose(iris[1:4]) # Only numeric = no warning
#' @export
data_transpose <- function(data, verbose = TRUE, ...) {
  if (length(unique(sapply(data, class))) > 1) {
    if (verbose) {
      warning("Your data contains mixed types of data. After transposition, all will be transformed into characters.")
    }
  }
  new <- as.data.frame(t(data))

  # Restore names
  colnames(new) <- rownames(data)
  rownames(new) <- colnames(data)

  new
}
