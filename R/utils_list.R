#' Remove empty elements from lists
#'
#' @param x A list or vector.
#' @param remove_na Logical to decide if `NA`s should be removed.
#'
#' @examples
#' compact_list(list(NULL, 1, c(NA, NA)))
#' compact_list(c(1, NA, NA))
#' compact_list(c(1, NA, NA), remove_na = TRUE)
#' @export

compact_list <- function(x, remove_na = FALSE) {
  if (remove_na) {
    x[!sapply(x, function(i) length(i) == 0L || is.null(i) || (length(i) == 1L & is.na(i)) || any(i == "NULL", na.rm = TRUE))]
  } else {
    x[!sapply(x, function(i) length(i) == 0L || is.null(i) || any(i == "NULL", na.rm = TRUE))]
  }
}
