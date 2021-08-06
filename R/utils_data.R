#' help-functions
#' @keywords internal
#' @noRd
.data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}
