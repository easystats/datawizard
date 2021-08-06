#' @rdname data_rename
#' @export
data_reorder <- function(data, cols, safe = TRUE) {
  remaining_columns <- setdiff(colnames(data), cols)
  if (isTRUE(safe)) cols <- cols[cols %in% names(data)]
  data[, c(cols, remaining_columns)]
}
