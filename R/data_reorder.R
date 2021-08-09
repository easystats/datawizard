#' @rdname data_rename
#' @examples
#' # Reorder columns
#' head(data_reorder(iris, c("Species", "Sepal.Length")))
#' head(data_reorder(iris, c("Species", "dupa"))) # Safe for non-existing cols
#' @export
data_reorder <- function(data, cols, safe = TRUE, ...) {
  remaining_columns <- setdiff(colnames(data), cols)
  if (isTRUE(safe)) cols <- cols[cols %in% names(data)]
  data[, c(cols, remaining_columns)]
}
