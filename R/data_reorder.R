#' Find, reorder or remove specific columns
#'
#' Safe and intuitive functions to reorder or remove variables in dataframes.
#'
#' @inheritParams data_rename
#' @param pattern,starts_with,ends_with Character strings.
#' @param cols Vector of column names.
#'
#' @examples
#' # Reorder columns
#' head(data_reorder(iris, c("Species", "Sepal.Length")))
#' head(data_reorder(iris, c("Species", "dupa"))) # Safe for non-existing cols
#'
#' @inherit data_rename seealso
#' @export
data_reorder <- function(data, cols, safe = TRUE, ...) {
  remaining_columns <- setdiff(colnames(data), cols)
  if (isTRUE(safe)) cols <- cols[cols %in% names(data)]
  data[, c(cols, remaining_columns)]
}
