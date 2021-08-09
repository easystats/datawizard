#' @rdname data_rename
#' @examples
#' # Add prefix / suffix to all columns
#' head(data_addprefix(iris, "NEW_"))
#' head(data_addsuffix(iris, "_OLD"))
#' @export
data_addprefix <- function(data, pattern, ...) {
  names(data) <- paste0(pattern, names(data))
  data
}


#' @rdname data_rename
#' @export
data_addsuffix <- function(data, pattern, ...) {
  names(data) <- paste0(names(data), pattern)
  data
}
