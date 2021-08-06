#' @rdname data_rename
#' @export
data_addprefix <- function(data, pattern) {
  names(data) <- paste0(pattern, names(data))
  data
}


#' @rdname data_rename
#' @export
data_addsuffix <- function(data, pattern) {
  names(data) <- paste0(names(data), pattern)
  data
}
