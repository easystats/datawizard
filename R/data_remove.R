#' @rdname data_rename
#' @importFrom utils modifyList
#' @export
data_remove <- function(data, pattern) {
  new <- data[!names(data) %in% pattern]
  attributes(new) <- utils::modifyList(attributes(data), attributes(new))
  class(new) <- class(data)
  new
}
