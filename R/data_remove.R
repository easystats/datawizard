#' @rdname data_rename
#' @importFrom utils modifyList
#' @examples
#' # Remove columns
#' head(data_remove(iris, "Sepal.Length"))
#' @export
data_remove <- function(data, pattern, ...) {
  new <- data[!names(data) %in% pattern]
  attributes(new) <- utils::modifyList(attributes(data), attributes(new))
  class(new) <- class(data)
  new
}
