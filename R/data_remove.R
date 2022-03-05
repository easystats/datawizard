#' @rdname data_relocate
#' @examples
#' # Remove columns
#' head(data_remove(iris, "Sepal.Length"))
#' @export
data_remove <- function(data, pattern, ...) {
  new <- data[!grepl(pattern, names(data))]
  attributes(new) <- utils::modifyList(attributes(data), attributes(new))
  class(new) <- class(data)
  new
}
