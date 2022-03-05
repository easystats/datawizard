#' @param fixed Logical, if `TRUE`, `pattern` is treated as a string to be
#'   matched as is, else `pattern` is treated as regular expression.
#' @rdname data_relocate
#' @examples
#' # Remove columns
#' head(data_remove(iris, "Sepal.Length"))
#' @export
data_remove <- function(data, pattern, fixed = FALSE, ...) {
  new <- data[!grepl(pattern, names(data), fixed = fixed)]
  attributes(new) <- utils::modifyList(attributes(data), attributes(new))
  class(new) <- class(data)
  new
}
