#' @inheritParams data_extract
#' @rdname data_relocate
#' @examples
#' # Remove columns
#' head(data_remove(iris, "Sepal.Length"))
#' head(data_remove(iris, starts_with("Sepal")))
#' @export
data_remove <- function(data, pattern, ignore_case = FALSE, verbose = FALSE, ...) {

  ## TODO set verbose = TRUE by default in a later update?

  # evaluate arguments
  pattern <- .pattern_nse(pattern, data, exclude = NULL, ignore_case)

  # nothing to remove?
  if (!length(pattern)) {
    return(data)
  }

  new <- data[!colnames(data) %in% pattern]
  attributes(new) <- utils::modifyList(attributes(data), attributes(new))
  class(new) <- class(data)

  new
}
