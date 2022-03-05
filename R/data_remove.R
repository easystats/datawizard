#' @param fixed Logical, if `TRUE`, `pattern` is treated as a string to be
#'   matched as is, else `pattern` is treated as regular expression.
#' @rdname data_relocate
#' @examples
#' # Remove columns
#' head(data_remove(iris, "Sepal.Length"))
#' head(data_remove(iris, starts_with("Sepal")))
#' @export
data_remove <- function(data, pattern, fixed = TRUE, ...) {
  # check if pattern is a function like "starts_with()",
  # then override "fixed" argument
  p <- substitute(pattern)

  # evaluate pattern, can be function like "starts_with()"
  pattern <- tryCatch(eval(p), error = function(e) NULL)

  if (is.null(pattern)) {
    fixed <- FALSE
    pattern <- .evaluate_pattern(.safe_deparse(p))
  }

  if (isTRUE(fixed)) {
    # we can have multiple patterns here
    new <- data[!names(data) %in% pattern]
  } else {
    new <- data[!grepl(pattern, names(data))]
  }
  attributes(new) <- utils::modifyList(attributes(data), attributes(new))
  class(new) <- class(data)
  new
}
