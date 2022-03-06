#' @param fixed Logical, if `TRUE`, `pattern` is treated as a string to be
#'   matched as is, else `pattern` is treated as regular expression.
#' @inheritParams data_extract
#' @rdname data_relocate
#' @examples
#' # Remove columns
#' head(data_remove(iris, "Sepal.Length"))
#' head(data_remove(iris, starts_with("Sepal")))
#' @export
data_remove <- function(data, pattern, fixed = TRUE, ignore_case = FALSE, ...) {
  # in case pattern is a variable from another function call...
  p <- try(eval(pattern), silent = TRUE)
  if (inherits(p, c("try-error", "simpleError"))) {
    p <- substitute(pattern)
  }

  # check if pattern is a function like "starts_with()",
  # then override "fixed" argument
  pattern <- tryCatch(eval(p), error = function(e) NULL)

  if (is.null(pattern)) {
    evaluated_pattern <- .evaluate_pattern(.safe_deparse(p), data)
    pattern <- evaluated_pattern$pattern
    fixed <- evaluated_pattern$fixed
  }

  if (isTRUE(fixed)) {
    # we can have multiple patterns here
    new <- data[!names(data) %in% pattern]
  } else {
    new <- data[!grepl(pattern, names(data), ignore.case = ignore_case)]
  }
  attributes(new) <- utils::modifyList(attributes(data), attributes(new))
  class(new) <- class(data)
  new
}
