#' @inheritParams data_extract
#' @rdname data_relocate
#' @examples
#' # Remove columns
#' head(data_remove(iris, "Sepal.Length"))
#' head(data_remove(iris, starts_with("Sepal")))
#' @export
data_remove <- function(data, pattern, ignore_case = FALSE, verbose = TRUE, ...) {

  fixed <- TRUE
  # avoid conflicts
  conflicting_packages <- .conflicting_packages("poorman")

  # in case pattern is a variable from another function call...
  p <- try(eval(pattern), silent = TRUE)
  if (inherits(p, c("try-error", "simpleError"))) {
    p <- substitute(pattern)
  }

  # check if pattern is a function like "starts_with()",
  # then override "fixed" argument
  pattern <- tryCatch(eval(p), error = function(e) NULL)

  if (is.null(pattern)) {
    evaluated_pattern <- .evaluate_pattern(.safe_deparse(p), data, ignore_case)
    pattern <- evaluated_pattern$pattern
    fixed <- evaluated_pattern$fixed
  }

  # load again
  .attach_packages(conflicting_packages)

  # seems to be no valid column name or index, so try to grep
  if (isFALSE(fixed)) {
    pattern <- colnames(data)[grepl(pattern, colnames(data), ignore.case = ignore_case)]
  }

  # check if colnames are in data
  if (!all(pattern %in% colnames(data))) {
    if (isTRUE(verbose)) {
      warning(insight::format_message(
        paste0("Following variable(s) were not found: ", paste0(setdiff(pattern, colnames(data)), collapse = ", "))
      ))
    }
    pattern <- intersect(pattern, colnames(data))
  }

  # nothing to remove?
  if (!length(pattern)) {
    return(data)
  }

  new <- data[!colnames(data) %in% pattern]
  attributes(new) <- utils::modifyList(attributes(data), attributes(new))
  class(new) <- class(data)

  new
}
