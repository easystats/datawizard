#' @param pattern A regular expression (as character string), representing the
#'   pattern to be matched in the in column names. Can also be one of the
#'   following select-helpers: `starts_with("")`, `end_with("")`, `regex("")`,
#'   `contains("")`, or a range using `:`.
#' @param starts_with,ends_with Character string, containing the string to be
#'   matched in the column names. `starts_with` finds matches at the beginning
#'   of column names, `ends_with` finds matches at the end of column names.
#' @inheritParams data_extract
#' @rdname data_relocate
#' @examples
#' # Find columns names by pattern
#' data_findcols(iris, starts_with = "Sepal")
#' data_findcols(iris, ends_with = "Width")
#' data_findcols(iris, pattern = "\\.")
#' data_findcols(iris, c("Petal.Width", "Sepal.Length"))
#'
#' # using select helpers
#' data_findcols(iris, starts_with("Sepal"))
#' data_findcols(iris, ends_with("Width"))
#'
#' @export
data_findcols <- function(data,
                          pattern = NULL,
                          starts_with = NULL,
                          ends_with = NULL,
                          ignore_case = FALSE,
                          ...) {
  # TODO: Need to extend this to work with NSE so that the following shoud work:
  # - data_findcols(iris, Sepal.Length)
  # - data_findcols(iris, starts_with("Sepal"))
  # - data_findcols(iris, contains("Sepal"))


  # init
  n <- names(data)
  match <- c()

  # avoid conflicts
  conflicting_packages <- .conflicting_packages("poorman")

  # in case pattern is a variable from another function call...
  p <- try(eval(pattern), silent = TRUE)
  if (inherits(p, c("try-error", "simpleError"))) {
    p <- substitute(pattern)
  }

  # evaluate pattern, can be function like "starts_with()"
  pattern <- tryCatch(
    {
      eval(p)
    },
    error = function(e) {
      .evaluate_pattern(insight::safe_deparse(p))$pattern
    }
  )

  # load again
  .attach_packages(conflicting_packages)

  if (!is.null(pattern)) {
    for (i in pattern) {
      match <- c(match, n[grepl(i, n, ignore.case = ignore_case)])
    }
  }
  if (!is.null(starts_with)) {
    match <- c(match, n[grepl(paste0("^", starts_with), n, ignore.case = ignore_case)])
  }
  if (!is.null(ends_with)) {
    match <- c(match, n[grepl(paste0(ends_with, "$"), n, ignore.case = ignore_case)])
  }
  match
}
