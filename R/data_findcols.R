#' @title Find columns in a data frame based on search patterns
#' @name data_findcols
#'
#' @param data A data frame.
#' @param pattern Deprecated. Please use `select`.
#' @param starts_with,ends_with Deprecated. Please use select-helpers in `select`.
#' @param select Variables that will be included when performing the required
#'   tasks. Can be either
#'
#'   - a variable specified as a literal variable name (e.g., `column_name`),
#'   - a string with the variable name (e.g., `"column_name"`), or a character
#'     vector of variable names (e.g., `c("col1", "col2", "col3")`),
#'   - a formula with variable names (e.g., `~column_1 + column_2`),
#'   - a vector of positive integers, giving the positions counting from the left
#'     (e.g. `1` or `c(1, 3, 5)`),
#'   - a vector of negative integers, giving the positions counting from the
#'     right (e.g., `-1` or `-1:-3`),
#'   - or one of the following select-helpers: `starts_with("")`, `ends_with("")`,
#'   `contains("")`, a range using `:` or `regex("")`.
#' @param exclude See `select`, however, column names matched by the pattern
#'   from `exclude` will be excluded instead of selected.
#' @param ignore_case Logical, if `TRUE` and when one of the select-helpers or
#'   a regular expression is used in `select`, ignores lower/upper case in the
#'   search pattern when matching against variable names.
#' @param verbose Toggle warnings.
#'
#' @inherit data_rename seealso
#'
#' @return A character vector with column names that matched the pattern in
#'   `select` and `exclude`, or `NULL` if no matching column name was found.
#'
#' @examples
#' # Find columns names by pattern
#' data_findcols(iris, select = starts_with("Sepal"))
#' data_findcols(iris, select = ends_with("Width"))
#' data_findcols(iris, select = regex("\\."))
#' data_findcols(iris, select = c("Petal.Width", "Sepal.Length"))
#'
#' @export
data_findcols <- function(data,
                          pattern = NULL,
                          starts_with = NULL,
                          ends_with = NULL,
                          select = NULL,
                          exclude = NULL,
                          ignore_case = FALSE,
                          verbose = TRUE,
                          ...) {
  # TODO: Need to extend this to work with NSE so that the following shoud work:
  # - data_findcols(iris, Sepal.Length)
  # - data_findcols(iris, starts_with("Sepal"))
  # - data_findcols(iris, contains("Sepal"))

  if (!missing(select)) {
    columns <- .select_nse(select, data, exclude, ignore_case = ignore_case, verbose = FALSE)
    if (!length(columns) || is.null(columns)) {
      columns <- NULL
      if (isTRUE(verbose)) {
        warning(insight::format_message("No column names that matched the required find pattern were found."), call. = FALSE)
      }
    }
    return(columns)
  }

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
