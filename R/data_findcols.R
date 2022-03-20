#' @title Find columns in a data frame based on search patterns
#' @name find_columns
#'
#' @param data A data frame.
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
#' @param ... Arguments passed down to other functions. Mostly not used yet.
#'
#' @inherit data_rename seealso
#'
#' @return A character vector with column names that matched the pattern in
#'   `select` and `exclude`, or `NULL` if no matching column name was found.
#'
#' @examples
#' # Find columns names by pattern
#' find_columns(iris, starts_with("Sepal"))
#' find_columns(iris, ends_with("Width"))
#' find_columns(iris, regex("\\."))
#' find_columns(iris, c("Petal.Width", "Sepal.Length"))
#'
#' # starts with "Sepal", but not allowed to end with "width"
#' find_columns(iris, starts_with("Sepal"), exclude = contains("Width"))
#' @export
find_columns <- function(data,
                         select = NULL,
                         exclude = NULL,
                         ignore_case = FALSE,
                         verbose = TRUE,
                         ...) {
  columns <- .select_nse(select, data, exclude, ignore_case = ignore_case, verbose = FALSE)

  if (!length(columns) || is.null(columns)) {
    columns <- NULL
    if (isTRUE(verbose)) {
      warning(insight::format_message("No column names that matched the required search pattern were found."), call. = FALSE)
    }
  }

  columns
}


#' @export
data_findcols <- function(data,
                          pattern = NULL,
                          starts_with = NULL,
                          ends_with = NULL,
                          ignore_case = FALSE,
                          ...) {

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
