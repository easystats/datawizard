#' @title Find or get columns in a data frame based on search patterns
#' @name find_columns
#'
#' @description `find_columns()` returns column names from a data set that
#' match a certain search pattern, while `get_columns()` returns the found data.
#' `data_select()` is an alias for `get_columns()`, and `data_find()` is an alias
#' for `find_columns()`.
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
#'   - one of the following select-helpers: `starts_with("")`, `ends_with("")`,
#'     `contains("")`, a range using `:` or `regex("")`,
#'   - or a function testing for logical conditions, e.g. `is.numeric()` (or
#'     `is.numeric`), or any user-defined function that selects the variables
#'     for which the function returns `TRUE` (like: `foo <- function(x) mean(x) > 3`),
#'   - ranges specified via literal variable names, select-helpers (except
#'     `regex()`) and (user-defined) functions can be negated, i.e. return
#'     non-matching elements, when prefixed with a `-`, e.g. `-ends_with("")`,
#'     `-is.numeric` or `-Sepal.Width:Petal.Length`. **Note:** Negation means
#'     that matches are _excluded_, and thus, the `exclude` argument can be
#'     used alternatively. For instance, `select=-ends_with("Length")` (with
#'     `-`) is equivalent to `exclude=ends_with("Length")` (no `-`). In case
#'     negation should not work as expected, use the `exclude` argument instead.
#'
#'   If `NULL`, selects all columns. Patterns that found no matches are silently
#'   ignored, e.g. `find_columns(iris, select = c("Species", "Test"))` will just
#'   return `"Species"`.
#' @param exclude See `select`, however, column names matched by the pattern
#'   from `exclude` will be excluded instead of selected. If `NULL` (the default),
#'   excludes no columns.
#' @param ignore_case Logical, if `TRUE` and when one of the select-helpers or
#'   a regular expression is used in `select`, ignores lower/upper case in the
#'   search pattern when matching against variable names.
#' @param regex Logical, if `TRUE`, the search pattern from `select` will be
#'   treated as regular expression. When `regex = TRUE`, select *must* be a
#'   character string (or a variable containing a character string) and is not
#'   allowed to be one of the supported select-helpers or a character vector
#'   of length > 1. `regex = TRUE` is comparable to using one of the two
#'   select-helpers, `select = contains("")` or `select = regex("")`, however,
#'   since the select-helpers may not work when called from inside other
#'   functions (see 'Details'), this argument may be used as workaround.
#' @param verbose Toggle warnings.
#' @param ... Arguments passed down to other functions. Mostly not used yet.
#'
#' @inherit data_rename seealso
#'
#' @return
#'
#' `find_columns()` returns a character vector with column names that matched
#' the pattern in `select` and `exclude`, or `NULL` if no matching column name
#' was found. `get_columns()` returns a data frame with matching columns.
#'
#' @details
#'
#' Note that there are some limitations when calling this from inside other
#' functions. The following will work as expected, returning all columns that
#' start with `"Sep"`:
#'
#' ```r
#' foo <- function(data) {
#'   find_columns(data, select = starts_with("Sep"))
#' }
#' foo(iris)
#' ```
#'
#' However, this example won't work as expected!
#'
#' ```r
#' foo <- function(data) {
#'   i <- "Sep"
#'   find_columns(data, select = starts_with(i))
#' }
#' foo(iris)
#' ```
#'
#' One workaround is to use the `regex` argument, which provides at least a bit
#' more flexibility than exact matching. `regex` in its basic usage (as seen
#' below) means that `select` behaves like the `contains("")` select-helper, but
#' can also make the function more flexible by allowing to define complex
#' regular expression pattern in `select`.
#'
#' ```r
#' foo <- function(data) {
#'   i <- "Sep"
#'   find_columns(data, select = i, regex = TRUE)
#' }
#' foo(iris)
#' ```
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
#'
#' # find numeric with mean > 3.5
#' numeric_mean_35 <- function(x) is.numeric(x) && mean(x, na.rm = TRUE) > 3.5
#' find_columns(iris, numeric_mean_35)
#' @export
find_columns <- function(data,
                         select = NULL,
                         exclude = NULL,
                         ignore_case = FALSE,
                         regex = FALSE,
                         verbose = TRUE,
                         ...) {
  columns <- .select_nse(
    select,
    data,
    exclude,
    ignore_case = ignore_case,
    regex = regex,
    verbose = FALSE
  )

  if (!length(columns) || is.null(columns)) {
    columns <- NULL
    if (isTRUE(verbose)) {
      warning(insight::format_message(
        "No column names that matched the required search pattern were found."
      ), call. = FALSE)
    }
  }

  columns
}


#' @rdname find_columns
#' @export
data_find <- find_columns