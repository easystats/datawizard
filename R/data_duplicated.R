#' @title Extract all duplicates
#'
#' @description Extract all duplicates, for visual inspection.
#' Note that it also contains the first occurrence of future
#' duplicates, unlike [duplicated()] or [dplyr::distinct()]). Also
#' contains an additional column reporting the number of missing
#' values for that row, to help in the decision-making when
#' selecting which duplicates to keep.
#'
#' @param data The data frame.
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
#'   - one of the following select-helpers: `starts_with()`, `ends_with()`,
#'     `contains()`, a range using `:` or `regex("")`. `starts_with()`,
#'     `ends_with()`, and  `contains()` accept several patterns, e.g
#'     `starts_with("Sep", "Petal")`.
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
#' @param verbose Toggle messages and warnings.
#' @keywords duplicates
#' @export
#' @seealso
#' [data_unique()]
#' @return A dataframe, containing all duplicates.
#' @examples
#' df1 <- data.frame(
#'    id = c(1, 2, 3, 1, 3),
#'    year = c(2022, 2022, 2022, 2022, 2000),
#'    item1 = c(NA, 1, 1, 2, 3),
#'    item2 = c(NA, 1, 1, 2, 3),
#'    item3 = c(NA, 1, 1, 2, 3)
#' )
#'
#' data_duplicated(df1, select = "id")
#'
#' data_duplicated(df1, select = c("id", "year"))
#'
#' # Filter to exclude duplicates
#' df2 <- df1[-c(1, 5),]
#' df2

data_duplicated <- function(data,
                            select = NULL,
                            exclude = NULL,
                            ignore_case = FALSE,
                            regex = FALSE,
                            verbose = TRUE) {
  UseMethod("data_duplicated")
}

#' @export
data_duplicated.data.frame <- function(data,
                                       select = NULL,
                                       exclude = NULL,
                                       ignore_case = FALSE,
                                       regex = FALSE,
                                       verbose = TRUE) {

  select <- .select_nse(
    select,
    data,
    exclude = exclude,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose
  )

  data$temporary_id <- do.call(paste, c(data_select(data, select), sep = "_"))

  data <- cbind(Row = seq_len(nrow(data)), data)
  dups.index <- data$temporary_id %in% data$temporary_id[duplicated(data$temporary_id)]
  dups <- data[dups.index, ]
  dups$count_na <- rowSums(is.na(dups))
  dups <- data_arrange(dups, "temporary_id")
  dups <- data_remove(dups, "temporary_id")
  dups

}

#' @export
data_duplicated.grouped_df <- function(data,
                                       select = NULL,
                                       exclude = NULL,
                                       ignore_case = FALSE,
                                       regex = FALSE,
                                       verbose = TRUE) {

  select <- .select_nse(select,
                        data,
                        exclude = exclude,
                        ignore_case = ignore_case,
                        regex = regex,
                        verbose = verbose
  )

  # works only for dplyr >= 0.8.0
  grps <- attr(data, "groups", exact = TRUE)
  grps <- grps[[".rows"]]

  out <- lapply(grps, function(x) {
    data_duplicated.data.frame(data[x, ], select = select)
  })

  out <- do.call(rbind, out)

  out
}
