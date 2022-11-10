#' @title Keep only one row from all with duplicated IDs
#'
#' @description From all rows with at least one duplicated ID,
#' keep only one. Methods for selecting the duplicated row are
#' either the first duplicate, the last duplicate, or the "best"
#' duplicate (default), based on the duplicate with the smallest
#' number of `NA`. In case of ties, it picks the first
#' duplicate, as it is the one most likely to be valid and
#' authentic, given practice effects.
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
#' @param keep The method to be used for duplicate selection, either "best"
#'   (the default), "first", or "last".
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
#' @return A dataframe, containing only the chosen duplicates.
#' @seealso
#' [data_duplicated()]
#' @export
#' @examples
#' df1 <- data.frame(
#'    id = c(1, 2, 3, 1, 3),
#'    item1 = c(NA, 1, 1, 2, 3),
#'    item2 = c(NA, 1, 1, 2, 3),
#'    item3 = c(NA, 1, 1, 2, 3)
#' )
#'
#' data_unique(df1, select = "id")

data_unique <- function(data,
                        select = NULL,
                        keep = "best",
                        exclude = NULL,
                        ignore_case = FALSE,
                        regex = FALSE,
                        verbose = TRUE) {
  UseMethod("data_unique")
}


#' @export
data_unique.data.frame <- function(data,
                                   select = NULL,
                                   keep = "best",
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

  # temporary_id <- paste0(sample(letters), collapse = "")
  data$temporary_id2 <- do.call(paste, c(data_select(data, select), sep = "_"))

  og.names <- names(data)
  dups <- data_duplicated(data, "temporary_id2")

  # if no duplicates, return the original data
  if (nrow(dups) == 0L) {
    data <- data_remove(data, "temporary_id2")
    return(data)
  }

  # count number of duplicates
  dups.n <- sum(duplicated(dups$temporary_id2))
  good.dups <- data_group(dups, "temporary_id2")

  # keep row that has the least duplicates
  if (keep == "best") {
    good.dups <- data_filter(good.dups, "count_na == min(count_na)")
  }

  good.dups <- good.dups[!duplicated(good.dups$temporary_id2,
                                     fromLast = keep == "last"), ]

  good.dups <- data_select(good.dups, og.names)
  out <- data[!duplicated(data$temporary_id2), ]
  match.index <- out$temporary_id2 %in% good.dups$temporary_id2
  out[match.index, ] <- good.dups

  # id is not useful anymore
  out <- data_remove(out, "temporary_id2")

  if (verbose) {
    dup.msg <- sprintf("(%s duplicates removed, with method '%s')", dups.n, keep)
    dup.msg <- paste0(dup.msg, ifelse(dups.n != 69, "", " 69... nice"))
    insight::format_alert(dup.msg)
  }

  out

}


#' @export
data_unique.grouped_df <- function(data,
                                   select = NULL,
                                   keep = "best",
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

  # works only for dplyr >= 0.8.0
  grps <- attr(data, "groups", exact = TRUE)
  grps <- grps[[".rows"]]

  data2 <- data_ungroup(data)

  out <- lapply(grps, function(x) {
    data_unique.data.frame(data2[x, ], select = select, keep = keep, verbose = verbose)
  })

  out <- do.call(rbind, out)

  class(out) <- class(data)
  attr(out, "groups") <- attr(data, "groups")

  out
}
