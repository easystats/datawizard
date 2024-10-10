#' @title Count specific values row-wise
#' @name row_count
#' @description `row_count()` mimics base R's `rowSums()`, with sums for a
#' specific value indicated by `count`. Hence, it is equivalent to
#' `rowSums(x == count, na.rm = TRUE)`, but offers some more options, including
#' strict comparisons. Comparisons using `==` coerce values to atomic vectors,
#' thus both `2 == 2` and `"2" == 2` are `TRUE`. In `row_count()`, it is also
#' possible to make "type safe" comparisons using the `allow_coercion` argument,
#' where `"2" == 2` is not true.
#'
#' @param data A data frame with at least two columns, where number of specific
#' values are counted row-wise.
#' @param count The value for which the row sum should be computed. May be a
#' numeric value, a character string (for factors or character vectors), `NA` or
#' `Inf`.
#' @param allow_coercion Logical. If `TRUE`, `count` matches only values of same
#' type (i.e. when `count = 2`, the value `"2"` is not counted and vice versa).
#' By default, when `allow_coercion = FALSE`, `count = 2` also matches `"2"`.
#' See 'Examples'.
#'
#' @inheritParams extract_column_names
#' @inheritParams row_means
#'
#' @return A vector with row-wise counts of values specified in `count`.
#'
#' @examples
#' dat <- data.frame(
#'   c1 = c(1, 2, NA, 4),
#'   c2 = c(NA, 2, NA, 5),
#'   c3 = c(NA, 4, NA, NA),
#'   c4 = c(2, 3, 7, 8)
#' )
#'
#' # count all 2s per row
#' row_count(dat, count = 4)
#' # count all missing values per row
#' row_count(dat, count = NA)
#'
#' dat <- data.frame(
#'   c1 = c("1", "2", NA, "3"),
#'   c2 = c(NA, "2", NA, "3"),
#'   c3 = c(NA, 4, NA, NA),
#'   c4 = c(2, 3, 7, Inf)
#' )
#' # count all 2s and "2"s per row
#' row_count(dat, count = 2)
#' # only count 2s, but not "2"s
#' row_count(dat, count = 2, allow_coercion = TRUE)
#'
#' @export
row_count <- function(data,
                      select = NULL,
                      exclude = NULL,
                      count = NULL,
                      allow_coercion = TRUE,
                      ignore_case = FALSE,
                      regex = FALSE,
                      verbose = TRUE) {
  # evaluate arguments
  select <- .select_nse(select,
    data,
    exclude,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose
  )

  if (is.null(count)) {
    insight::format_error("`count` must be a valid value (including `NA` or `Inf`), but not `NULL`.")
  }

  if (is.null(select) || length(select) == 0) {
    insight::format_error("No columns selected.")
  }

  data <- .coerce_to_dataframe(data[select])

  # check if we have a data framme with at least two columns
  if (nrow(data) < 1) {
    insight::format_error("`data` must be a data frame with at least one row.")
  }

  # check if we have a data framme with at least two columns
  if (ncol(data) < 2) {
    insight::format_error("`data` must be a data frame with at least two numeric columns.")
  }

  # special case: count missing
  if (is.na(count)) {
    rowSums(is.na(data))
  } else {
    # comparisons in R using == coerce values into a atomic vector, i.e.
    # 2 == "2" is TRUE. If `allow_coercion = FALSE`, we only want 2 == 2 or
    # "2" == "2". to achieve this, we simply compute the comparison on numeric
    # or non-numeric columns only
    if (isFALSE(allow_coercion)) {
      numeric_columns <- vapply(data, is.numeric, TRUE)
      if (is.numeric(count)) {
        data <- data[numeric_columns]
      } else {
        data <- data[!numeric_columns]
      }
    }
    rowSums(data == count, na.rm = TRUE)
  }
}
