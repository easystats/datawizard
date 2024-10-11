#' @title Count specific values row-wise
#' @name row_count
#' @description `row_count()` mimics base R's `rowSums()`, with sums for a
#' specific value indicated by `count`. Hence, it is similar to
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
#' @param allow_coercion Logical. If `FALSE`, `count` matches only values of same
#' class (i.e. when `count = 2`, the value `"2"` is not counted and vice versa).
#' By default, when `allow_coercion = TRUE`, `count = 2` also matches `"2"`. In
#' order to count factor levels in the data, use `count = factor("level")`. See
#' 'Examples'.
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
#' # count all 4s per row
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
#' row_count(dat, count = 2, allow_coercion = FALSE)
#'
#' dat <- data.frame(
#'   c1 = factor(c("1", "2", NA, "3")),
#'   c2 = c("2", "1", NA, "3"),
#'   c3 = c(NA, 4, NA, NA),
#'   c4 = c(2, 3, 7, Inf)
#' )
#' # find only character "2"s
#' row_count(dat, count = "2", allow_coercion = FALSE)
#' # find only factor level "2"s
#' row_count(dat, count = factor("2"), allow_coercion = FALSE)
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
    # "2" == "2" (i.e. we want exact types to be compared only)
    if (isFALSE(allow_coercion)) {
      # we need the "type" of the count-value - we use class() instead of typeof(),
      # because the latter sometimes returns unsuitable classes/types. compare
      # typeof(as.Date("2020-01-01")), which returns "double".
      count_type <- class(count)[1]
      valid_columns <- vapply(data, inherits, TRUE, what = count_type)
      # check if any columns left?
      if (!any(valid_columns)) {
        insight::format_error("No column has same type as the value provided in `count`. Set `allow_coercion = TRUE` or specify a valid value for `count`.") # nolint
      }
      data <- data[valid_columns]
    }
    # coerce - we have only valid columns anyway, and we need to coerce factors
    # to vectors, else comparison with `==` errors.
    count <- as.vector(count)
    # finally, count
    rowSums(data == count, na.rm = TRUE)
  }
}
