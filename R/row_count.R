#' @title Row means or sums (optionally with minimum amount of valid values)
#' @name row_count
#' @description `row_count()` mimics base R's `rowSums()`, with sums for a
#' specific value indicated by `count`. Hence, it is equivalent to
#' `rowSums(x == count, na.rm = TRUE)`.
#'
#' @param data A data frame with at least two columns, where number of specific
#' values are counted row-wise.
#' @param count The value for which the row sum should be computed. May be a
#' numeric value, a character string (for factors or character vectors), `NA` or
#' `Inf`.
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
#' row_count(dat, count = 2)
#' # count all missing values per row
#' row_count(dat, count = NA)
#'
#' @export
row_count <- function(data,
                      select = NULL,
                      exclude = NULL,
                      count = NULL,
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
  if (ncol(data) < 2) {
    insight::format_error("`data` must be a data frame with at least two numeric columns.")
  }

  # special case: count missing
  if (is.na(count)) {
    rowSums(is.na(data))
  } else {
    rowSums(data == count, na.rm = TRUE)
  }
}
