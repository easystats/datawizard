#' @title Row means (optionally with minimum amount of valid values)
#' @name row_means
#' @description This function is similar to the SPSS `MEAN.n` function and computes
#' row means from a data frame or matrix if at least `min_valid` values of a row are
#' valid (and not `NA`).
#'
#' @param data A data frame with at least two columns, where row means are applied.
#' @param min_valid Optional, a numeric value of length 1. May either be
#' - a numeric value that indicates the amount of valid values per row to
#'   calculate the row mean;
#' - or a value between `0` and `1`, indicating a proportion of valid values per
#'   row to calculate the row mean (see 'Details').
#' - `NULL` (default), in which all cases are considered.
#'
#' If a row's sum of valid values is less than `min_valid`, `NA` will be returned.
#' @param digits Numeric value indicating the number of decimal places to be
#' used for rounding mean values. Negative values are allowed (see 'Details').
#' By default, `digits = NULL` and no rounding is used.
#' @param remove_na Logical, if `TRUE` (default), removes missing (`NA`) values
#' before calculating row means. Only applies if `min_valuid` is not specified.
#' @param verbose Toggle warnings.
#' @inheritParams extract_column_names
#'
#' @return A vector with row means for those rows with at least `n` valid values.
#'
#' @details Rounding to a negative number of `digits` means rounding to a power of
#' ten, for example `row_means(df, 3, digits = -2)` rounds to the nearest hundred.
#' For `min_valid`, if not `NULL`, `min_valid` must be a numeric value from `0`
#' to `ncol(data)`. If a row in the data frame has at least `min_valid`
#' non-missing values, the row mean is returned. If `min_valid` is a non-integer
#' value from 0 to 1, `min_valid` is considered to indicate the proportion of
#' required non-missing values per row. E.g., if `min_valid = 0.75`, a row must
#' have at least `ncol(data) * min_valid` non-missing values for the row mean
#' to be calculated. See 'Examples'.
#'
#' @examples
#' dat <- data.frame(
#'   c1 = c(1, 2, NA, 4),
#'   c2 = c(NA, 2, NA, 5),
#'   c3 = c(NA, 4, NA, NA),
#'   c4 = c(2, 3, 7, 8)
#' )
#'
#' # default, all means are shown, if no NA values are present
#' row_means(dat)
#'
#' # remove all NA before computing row means
#' row_means(dat, remove_na = TRUE)
#'
#' # needs at least 4 non-missing values per row
#' row_means(dat, min_valid = 4) # 1 valid return value
#'
#' # needs at least 3 non-missing values per row
#' row_means(dat, min_valid = 3) # 2 valid return values
#'
#' # needs at least 2 non-missing values per row
#' row_means(dat, min_valid = 2)
#'
#' # needs at least 1 non-missing value per row, for two selected variables
#' row_means(dat, select = c("c1", "c3"), min_valid = 1)
#'
#' # needs at least 50% of non-missing values per row
#' row_means(dat, min_valid = 0.5) # 3 valid return values
#'
#' # needs at least 75% of non-missing values per row
#' row_means(dat, min_valid = 0.75) # 2 valid return values
#'
#' @export
row_means <- function(data,
                      select = NULL,
                      exclude = NULL,
                      min_valid = NULL,
                      digits = NULL,
                      ignore_case = FALSE,
                      regex = FALSE,
                      remove_na = FALSE,
                      verbose = TRUE) {
  # evaluate arguments
  select <- .select_nse(select,
    data,
    exclude,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose
  )

  if (is.null(select) || length(select) == 0) {
    insight::format_error("No columns selected.")
  }

  data <- .coerce_to_dataframe(data[select])

  # n must be a numeric, non-missing value
  if (!is.null(min_valid) && (all(is.na(min_valid)) || !is.numeric(min_valid) || length(min_valid) > 1)) {
    insight::format_error("`min_valid` must be a numeric value of length 1.")
  }

  # make sure we only have numeric values
  numeric_columns <- vapply(data, is.numeric, TRUE)
  if (!all(numeric_columns)) {
    if (verbose) {
      insight::format_alert("Only numeric columns are considered for calculation.")
    }
    data <- data[numeric_columns]
  }

  # check if we have a data framme with at least two columns
  if (ncol(data) < 2) {
    insight::format_error("`data` must be a data frame with at least two numeric columns.")
  }

  # proceed here if min_valid is not NULL
  if (is.null(min_valid)) {
    out <- rowMeans(data, na.rm = remove_na)
  } else {
    # is 'min_valid' indicating a proportion?
    decimals <- min_valid %% 1
    if (decimals != 0) {
      min_valid <- round(ncol(data) * decimals)
    }

    # min_valid may not be larger as df's amount of columns
    if (ncol(data) < min_valid) {
      insight::format_error("`min_valid` must be smaller or equal to number of columns in data frame.")
    }

    # row means
    to_na <- rowSums(is.na(data)) > ncol(data) - min_valid
    out <- rowMeans(data, na.rm = TRUE)
    out[to_na] <- NA
  }

  # round, if requested
  if (!is.null(digits) && !all(is.na(digits))) {
    out <- round(out, digits = digits)
  }

  out
}
