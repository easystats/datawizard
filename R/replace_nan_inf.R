#' @title Convert infinite or `NaN` values into `NA`
#' @name replace_nan_inf
#'
#' @description
#' Replaces all infinite (`Inf` and `-Inf`) or `NaN` values with `NA`.
#'
#' @param x A vector or a dataframe
#' @param ... Currently not used.
#'
#' @return
#' Data with `Inf`, `-Inf`, and `NaN` converted to `NA`.
#'
#' @examples
#' # a vector
#' x <- c(1, 2, NA, 3, NaN, 4, NA, 5, Inf, -Inf, 6, 7)
#' replace_nan_inf(x)
#'
#' # a data frame
#' df <- data.frame(
#'   x = c(1, NA, 5, Inf, 2, NA),
#'   y = c(3, NaN, 4, -Inf, 6, 7),
#'   stringsAsFactors = FALSE
#' )
#' replace_nan_inf(df)
#' @export

replace_nan_inf <- function(x, ...) {
  UseMethod("replace_nan_inf")
}

#' @export
replace_nan_inf.default <- function(x, ...) {
  x[is.nan(x) | is.infinite(x)] <- NA
  x
}

#' @inheritParams find_columns
#' @export
replace_nan_inf.data.frame <- function(x,
                                       select = NULL,
                                       exclude = NULL,
                                       ignore_case = FALSE,
                                       regex = FALSE,
                                       verbose = TRUE,
                                       ...) {
  # Select and deselect
  cols <- .select_nse(
    select,
    x,
    exclude = exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  for (i in cols) {
    x[[i]] <- replace_nan_inf(x[[i]])
  }

  x
}
