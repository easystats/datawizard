#' Arrange rows by column values
#'
#' `data_arrange2()` orders the rows of a data frame by the values of selected
#' columns.
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @param ... Quoted column names. Use a dash just before column name to arrange
#'   in decreasing order, for example "-x1".
#' @param safe Do not throw an error if one of the variables specified doesn't
#'   exist.
#'
#' @return A data frame.
#'
#' @examples
#' # Arrange using several variables
#' data_arrange(head(mtcars), "gear", "carb")
#'
#' # Arrange in decreasing order
#' data_arrange(head(mtcars), "-carb")
#'
#' # Throw an error if one of the variables specified doesn't exist
#' data_arrange(head(mtcars), "gear", "foo", safe = FALSE)
#'
#' @export

data_arrange2 <- function(data,
                          select = NULL,
                          exclude = NULL,
                          descending = NULL,
                          ignore_case = FALSE,
                          ...) {

  # evaluate arguments
  select <- .select_nse(select, data, exclude, ignore_case)
  out <- data

  # reverse order for variables that should be decreasing
  if (!is.null(descending) && length(descending) > 0) {
    # must be a vector of logicals
    if (!all(is.logical(descending))) {
      stop(insight::format_message("`descending` must a a vector of logicals, either of length 1 or of same length as `select`."), call. = FALSE)
    }
    for (i in select[descending]) {
      out[[i]] <- -xtfrm(out[[i]])
    }
  }

  # apply ordering
  if (length(select) == 1) {
    data[order(out[[select]]), ]
  } else {
    data[do.call(order, out[, select]), ]
  }
}
