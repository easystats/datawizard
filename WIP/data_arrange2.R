#' Arrange rows by column values
#'
#' `data_arrange2()` orders the rows of a data frame by the values of selected
#' columns.
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @param descending Vector of logicals. Can be of length 1, or of same length
#'   as selected variables in `select`. Indicates for each variable whether it
#'   should be sorted in descending order or not. By default, if `descending = NULL`,
#'   all selected variables will be sorted in ascending order. If `descending`
#'   is of length one and `TRUE`, all variables will be sorted in descending
#'   order. Else, variables indicated with a `TRUE` will be sorted in descending
#'   order.
#' @inheritParams find_columns
#'
#' @return A data frame.
#'
#' @examples
#' # Arrange using several variables
#' data_arrange2(head(mtcars), select = c("gear", "carb"))
#'
#' # Arrange all in decreasing order
#' data_arrange2(head(mtcars), c("gear", "carb"), descending = TRUE)
#'
#' # Arrange 2nd in decreasing order
#' data_arrange2(head(mtcars), c("gear", "carb"), descending = c(FALSE, TRUE))
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

  # old code, from character-vector-approach
  # ascending <- .select_nse(ascending, data, descending, ignore_case)
  # descending <- setdiff(select, ascending)

  out <- data

  # reverse order for variables that should be decreasing
  if (!is.null(descending) && length(descending) > 0) {
    # must be a vector of logicals
    if (!all(is.logical(descending))) {
      insight::format_error("`descending` must a a vector of logicals, either of length 1 or of same length as variables selected by `select`.")
    }
    for (i in select[descending]) {
      out[[i]] <- -xtfrm(out[[i]])
    }

    # old code, from character-vector-approach
    # for (i in descending) {
    #   out[[i]] <- -xtfrm(out[[i]])
    # }
  }

  # apply ordering
  if (length(select) == 1) {
    data[order(out[[select]]), ]
  } else {
    data[do.call(order, out[, select]), ]
  }
}
