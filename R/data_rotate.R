#' @title Rotate a dataframe
#' @name data_rotate
#'
#' @description
#' This function rotates a dataframe, i.e. columns become rows and vice versa.
#'
#' @param data A dataframe.
#' @param rownames Character vector (optional). If not `NULL`, the dataframe's
#'   rownames will be added as (first) column to the output, with `rownames`
#'   being the name of this column.
#' @param colnames Logical (optional), if `TRUE`, the values of the first column
#'   in `x` will be used as column names in the rotated dataframe.
#' @param verbose Toggle warnings.
#'
#' @return A (rotated) dataframe.
#'
#' @examples
#' x <- mtcars[1:3, 1:4]
#' data_rotate(x)
#' data_rotate(x, rownames = "property")
#'
#' # use values in 1. column as column name
#' data_rotate(x, colnames = TRUE)
#' data_rotate(x, rownames = "property", colnames = TRUE)
#' @export
data_rotate <- function(data, rownames = NULL, colnames = FALSE, verbose = TRUE) {
  if (length(unique(sapply(data, class))) > 1) {
    if (verbose) {
      warning(insight::format_message("Your data frame contains mixed types of data. After transposition, all will be transformed into characters."), call. = FALSE)
    }
  }

  # check if first column has column names to be used for rotated data
  if (colnames) {
    colnames <- data[[1]]
    data <- data[-1]
  } else {
    colnames <- NULL
  }

  # copy attributes
  a <- attributes(data)

  # rotate dataframe by 90 degrees
  data <- as.data.frame(t(as.data.frame(data)))

  # add column names, if requested
  if (!is.null(colnames)) {
    # check if we have correct length of column names
    if (length(colnames) != ncol(data)) {
      warning(
        "Length of provided column names does not match number of columns. No column names changed.",
        call. = FALSE
      )
    } else {
      colnames(data) <- colnames
    }
  }

  # add rownames as a new column, if requested
  if (!is.null(rownames)) data <- rownames_as_column(data, var = rownames)

  # delete the common attributes
  a[names(a) %in% names(attributes(data))] <- NULL

  # and then add other attributes to the dataframe
  attributes(data) <- utils::modifyList(attributes(data), a)

  data
}
