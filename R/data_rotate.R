#' @title Rotate a data frame
#' @name data_rotate
#'
#' @description
#' This function rotates a data frame, i.e. columns become rows and vice versa.
#'   It's the equivalent of using `t()` but restores the `data.frame` class,
#'   preserves attributes and prints a warning if the data type is
#'   modified (see example).
#'
#' @param data A data frame.
#' @param rownames Character vector (optional). If not `NULL`, the data frame's
#'   rownames will be added as (first) column to the output, with `rownames`
#'   being the name of this column.
#' @param colnames Logical or character vector (optional). If `TRUE`, the values
#'   of the first column in `x` will be used as column names in the rotated data
#'   frame. If a character vector, values from that column are used as column
#'   names.
#' @param verbose Toggle warnings.
#'
#' @return A (rotated) data frame.
#'
#' @examples
#' x <- mtcars[1:3, 1:4]
#'
#' x
#'
#' data_rotate(x)
#' data_rotate(x, rownames = "property")
#'
#' # use values in 1. column as column name
#' data_rotate(x, colnames = TRUE)
#' data_rotate(x, rownames = "property", colnames = TRUE)
#'
#' # warn that data types are changed
#' str(data_rotate(iris[1:4, ]))
#'
#' # use either first column or specific column for column names
#' x <- data.frame(a = 1:5, b = 11:15, c = letters[1:5], d = rnorm(5))
#' data_rotate(x, colnames = TRUE)
#' data_rotate(x, colnames = "c")
#'
#' @inherit data_rename seealso
#' @export
data_rotate <- function(data, rownames = NULL, colnames = FALSE, verbose = TRUE) {
  # copy attributes
  attr_data <- attributes(data)

  # check if first column has column names to be used for rotated data
  if (isTRUE(colnames)) {
    colnames <- data[[1]]
    data <- data[-1]
  } else if (!is.null(colnames) && is.character(colnames) && colnames %in% colnames(data)) {
    cn_col <- which(colnames(data) == colnames)
    colnames <- data[[colnames]]
    data <- data[-cn_col]
  } else {
    colnames <- row.names(data)
  }

  # warning after possible removal of columns
  if (length(unique(sapply(data, class))) > 1) {
    if (verbose) {
      insight::format_warning("Your data frame contains mixed types of data. After transposition, all variables will be transformed into characters.")
    }
  }

  # rotate data frame by 90 degrees
  out <- as.data.frame(t(as.data.frame(data)))

  # add column names, if requested
  if (!is.null(colnames)) {
    # check if we have correct length of column names
    if (length(colnames) != ncol(out)) {
      insight::format_warning(
        "Length of provided column names does not match number of columns. No column names changed."
      )
    } else {
      colnames(out) <- colnames
    }
  }

  # add rownames as a new column, if requested
  if (!is.null(rownames)) out <- rownames_as_column(out, var = rownames)

  out <- .replace_attrs(out, attr_data)

  out
}


#' @rdname data_rotate
#' @export
data_transpose <- data_rotate
