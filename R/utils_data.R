#' help-functions
#' @keywords internal
#' @noRd
.data_frame <- function(...) {
  x <- data.frame(..., stringsAsFactors = FALSE)
  rownames(x) <- NULL
  x
}

#' Tools for working with row names
#'
#' @param x A data frame.
#' @param var Name of column to use for rownames. For `column_as_rownames()`,
#'   this argument can be the variable name or the column number.
#'
#' @return
#' `rownames_as_column()` and `column_as_rownames()` both return a data frame.
#'
#' @rdname rownames
#'
#' @examples
#' # Convert between row names and column --------------------------------
#' test <- rownames_as_column(mtcars, var = "car")
#' test
#' head(column_as_rownames(test, var = "car"))
#'
#' @export
rownames_as_column <- function(x, var = "rowname") {
  if (!object_has_rownames(x)) {
    stop("The dataframe doesn't have rownames.")
  }
  if (is.null(var)) {
    var <- "rowname"
  }
  if (!is.character(var)) {
    stop("Argument 'var' must be of type character.")
  }
  rn <- data.frame(rn = rownames(x), stringsAsFactors = FALSE)
  x <- cbind(rn, x)
  colnames(x)[1] <- var
  rownames(x) <- NULL
  x
}

#' @rdname rownames
#' @export
column_as_rownames <- function(x, var = "rowname") {
  if (!is.character(var) & !is.numeric(var)) {
    stop("Argument 'var' must be of type character or numeric.")
  }
  if (is.character(var)) {
    if (!var %in% names(x)) {
      stop(paste0('Variable "', var, '" is not in the dataframe.'))
    }
  }
  if (is.numeric(var)) {
    if (var > ncol(x) | var <= 0) {
      stop("Column ", var, " does not exist. There are ", ncol(x), " columns in the dataframe.")
    }
  }
  rownames(x) <- x[[var]]
  x[[var]] <- NULL
  x
}
