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
#' @param var Name of column to use for rownames.
#'
#' @return \code{rownames_as_column} and \code{column_as_rownames} both return a data frame.
#' @export
#'
#' @rdname rownames
#'
#' @examples
#' # Convert between row names and column --------------------------------
#' test <- rownames_as_column(mtcars, var = "car")
#' test
#' head(column_as_rownames(test, var = "car"))
rownames_as_column <- function(x, var = "rowname") {
  if (!object_has_rownames(x)) {
    stop("The dataframe doesn't have rownames.")
  }
  if (is.null(var)) {
    var <- "rowname"
  }
  if (!is.character(var)) {
    stop("Argument 'var' must be a string.")
  }
  rn <- data.frame(rn = rownames(x), stringsAsFactors = FALSE)
  x <- cbind(rn, x)
  colnames(x)[1] <- var
  rownames(x) <- NULL
  x
}

#' @rdname rownames
#' @export
#' @examples
column_as_rownames <- function(x, var = "rowname") {
  if (is.null(var)) {
    var <- "rowname"
  }
  if (!is.character(var)) {
    stop("Argument 'var' must be a string.")
  }
  if (!var %in% names(x)) {
    stop(paste0('Variable "', var, '" is not in the dataframe.'))
  }
  rownames(x) <- x[[var]]
  x[[var]] <- NULL
  x
}

