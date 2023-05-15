#' Tools for working with row names or row ids
#'
#' @param x A data frame.
#' @param var Name of column to use for row names/ids. For `column_as_rownames()`,
#'   this argument can be the variable name or the column number.
#'
#' @return
#' A data frame.
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
  if (!insight::object_has_rownames(x)) {
    insight::format_error("The data frame doesn't have rownames.")
  }
  if (is.null(var)) {
    var <- "rowname"
  }
  if (!is.character(var)) {
    insight::format_error("Argument 'var' must be of type character.")
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
  if (!is.character(var) && !is.numeric(var)) {
    insight::format_error("Argument `var` must be of type character or numeric.")
  }
  if (is.character(var) && !var %in% names(x)) {
    insight::format_error(paste0("Variable \"", var, "\" is not in the data frame."))
  }
  if (is.numeric(var) && (var > ncol(x) || var <= 0)) {
    insight::format_error("Column ", var, " does not exist. There are ", ncol(x), " columns in the data frame.")
  }
  rownames(x) <- x[[var]]
  x[[var]] <- NULL
  x
}



#' @rdname rownames
#' @export
#' @examples
#' test_data <- head(iris)
#'
#' rowid_as_column(test_data)
#' rowid_as_column(test_data, var = "my_id")

rowid_as_column <- function(x, var = "rowid") {
  if (is.null(var)) {
    var <- "rowid"
  }
  if (!is.character(var)) {
    insight::format_error("Argument 'var' must be of type character.")
  }
  rn <- data.frame(rn = seq_len(nrow(x)), stringsAsFactors = FALSE)
  x <- cbind(rn, x)
  colnames(x)[1] <- var
  rownames(x) <- NULL
  x
}
