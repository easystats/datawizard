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


#' Tools for working with column names
#'
#' @param x A data frame.
#' @param row Row to use as column names.
#' @param na_prefix Prefix to give to the column name if the row has an `NA`.
#' Default is 'x', and it will be incremented at each `NA` (`x1`, `x2`, etc.).
#' @param verbose Toggle warnings.
#' @param prefix Prefix to give to the column name. Default is 'x', and it will
#' be incremented at each column (`x1`, `x2`, etc.).
#'
#' @return
#' `row_to_colnames()` and `colnames_to_row()` both return a data frame.
#'
#' @rdname colnames
#'
#' @export
#'
#' @examples
#' # Convert a row to column names --------------------------------
#' test <- data.frame(
#'   a = c("iso", 2, 5),
#'   b = c("year", 3, 6),
#'   c = c("value", 5, 7)
#' )
#' test
#' row_to_colnames(test)
#'
#' # Convert column names to row --------------------------------
#' test <- data.frame(
#'   ARG = c("BRA", "FRA"),
#'   `1960` = c(1960, 1960),
#'   `2000` = c(2000, 2000)
#' )
#' test
#' colnames_to_row(test)
#'
row_to_colnames <- function(x, row = 1, na_prefix = "x", verbose = TRUE) {
  if (!is.numeric(row)) {
    insight::format_error("Argument `row` must be of type numeric.")
  }
  if (length(row) != 1) {
    insight::format_error("Argument `row` must be of length 1.")
  }
  if (nrow(x) < row) {
    insight::format_error(
      paste0(
        "You used row = ", row,
        " but the dataset only has ", nrow(x), " rows."
      )
    )
  }

  new_colnames <- as.character(unlist(x[row, ], use.names = FALSE))

  # Create default colnames if there are NAs in the row used
  which_na <- which(is.na(new_colnames))
  n_na <- length(which_na)
  if (n_na > 0) {
    for (i in seq_along(which_na)) {
      new_colnames[which_na[i]] <- paste0(na_prefix, i)
    }
    if (verbose) {
      insight::format_warning(
        paste0(
          "Some values of row ", row,
          " were NAs. The corresponding column names are prefixed with `",
          na_prefix, "`."
        )
      )
    }
  }
  colnames(x) <- new_colnames
  x[-row, ]
}




#' @rdname colnames
#' @export
colnames_to_row <- function(x, prefix = "x") {
  if (length(prefix) != 1) {
    insight::format_error("Argument `prefix` must be of length 1.")
  }
  if (!is.character(prefix)) {
    insight::format_error("Argument `prefix` must be of type character.")
  }
  x2 <- rbind(colnames(x), x)
  colnames(x2) <- paste0(prefix, seq_len(ncol(x2)))
  x2
}
