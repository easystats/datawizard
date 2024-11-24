#' Tools for working with row names or row ids
#'
#' @param x A data frame.
#' @param var Name of column to use for row names/ids. For `column_as_rownames()`,
#'   this argument can be the variable name or the column number. For
#'   `rownames_as_column()` and `rowid_as_column()`, the column name must not
#'   already exist in the data.
#'
#' @details
#' These are similar to `tibble`'s functions `column_to_rownames()`,
#' `rownames_to_column()` and `rowid_to_column()`. Note that the behavior of
#' `rowid_as_column()` is different for grouped dataframe: instead of making
#' the rowid unique across the full dataframe, it creates rowid per group.
#' Therefore, there can be several rows with the same rowid if they belong to
#' different groups.
#'
#' If you are familiar with `dplyr`, this is similar to doing the following:
#' ```r
#' data |>
#'   group_by(grp) |>
#'   mutate(id = row_number()) |>
#'   ungroup()
#' ```
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
  if (var %in% colnames(x)) {
    insight::format_error(
      paste0("There is already a variable named `", var, "` in your dataset.")
    )
  }
  original_x <- x
  rn <- data.frame(rn = rownames(x), stringsAsFactors = FALSE)
  x <- cbind(rn, x)
  colnames(x)[1] <- var
  rownames(x) <- NULL
  x <- .replace_attrs(x, attributes(original_x))
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

  original_x <- x
  rownames(x) <- x[[var]]
  x[[var]] <- NULL
  x <- .replace_attrs(x, attributes(original_x))
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
  UseMethod("rowid_as_column")
}

#' @export
rowid_as_column.default <- function(x, var = "rowid") {
  if (is.null(var)) {
    var <- "rowid"
  }
  if (!is.character(var)) {
    insight::format_error("Argument 'var' must be of type character.")
  }
  if (var %in% colnames(x)) {
    insight::format_error(
      paste0("There is already a variable named `", var, "` in your dataset.")
    )
  }
  original_x <- x
  rn <- data.frame(rn = seq_len(nrow(x)), stringsAsFactors = FALSE)
  x <- cbind(rn, x)
  colnames(x)[1] <- var
  rownames(x) <- NULL
  x <- .replace_attrs(x, attributes(original_x))
  x
}


#' @export
rowid_as_column.grouped_df <- function(x, var = "rowid") {
  if (!is.character(var)) {
    insight::format_error("Argument 'var' must be of type character.")
  }
  if (var %in% colnames(x)) {
    insight::format_error(
      paste0("There is already a variable named `", var, "` in your dataset.")
    )
  }

  grps <- attr(x, "groups", exact = TRUE)
  grps <- grps[[".rows"]]

  for (i in seq_along(grps)) {
    x[grps[[i]], var] <- seq_along(grps[[i]])
  }

  # can't just put select = "var" because there could be another variable
  # called var
  x <- data_relocate(x, paste0("^", var, "$"), regex = TRUE, before = 1)

  x
}
