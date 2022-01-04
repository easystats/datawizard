#' Extract a single column or element from an object
#'
#' `extract()` is similar to `$`. It extracts a single column or element from an object
#' (e.g., a data frame, list, )
#'
#' @param data The object to subset. Methods are currently available for data frames
#'   and data frame extensions (e.g., tibbles).
#' @param select 	A variable specified as:
#'
#'   - a literal variable name (e.g., `column_name`)
#'   - a character vector with the variable name (e.g., `"column_name"`)
#'   - a positive integer, giving the position counting from the left
#'   - a negative integer, giving the position counting from the right.
#'
#'   The default returns the last column.
#'   If the special value `0` or `"row.names"` is given, the row names of the
#'   object (if any) are extracted.
#' @param name An optional argument that specifies the column to be used as
#'   names for for the vector after extraction.
#'   Specified in the same way as `select`.
#' @param ... For use by future methods.
#'
#' @return A vector containing the extracted element.
#' @export
#'
#' @examples
#' extract(mtcars, cyl, name = gear)
#' extract(mtcars, "cyl", name = gear)
#' extract(mtcars, -1, name = gear)
#' extract(mtcars, cyl, name = 0)
#' extract(mtcars, cyl, name = "row.names")
data_extract <- function(data, select, name = NULL, ...) {
  UseMethod("data_extract")
}

#' @export
#' @rdname data_extract
extract <- data_extract

#' @export
data_extract.data.frame <- function(data, select, name = NULL, ...) {
  nl <- as.list(seq_along(data))
  names(nl) <- names(data)

  select <- eval(substitute(select), nl, parent.frame())
  if (is.numeric(select) && select < 0) {
    select <- length(data) + select + 1
  } else if (is.numeric(select) && select == 0) {
    select <- rownames(data)
  } else if (is.character(select) && select == "row.names") {
    select <- rownames(data)
  }

  name <- eval(substitute(name), nl, parent.frame())
  if (is.numeric(name) && name < 0) {
    name <- length(data) + name + 1
  } else if (is.numeric(name) && name == 0) {
    name <- rownames(data)
  } else if (is.character(name) && name == "row.names") {
    name <- rownames(data)
  }
  if (length(name) == 1) {
    stats::setNames(data[, select, drop = TRUE], data[, name, drop = TRUE])
  } else {
    stats::setNames(data[, select, drop = TRUE], name)
  }
}
