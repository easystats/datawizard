#' Extract one or more columns or elements from an object
#'
#' `data_extract()` (or its alias `extract()`) is similar to `$`. It extracts
#' either a single column or element from an object (e.g., a data frame, list),
#' or multiple columns resp. elements.
#'
#' @param data The object to subset. Methods are currently available for data frames
#'   and data frame extensions (e.g., tibbles).
#' @param select 	Either a variable specified as:
#'
#'   - a literal variable name (e.g., `column_name`)
#'   - a string vector with the variable name (e.g., `"column_name"`)
#'   - a positive integer, giving the position counting from the left
#'   - a negative integer, giving the position counting from the right.
#'
#'  or one of the following select-helpers: `starts_with("")`, `end_with("")` or
#'  `contains("")`. Multiple variables can also be extracts using a character
#'  vector of length > 1. The default returns the last column. If the special
#'  value `0` or `"row.names"` is given, the row names of the object (if any)
#'  are extracted.
#' @param name An optional argument that specifies the column to be used as
#'   names for for the vector after extraction.
#'   Specified in the same way as `select`.
#' @param verbose Toggle warnings.
#' @param ... For use by future methods.
#'
#' @return A vector containing the extracted element.
#' @export
#'
#' @examples
#' # single variable
#' extract(mtcars, cyl, name = gear)
#' extract(mtcars, "cyl", name = gear)
#' extract(mtcars, -1, name = gear)
#' extract(mtcars, cyl, name = 0)
#' extract(mtcars, cyl, name = "row.names")
#'
#' # selecting multiple variables
#' head(extract(iris, starts_with("Sepal")))
#' head(extract(iris, ends_with("Width")))
#' head(extract(iris, 2:4))
data_extract <- function(data, select, name = NULL, ...) {
  UseMethod("data_extract")
}

#' @export
#' @rdname data_extract
extract <- data_extract

#' @rdname data_extract
#' @export
data_extract.data.frame <- function(data, select, name = NULL, verbose = TRUE, ...) {
  fixed <- TRUE

  # in case pattern is a variable from another function call...
  p <- try(eval(select), silent = TRUE)
  if (inherits(p, c("try-error", "simpleError"))) {
    p <- substitute(select)
  }

  # check if pattern is a function like "starts_with()"
  select <- tryCatch(eval(p), error = function(e) NULL)

  # if select could not be evaluated (because expression "makes no sense")
  # try to evaluate and find select-helpers. In this case, set fixed = FALSE,
  # so we can use grepl()
  if (is.null(select)) {
    fixed <- FALSE
    select <- .evaluate_pattern(.safe_deparse(p))
  }

  # seems to be no valid column name or index, so try to grep
  if (isFALSE(fixed)) {
    select <- colnames(data)[grepl(select, colnames(data))]
  }


  if (is.numeric(select)) {
    if (length(select) == 1) {
      if (select < 0) {
        # select last column
        select <- colnames(data)[ncol(data) + select + 1]
      } else if (select == 0) {
        # select row names
        select <- rownames(data)
      }
    } else {
      # make sure we have valid column indices
      select <- colnames(data)[intersect(select, 1:ncol(data))]
    }
  } else if (is.character(select) && identical(select, "row.names")) {
    select <- rownames(data)
  }

  nl <- as.list(seq_along(data))
  names(nl) <- names(data)
  name <- eval(substitute(name), nl, parent.frame())

  if (is.numeric(name) && length(name) == 1) {
    if (name < 0) {
      name <- ncol(data) + name + 1
    } else if (name == 0) {
      name <- rownames(data)
    }
  } else if (is.character(name) && identical(name, "row.names")) {
    name <- rownames(data)
  }

  # check if colnames are in data
  if (!all(select %in% colnames(data))) {
    if (isTRUE(verbose)) {
      warning(insight::format_message(
        paste0("Following variable(s) were not found: ", paste0(setdiff(select, colnames(data)), collapse = ", "))
      ))
    }
    select <- intersect(select, colnames(data))
  }

  if (!is.null(name) && length(name) == 1) {
    stats::setNames(data[, select, drop = TRUE], data[, name, drop = TRUE])
  } else {
    if (is.null(name) && length(select) > 1) {
      if (is.numeric(select)) {
        name <- colnames(data)[select]
      } else {
        name <- select
      }
    }
    stats::setNames(data[, select, drop = TRUE], name)
  }
}
