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
#'   - a string with the variable name (e.g., `"column_name"`)
#'   - a positive integer, giving the position counting from the left
#'   - a negative integer, giving the position counting from the right.
#'
#'  or one of the following select-helpers: `starts_with("")`, `end_with("")`,
#'  `contains("")`, or `"regex()"`. Multiple variables can also be extracted
#'  using a character vector of length > 1, or a numeric vector containing
#'  column indices. If the special value `0` or `"row.names"` is given, the
#'  row names of the object (if any) are extracted.
#' @param name An optional argument that specifies the column to be used as
#'   names for for the vector after extraction.
#'   Specified in the same way as `select`.
#' @param extract String, indicating which element will be extracted when `select`
#'   matches multiple variables. Can be `"all"` (the default) to return all
#'   matched variables, `"first"` or `"last"` to return the first or last match,
#'   or `"odd"` and `"even"` to return all odd-numbered or even-numbered
#'   matches. Note that `"first"` or `"last"` return a vector (unless
#'   `as_data_frame = TRUE`), while `"all"` can return a vector (if only one
#'   match was found) *or* a data frame (for more than one match). Type safe
#'   return values are only possible when `extract` is `"first"` or `"last"` (will
#'   always return a vector) or when `as_data_frame = TRUE` (always returns a
#'   data frame).
#' @param as_data_frame Logical, if `TRUE`, will always return a data frame,
#'   even if only one variable was matched. If `FALSE`, either returns a vector
#'   or a data frame. See `extract` for details.
#' @param ignore_case Logical, if `TRUE` and when one of the select-helpers or
#'   a regular expression is used in `select`, ignores lower/upper case in the
#'   search pattern when matching against variable names.
#' @param verbose Toggle warnings.
#' @param ... For use by future methods.
#'
#' @details `data_extract()` can be used to select multiple variables or pull a
#' single variable from a data frame. Thus, the return value is by default not
#' type safe - `data_extract()` either returns a vector or a data frame.
#' \subsection{Extracting single variables (vectors)}{
#' When `select` is the name of a single column, or when select only matches
#' one column, a vector is returned. A single variable is also returned when
#' `pull` is either `"first` or `"last"`. Setting `as_data_frame` to `TRUE`
#' overrides this behaviour and *always* returns a data frame.
#' }
#' \subsection{Extracting a data frame of variables}{
#' When `select` is a character vector containing more than one column name (or
#' a numeric vector with more than one valid column indices), or when `select`
#' uses one of the supported select-helpers that match multiple columns, a
#' data frame is returned. Setting `as_data_frame` to `TRUE` *always* returns
#' a data frame.
#' }
#'
#' @return A vector (or a data frame) containing the extracted element, or
#'   `NULL` if no matching variable was found.
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
#'
#' # select first of multiple variables
#' extract(iris, starts_with("Sepal"), extract = "first")
#' # select first of multiple variables, return as data frame
#' head(extract(iris, starts_with("Sepal"), extract = "first", as_data_frame = TRUE))
data_extract <- function(data, select, ...) {
  UseMethod("data_extract")
}

#' @export
#' @rdname data_extract
extract <- data_extract

#' @rdname data_extract
#' @export
data_extract.data.frame <- function(data,
                                    select,
                                    name = NULL,
                                    extract = "all",
                                    as_data_frame = FALSE,
                                    ignore_case = FALSE,
                                    verbose = TRUE,
                                    ...) {
  fixed <- TRUE
  extract <- match.arg(tolower(extract), choices = c("all", "first", "last", "odd", "even"))

  # avoid conflicts
  conflicting_packages <- .conflicting_packages("poorman")

  # make sure as_data_frame is logical
  if (!is.logical(as_data_frame)) {
    as_data_frame <- FALSE
  }

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
    evaluated_pattern <- .evaluate_pattern(.safe_deparse(p), data, ignore_case)
    select <- evaluated_pattern$pattern
    fixed <- evaluated_pattern$fixed
  }

  # seems to be no valid column name or index, so try to grep
  if (isFALSE(fixed)) {
    select <- colnames(data)[grepl(select, colnames(data), ignore.case = ignore_case)]
  }

  # load again
  .attach_packages(conflicting_packages)

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

  # return valid column names, based on pattern
  select <- .evaluated_pattern_to_colnames(select, data, ignore_case, verbose)

  # nothing to select?
  if (!length(select)) {
    return(NULL)
  }

  # chose which matched variables to extract
  select <- switch(extract,
    "first" = select[1],
    "last" = select[length(select)],
    "odd" = select[seq(1, length(select), 2)],
    "even" = select[seq(2, length(select), 2)],
    select
  )

  if (!is.null(name) && length(name) == 1) {
    stats::setNames(data[, select, drop = !as_data_frame], data[, name, drop = !as_data_frame])
  } else {
    if (is.null(name) && (length(select) > 1 || isTRUE(as_data_frame))) {
      name <- select
    }
    stats::setNames(data[, select, drop = !as_data_frame], name)
  }
}
