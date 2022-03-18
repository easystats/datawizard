#' Rescale Variables to a New Range
#'
#' Rescale variables to a new range.
#' Can also be used to reverse-score variables (change the keying/scoring direction).
#'
#' @inheritParams data_cut
#' @inheritParams convert_to_na
#' @inheritParams standardize.data.frame
#'
#' @param to Numeric vector of length 2 giving the new range that the variable will have after rescaling.
#'   To reverse-score a variable, the range should be given with the maximum value first.
#'   See examples.
#' @param range Initial (old) range of values. If `NULL`, will take the range of
#'   the input vector (`range(x)`).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' data_rescale(c(0, 1, 5, -5, -2))
#' data_rescale(c(0, 1, 5, -5, -2), to = c(-5, 5))
#' data_rescale(c(1, 2, 3, 4, 5), to = c(-2, 2))
#'
#' # Specify the "theoretical" range of the input vector
#' data_rescale(c(1, 3, 4), to = c(0, 40), range = c(0, 4))
#'
#' # Reverse-score a variable
#' data_rescale(c(1, 2, 3, 4, 5), to = c(5, 1))
#' data_rescale(c(1, 2, 3, 4, 5), to = c(2, -2))
#'
#' # Data frames
#' head(data_rescale(iris, to = c(0, 1)))
#' head(data_rescale(iris, to = c(0, 1), select = "Sepal.Length"))
#'
#' # One can specify a list of ranges
#' head(data_rescale(iris, to = list(
#'   "Sepal.Length" = c(0, 1),
#'   "Petal.Length" = c(-1, 0)
#' )))
#' @inherit data_rename seealso
#'
#' @return A rescaled object.
#'
#' @family transform utilities
#'
#' @export
data_rescale <- function(x, ...) {
  UseMethod("data_rescale")
}


#' @rdname data_rescale
#' @export
change_scale <- function(x, ...) {
  # Alias for data_rescale()
  data_rescale(x, ...)
}




#' @rdname data_rescale
#' @export
data_rescale.numeric <- function(x,
                                 to = c(0, 100),
                                 range = NULL,
                                 verbose = TRUE,
                                 ...) {
  if (is.null(to)) {
    return(x)
  }

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  # Warning if only one value
  if (length(unique(x)) == 1 && is.null(range)) {
    if (verbose) {
      warning("A `range` must be provided for data with only one unique value.")
    }
    return(x)
  }

  if (is.null(range)) {
    range <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  }

  min <- ifelse(is.na(range[1]), min(x, na.rm = TRUE), range[1])
  max <- ifelse(is.na(range[2]), max(x, na.rm = TRUE), range[2])
  new_min <- ifelse(is.na(to[1]), min, to[1])
  new_max <- ifelse(is.na(to[2]), max, to[2])

  out <- as.vector((new_max - new_min) / (max - min) * (x - min) + new_min)
  out
}




#' @export
data_rescale.factor <- function(x, ...) {
  x
}




#' @rdname data_rescale
#' @export
data_rescale.grouped_df <- function(x,
                                    to = c(0, 100),
                                    range = NULL,
                                    select = NULL,
                                    exclude = NULL,
                                    ignore_case = FALSE,
                                    ...) {
  info <- attributes(x)

  # dplyr >= 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  fixed <- TRUE
  # avoid conflicts
  conflicting_packages <- .conflicting_packages("poorman")

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
    evaluated_pattern <- .evaluate_pattern(insight::safe_deparse(p), x, ignore_case = ignore_case)
    select <- evaluated_pattern$pattern
    fixed <- evaluated_pattern$fixed
  }

  # seems to be no valid column name or index, so try to grep
  if (isFALSE(fixed)) {
    select <- colnames(x)[grepl(select, colnames(x), ignore.case = ignore_case)]
  }

  # load again
  .attach_packages(conflicting_packages)

  # return valid column names, based on pattern
  select <- .evaluated_pattern_to_colnames(select, x, ignore_case, verbose = FALSE, exclude)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
    grps <- lapply(grps, function(x) x + 1)
  } else {
    grps <- grps[[".rows"]]
  }

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- data_rescale(
      x[rows, ],
      select = select,
      exclude = exclude,
      to = to,
      range = range,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}


#' @rdname data_rescale
#' @export
data_rescale.data.frame <- function(x,
                                    to = c(0, 100),
                                    range = NULL,
                                    select = NULL,
                                    exclude = NULL,
                                    ignore_case = FALSE,
                                    ...) {
  fixed <- TRUE
  # avoid conflicts
  conflicting_packages <- .conflicting_packages("poorman")

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
    evaluated_pattern <- .evaluate_pattern(insight::safe_deparse(p), x, ignore_case = ignore_case)
    select <- evaluated_pattern$pattern
    fixed <- evaluated_pattern$fixed
  }

  # seems to be no valid column name or index, so try to grep
  if (isFALSE(fixed)) {
    select <- colnames(x)[grepl(select, colnames(x), ignore.case = ignore_case)]
  }

  # load again
  .attach_packages(conflicting_packages)

  # return valid column names, based on pattern
  select <- .evaluated_pattern_to_colnames(select, x, ignore_case, verbose = FALSE, exclude)

  # Transform the range so that it is a list now
  if (!is.null(range)) {
    if (!is.list(range)) {
      range <- stats::setNames(rep(list(range), length(select)), select)
    }
  }
  # Transform the 'to' so that it is a list now
  if (!is.list(to)) {
    to <- stats::setNames(rep(list(to), length(select)), select)
  }

  x[select] <- as.data.frame(sapply(select, function(n) {
    data_rescale(x[[n]], to = to[[n]], range = range[[n]])
  }, simplify = FALSE))
  x
}
