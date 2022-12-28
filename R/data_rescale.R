#' Rescale Variables to a New Range
#'
#' Rescale variables to a new range.
#' Can also be used to reverse-score variables (change the keying/scoring direction).
#'
#' @inheritParams categorize
#' @inheritParams find_columns
#' @inheritParams standardize.data.frame
#'
#' @param to Numeric vector of length 2 giving the new range that the variable will have after rescaling.
#'   To reverse-score a variable, the range should be given with the maximum value first.
#'   See examples.
#' @param range Initial (old) range of values. If `NULL`, will take the range of
#'   the input vector (`range(x)`).
#' @param ... Arguments passed to or from other methods.
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @examples
#' rescale(c(0, 1, 5, -5, -2))
#' rescale(c(0, 1, 5, -5, -2), to = c(-5, 5))
#' rescale(c(1, 2, 3, 4, 5), to = c(-2, 2))
#'
#' # Specify the "theoretical" range of the input vector
#' rescale(c(1, 3, 4), to = c(0, 40), range = c(0, 4))
#'
#' # Reverse-score a variable
#' rescale(c(1, 2, 3, 4, 5), to = c(5, 1))
#' rescale(c(1, 2, 3, 4, 5), to = c(2, -2))
#'
#' # Data frames
#' head(rescale(iris, to = c(0, 1)))
#' head(rescale(iris, to = c(0, 1), select = "Sepal.Length"))
#'
#' # One can specify a list of ranges
#' head(rescale(iris, to = list(
#'   "Sepal.Length" = c(0, 1),
#'   "Petal.Length" = c(-1, 0)
#' )))
#' @inherit data_rename
#'
#' @return A rescaled object.
#'
#' @seealso See [makepredictcall.dw_transformer()] for use in model formulas.
#' @family transform utilities
#'
#' @export
rescale <- function(x, ...) {
  UseMethod("rescale")
}


#' @rdname rescale
#' @export
change_scale <- function(x, ...) {
  # Alias for rescale()
  rescale(x, ...)
}



#' @export
rescale.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert(
      paste0("Variables of class `", class(x)[1], "` can't be rescaled and remain unchanged.")
    )
  }
  x
}



#' @rdname rescale
#' @export
rescale.numeric <- function(x,
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

  if (is.null(range)) {
    range <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  }

  # called from "makepredictcal()"? Then we have additional arguments
  dot_args <- list(...)
  required_dot_args <- c("min_value", "max_value", "new_min", "new_max")
  flag_predict <- FALSE

  if (all(required_dot_args %in% names(dot_args))) {
    # we gather informatiom about the original data, which is needed
    # for "predict()" to work properly when "rescale()" is called
    # in formulas on-the-fly, e.g. "lm(mpg ~ rescale(hp), data = mtcars)"
    min_value <- dot_args$min_value
    max_value <- dot_args$max_value
    new_min <- dot_args$new_min
    new_max <- dot_args$new_max
    flag_predict <- TRUE
  } else {
    min_value <- ifelse(is.na(range[1]), min(x, na.rm = TRUE), range[1])
    max_value <- ifelse(is.na(range[2]), max(x, na.rm = TRUE), range[2])
    new_min <- ifelse(is.na(to[1]), min_value, to[1])
    new_max <- ifelse(is.na(to[2]), max_value, to[2])
  }

  # Warning if only one value
  if (!flag_predict && insight::has_single_value(x) && is.null(range)) {
    if (verbose) {
      insight::format_warning(
        "A `range` must be provided for data with only one unique value."
      )
    }
    return(x)
  }

  out <- as.vector((new_max - new_min) / (max_value - min_value) *
    (x - min_value) + new_min)

  attr(out, "min_value") <- min_value
  attr(out, "max_value") <- max_value
  attr(out, "new_min") <- new_min
  attr(out, "new_max") <- new_max
  attr(out, "range_difference") <- max_value - min_value
  attr(out, "to_range") <- c(new_min, new_max)
  class(out) <- c("dw_transformer", class(out))

  out
}


#' @export
rescale.grouped_df <- function(x,
                               select = NULL,
                               exclude = NULL,
                               to = c(0, 100),
                               range = NULL,
                               ignore_case = FALSE,
                               regex = FALSE,
                               verbose = FALSE,
                               ...) {
  info <- attributes(x)

  # works only for dplyr >= 0.8.0
  grps <- attr(x, "groups", exact = TRUE)[[".rows"]]

  # evaluate arguments
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- rescale(
      x[rows, , drop = FALSE],
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



#' @rdname rescale
#' @export
rescale.data.frame <- function(x,
                               select = NULL,
                               exclude = NULL,
                               to = c(0, 100),
                               range = NULL,
                               ignore_case = FALSE,
                               regex = FALSE,
                               verbose = FALSE,
                               ...) {
  # evaluate arguments
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

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
    rescale(x[[n]], to = to[[n]], range = range[[n]])
  }, simplify = FALSE))
  x
}
