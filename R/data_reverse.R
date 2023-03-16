#' Reverse-Score Variables
#'
#' Reverse-score variables (change the keying/scoring direction).
#'
#' @param range Range of values that is used as reference for reversing the
#'   scale. For numeric variables, can be `NULL` or a numeric vector of length
#'   two, indicating the lowest and highest value of the reference range. If
#'   `NULL`, will take the range of the input vector (`range(x)`). For factors,
#'   `range` can be `NULL`, a numeric vector of length two, or a (numeric)
#'   vector of at least the same length as factor levels (i.e. must be equal
#'   to or larger than `nlevels(x)`). Note that providing a `range` for factors
#'   usually only makes sense when factor levels are numeric, not characters.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams categorize
#' @inheritParams find_columns
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @examples
#' reverse(c(1, 2, 3, 4, 5))
#' reverse(c(-2, -1, 0, 2, 1))
#'
#' # Specify the "theoretical" range of the input vector
#' reverse(c(1, 3, 4), range = c(0, 4))
#'
#' # Factor variables
#' reverse(factor(c(1, 2, 3, 4, 5)))
#' reverse(factor(c(1, 2, 3, 4, 5)), range = 0:10)
#'
#' # Data frames
#' head(reverse(iris))
#' head(reverse(iris, select = "Sepal.Length"))
#'
#' @return A reverse-scored object.
#'
#' @family transform utilities
#'
#' @inherit data_rename seealso
#'
#' @export
reverse <- function(x, ...) {
  UseMethod("reverse")
}


#' @rdname reverse
#' @export
reverse_scale <- reverse


#' @export
reverse.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert(
      paste0(
        "Variables of class '",
        class(x)[1],
        "' can't be recoded and remain unchanged."
      )
    )
  }

  x
}


#' @rdname reverse
#' @export
reverse.numeric <- function(x,
                            range = NULL,
                            verbose = TRUE,
                            ...) {
  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  # Warning if only one value
  if (insight::has_single_value(x) && is.null(range)) {
    if (verbose) {
      insight::format_warning("A `range` must be provided for data with only one unique value.")
    }
    return(x)
  }

  # no missing values allowed
  if (anyNA(range)) {
    insight::format_error("`range` is not allowed to have missing values.")
  }

  if (is.null(range)) {
    range <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  }

  # old minimum and maximum
  min <- min(range)
  max <- max(range)

  # check if a valid range (i.e. vector of length 2) is provided
  if (length(range) > 2) {
    insight::format_error(
      "`range` must be a numeric vector of length two, indicating lowest and highest value of the required range.",
      sprintf("Did you want to provide `range = c(%g, %g)`?", min, max)
    )
  }

  new_min <- max
  new_max <- min

  out <- as.vector((new_max - new_min) / (max - min) * (x - min) + new_min)

  # labelled data?
  out <- .set_back_labels(out, x)
  out
}



#' @export
reverse.factor <- function(x, range = NULL, verbose = TRUE, ...) {
  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  # Warning if only one value
  if (insight::has_single_value(x) && is.null(range)) {
    if (verbose) {
      insight::format_warning("A `range` must be provided for data with only one unique value.")
    }
    return(x)
  }

  # save for later use
  original_x <- x

  if (!is.null(range)) {
    # no missing values allowed
    if (anyNA(range)) {
      insight::format_error("`range` is not allowed to have missing values.")
    }
    range_ok <- TRUE
    # if we have a vector of length 2 for range, and more factor levels,
    # we assume `range` indicates minimum and maximum of range values
    if (length(range) == 2 && nlevels(droplevels(x)) > 2) {
      if (is.numeric(range)) {
        range <- min(range):max(range)
      } else {
        # if range is of length 2, and we have more than 2 number of levels,
        # range must be numeric to indicate minima and maxima. if not, stop.
        range_ok <- FALSE
      }
    }
    if (length(range) > 2 && length(range) < nlevels(droplevels(x))) {
      # if range has more than two values, but fewer values than number of
      # factor levels, we cannot associate the reversed scale, so stop
      range_ok <- FALSE
    }
    if (!range_ok) {
      insight::format_error(
        "`range` must be one of the following:",
        "- a numeric vector of length two, indicating lowest and highest value of the required range,",
        "- a vector (numeric or character) of values with at least as many values as number of levels in `x`,",
        "- or `NULL`."
      )
    }
    # check if no or not all old levels are in new range
    if (verbose) {
      if (!any(levels(x) %in% as.character(range))) {
        insight::format_warning(
          "No current factor level is included in `range`.",
          "Returned factor will only contain missing values."
        )
      } else if (!all(levels(x) %in% as.character(range))) {
        insight::format_warning(
          "Not all current factor levels are included in `range`.",
          "Returned factor will contain missing values."
        )
      }
    }
    old_levels <- range
    x <- factor(x, levels = range)
  } else {
    old_levels <- levels(x)
  }

  int_x <- as.integer(x)
  rev_x <- reverse(int_x, range = c(1, length(old_levels)))
  x <- factor(rev_x, levels = seq_len(length(old_levels)), labels = old_levels)

  # labelled data?
  x <- .set_back_labels(x, original_x)

  x
}



#' @export
reverse.grouped_df <- function(x,
                               select = NULL,
                               exclude = NULL,
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
    x[rows, ] <- reverse(
      x[rows, , drop = FALSE],
      select = select,
      exclude = exclude,
      range = range,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}



#' @rdname reverse
#' @export
reverse.data.frame <- function(x,
                               select = NULL,
                               exclude = NULL,
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
  if (!is.null(range) && !is.list(range)) {
    range <- stats::setNames(rep(list(range), length(select)), select)
  }

  x[select] <- lapply(select, function(n) {
    reverse(x[[n]], range = range[[n]])
  })
  x
}



# helper -----------------------------

.set_back_labels <- function(new, old, include_values = TRUE) {
  # labelled data?
  attr(new, "label") <- attr(old, "label", exact = TRUE)
  labels <- attr(old, "labels", exact = TRUE)
  if (isTRUE(include_values) && !is.null(labels)) {
    attr(new, "labels") <- stats::setNames(rev(labels), names(labels))
  }
  new
}
