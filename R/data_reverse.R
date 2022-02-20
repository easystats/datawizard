#' Reverse-Score Variables
#'
#' Reverse-score variables (change the keying/scoring direction).
#'
#' @inheritParams standardize.data.frame
#'
#' @param x A numeric variable.
#' @param range Initial (old) range of values. If `NULL`, will take the range of
#'   the input vector (`range(x)`).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' data_reverse(c(1, 2, 3, 4, 5))
#' data_reverse(c(-2, -1, 0, 2, 1))
#'
#' # Specify the "theoretical" range of the input vector
#' data_reverse(c(1, 3, 4), range = c(0, 4))
#'
#' # Data frames
#' head(data_reverse(iris)
#' head(data_reverse(iris, select = "Sepal.Length"))
#'
#' @return A reverse-scored object.
#'
#' @family transform utilities
#'
#' @seealso [data_rescale()] to change the score range for variables (potentially while reversing),
#'   [normalize()] [standardize()] [ranktransform()]
#'
#' @export
data_reverse <- function(x, ...) {
  UseMethod("data_reverse")
}


#' @rdname data_reverse
#' @export
reverse_scale <- function(x, ...) {
  # Alias for data_reverse()
  data_reverse(x, ...)
}




#' @rdname data_reverse
#' @export
data_reverse.numeric <- function(x,
                                 range = NULL,
                                 verbose = TRUE,
                                 ...) {

  # Warning if all NaNs
  if (all(is.na(x))) {
    return(x)
  }

  # Warning if only one value
  if (length(unique(x)) == 1 && is.null(range)) {
    if (verbose) {
      warning(paste0("A `range` must be provided for data with only one unique value."))
    }
    return(x)
  }

  if (is.null(range)) {
    range <- c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  }

  min <- ifelse(is.na(range[1]), min(x, na.rm = TRUE), range[1])
  max <- ifelse(is.na(range[2]), max(x, na.rm = TRUE), range[2])
  new_min <- max
  new_max <- min

  out <- as.vector((new_max - new_min) / (max - min) * (x - min) + new_min)
  out
}




#' @export
data_reverse.factor <- function(x, range = NULL, verbose = TRUE, ...) {

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

  if (!is.null(range)) {
    old_levels <- range
    x <- factor(x, levels = range)
  } else {
    old_levels <- levels(x)
  }

  int_x <- as.integer(x)
  rev_x <- data_reverse(int_x, range = c(1, length(old_levels)))
  x <- factor(rev_x, levels = seq_len(length(old_levels)), labels = old_levels)
  x
}




#' @rdname data_reverse
#' @export
data_reverse.grouped_df <- function(x,
                                    range = NULL,
                                    select = NULL,
                                    exclude = NULL,
                                    ...) {
  info <- attributes(x)

  # dplyr >= 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
    grps <- lapply(grps, function(x) x + 1)
  } else {
    grps <- grps[[".rows"]]
  }

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- data_reverse(
      x[rows, ],
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


#' @rdname data_reverse
#' @export
data_reverse.data.frame <- function(x,
                                    range = NULL,
                                    select = NULL,
                                    exclude = NULL,
                                    ...) {

  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  # Transform the range so that it is a list now
  if (!is.null(range)) {
    if (!is.list(range)) {
      range <- stats::setNames(rep(list(range), length(select)), select)
    }
  }

  x[select] <- as.data.frame(sapply(select, function(n) {
    data_reverse(x[[n]], range = range[[n]])
  }, simplify = FALSE))
  x
}
