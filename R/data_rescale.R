#' Rescale Variables to a New Range
#'
#' Rescale variables to a new range.
#'
#' @inheritParams standardize.data.frame
#'
#' @param x A numeric variable.
#' @param to New range that the variable will have after rescaling.
#' @param range Initial (old) range of values. If `NULL`, will take the range of
#'   the input vector (\code{range(x)}).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' data_rescale(c(0, 1, 5, -5, -2))
#' data_rescale(c(0, 1, 5, -5, -2), to = c(-5, 5))
#'
#' # Specify the "theoretical" range of the input vector
#' data_rescale(c(1, 3, 4), to = c(0, 40), range = c(0, 4))
#'
#' # Dataframes
#' head(data_rescale(iris, to = c(0, 1)))
#' head(data_rescale(iris, to = c(0, 1), select = "Sepal.Length"))
#'
#' # One can specify a list of ranges
#' head(data_rescale(iris, to = list(
#'   "Sepal.Length" = c(0, 1),
#'   "Petal.Length" = c(-1, 0)
#' )))
#' @seealso [normalize()] [standardize()] [ranktransform()]
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
  # TODO: Don't deprecate for now
  # so we have time to change it accross the verse, but for next round
  # .Deprecated("data_rescale")
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
      warning(paste0("A `range` must be provided for data with only one observation."))
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
    x[rows, ] <- change_scale(
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


#' @importFrom stats setNames
#' @rdname data_rescale
#' @export
data_rescale.data.frame <- function(x,
                                    to = c(0, 100),
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
      range <- setNames(rep(list(range), length(select)), select)
    }
  }
  # Transform the 'to' so that it is a list now
  if (!is.list(to)) {
    to <- setNames(rep(list(to), length(select)), select)
  }

  x[select] <- as.data.frame(sapply(select, function(n) {
    data_rescale(x[[n]], to = to[[n]], range = range[[n]])
  }, simplify = FALSE))
  x
}
