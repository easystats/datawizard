#' Reverse-Score Variables
#'
#' Reverse-score variables (change the keying/scoring direction).
#'
#' @param range Initial (old) range of values. If `NULL`, will take the range of
#'   the input vector (`range(x)`).
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
    message(insight::format_message(paste0("Variables of class '", class(x)[1], "' can't be recoded and remain unchanged.")))
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
  if (length(unique(x)) == 1 && is.null(range)) {
    if (verbose) {
      warning("A `range` must be provided for data with only one unique value.", call. = FALSE)
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
  if (length(unique(x)) == 1 && is.null(range)) {
    if (verbose) {
      warning("A `range` must be provided for data with only one unique value.", call. = FALSE)
    }
    return(x)
  }

  # save for later use
  original_x <- x

  if (!is.null(range)) {
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
                               ...) {
  info <- attributes(x)

  # dplyr >= 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)

  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
    grps <- lapply(grps, function(x) x + 1)
  } else {
    grps <- grps[[".rows"]]
  }

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
                               ...) {
  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)

  # Transform the range so that it is a list now
  if (!is.null(range)) {
    if (!is.list(range)) {
      range <- stats::setNames(rep(list(range), length(select)), select)
    }
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
