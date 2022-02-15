#' @title Recode data
#' @name data_recode
#'
#' @description
#' Recode data.
#'
#' @param x A data frame or vector.
#' @param split Name of a function, or numeric values, indicating cutoffs
#' @param n_groups If `split = "quantile"`.
#' @param size_groups If `split = "range"`.
#' @param lowest Minimum value if numeric variables are recoded.
#' @param force Logical, if `TRUE`, forces recoding of factors and dates
#'   as well.
#' @inheritParams standardize
#' @param ... not used.
#'
#' @examples
#' set.seed(123)
#' x <- sample(1:10, size = 50, replace = TRUE)
#'
#' table(x)
#'
#' # by default, at median
#' table(data_recode(x))
#'
#' # into 3 groups, based on distribution (quantiles)
#' table(data_recode(x, split = "quantile", n_groups = 3))
#'
#' # into 3 groups, try to return similar group sizes
#' # (i.e. similar count for each value/level)
#' table(data_recode(x, split = "quantile", n_groups = 3))
#'
#' # into 3 groups, manual cut offs
#' table(data_recode(x, split = c(3, 5)))
#'
#' set.seed(123)
#' x <- sample(1:100, size = 500, replace = TRUE)
#'
#' # into 5 groups, try to return similar group sizes
#' table(data_recode(x, split = "equal_size", n_groups = 5))
#'
#' # into 5 groups, try to return same range within groups
#' # i.e. 1-20, 21-40, 41-60, etc.
#' table(data_recode(x, split = "equal_range", size_groups = 20))
#' @export
data_recode <- function(x, ...) {
  UseMethod("data_recode")
}


#' @export
data_recode.default <- function(x, ...) {
  return(x)
}


#' @rdname data_recode
#' @export
data_recode.numeric <- function(x, split = "median", n_groups = NULL, size_groups = NULL, lowest = 1, ...) {
  # evaluate split-function
  split_arg <- substitute(split)

  # this is required when the numeric-method is called from another function
  # then "split" might be a name of a variable
  if (!deparse(split_arg) %in% c("median", "mean", "quantile", "equal_size", "equal_range", "equal", "equal_distance", "range", "distance")) {
    split <- eval(split_arg)
  } else {
    split <- split_arg
  }

  if (is.numeric(eval(split))) {
    split <- eval(split)
  } else if (!is.character(split)) {
    split <- deparse(split)
  }


  # handle aliases
  if (identical(split, "equal_size")) {
    split <- "equal"
  }

  if (identical(split, "equal_distance") || identical(split, "range") || identical(split, "equal_range")) {
    split <- "distance"
  }


  # save
  original_x <- x

  # no missings
  x <- stats::na.omit(x)

  # stop if all NA
  if (!length(x)) {
    warning(insight::format_message("Variable contains only missing values. No recoding carried out."), call. = FALSE)
    return(original_x)
  }

  if (is.numeric(split)) {
    cutoffs <- split
  } else {
    cutoffs <- switch(
      split,
      "median" = stats::median(x),
      "mean" = mean(x),
      "quantile" = stats::quantile(x, probs = length(x) / (rev(seq(1:n_groups)) * length(x))),
      "equal" = .equal_groups(x, n_groups),
      "distance" = .equal_distance(x, size_groups),
      NULL
    )
  }

  # complete ranges, including minimum and maximum
  cutoffs <- unique(c(min(x), cutoffs, max(x)))

  # recode into groups
  out <- droplevels(cut(
    x,
    breaks = cutoffs,
    include.lowest = TRUE,
    right = !identical(split, "equal") && !identical(split, "distance")
  ))
  levels(out) <- 1:nlevels(out)

  # fix lowest value, add back into original vector
  out <- as.numeric(out)
  out <- out - (min(out) - lowest)
  original_x[!is.na(original_x)] <- out

  original_x
}


#' @export
data_recode.factor <- function(x, ...) {
  levels(x) <- 1:nlevels(x)
  data_recode(as.numeric(x), ...)
}


#' @rdname data_recode
#' @export
data_recode.data.frame <- function(x, split = "median", n_groups = NULL, size_groups = NULL, lowest = 1, select = NULL, exclude = NULL, force = FALSE, ...) {
  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  select <- .select_variables(x, select, exclude, force = force)

  x[select] <- lapply(x[select], data_recode, split = split, n_groups = n_groups, size_groups = size_groups, lowest = lowest, ...)
  x

}




# tools --------------------

.equal_groups <- function(x, n_groups) {
  nominator <- seq_len(n_groups - 1)
  denominator <- rep(n_groups, length(nominator))
  qu_prob <- nominator / denominator
  stats::quantile(x, probs = qu_prob)
}


.equal_distance <- function(x, size_groups) {
  if (is.null(size_groups)) {
    size <- ceiling((max(x) - min(x)) / size_groups)
    size_groups <- as.numeric(size)
  }
  seq(min(x), max(x), by = size_groups)
}
