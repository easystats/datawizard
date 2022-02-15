#' @title Recode (or "cut") data into groups of values.
#' @name data_recode
#'
#' @description
#' This functions divides the range of variables into intervals and recodes
#' the values inside these intervals according to their related interval.
#' It is basically a wrapper around base R's `cut()`, providing a simplified
#' and more accessible way to define the interval breaks (cut-off values).
#'
#' @param x A data frame, numeric vector or factor.
#' @param split Character vector, indicating where to split variables, or
#'   numeric values, indicating cut-off values. If character, may be one of
#'   `"median"`, `"mean"`, `"quantile"`, `"equal_length"`, or `"equal_range"`.
#' @param n_groups If `split` is `"quantile"` or `"equal_length"`, this defines
#'   the number of requested groups (i.e. resulting number of levels or values)
#'   for the recoded variable(s). `"quantile"` will define intervals based
#'   on the distribution of the variable, while `"equal_length"` tries to
#'   divide the range of the variable into pieces of equal length.
#' @param range If `split = "equal_range"`, this defines the range of values
#'   that are recoded into a new value.
#' @param lowest Minimum value of the recoded variable(s). If `NULL` (the default),
#'   for numeric variables, the minimum of the original input is preserved. For
#'   factors, the default minimum is `1`. For `split = "equal_range"`, the
#'   default minimum is always `1`, unless specified otherwise in `lowest`.
#' @param force Logical, if `TRUE`, forces recoding of factors as well.
#' @param append Logical or string. If `TRUE`, recoded variables get new
#'   column names (with the suffix `"_r"`) and are appended (column bind) to `x`,
#'   thus returning both the original and the recoded variables. If `FALSE`,
#'   original variables in `x` will be overwritten by their recoded versions.
#'   If a character value, recoded variables are appended with new column
#'   names (using the defined suffix) to the original data frame.
#' @inheritParams standardize
#' @param ... not used.
#'
#' @details
#'
#'   \subsection{Splits and cut-off values}{
#'   Cut-off values, which are based on the median, mean or quantile
#'   functions, are _exclusive_, this means that these values indicate the
#'   lower bound of the next group to begin. Take a simple example, a numeric
#'   variable with values from 1 to 9. The median would be 5, thus 1-4 are
#'   recoded into 1, while 5-9 would turn into 2 (compare
#'   `cbind(1:9, data_recode(1:9))`). The same variable, using `split = "quantile"`
#'   and `n_groups = 3` would define cut-off points at 3.67 and 6.33 (see
#'   `quantile(1:9, probs = c(1/3, 2/3)`), which means that values from 1 to 3
#'   are recoded into 1 (because the next group of values to be recoded starts
#'   at 3.67), 4 to 6 into 2 and 7 to 9 into 3.
#'   }
#'
#'   \subsection{Recoding into groups with equal size or range}{
#'   `split = "equal_length"` and `split = "equal_range"` try to divide the
#'   range of `x` into intervals of similar (or same) length. The difference is
#'   that `split = "equal_length"` will divide the range of `x` into `n_groups`
#'   pieces and thereby defining the intervals used as breaks (hence, it is
#'   equivalent to `cut(x, breaks = n_groups)`), while  `split = "equal_range"`
#'   will cut `x` into intervals that all have the length of `range`, where the
#'   first interval by defaults starts at `1`. The lowest (or starting) value
#'   of that interval can be defined using the `lowest` argument.
#'   }
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
#' # into 3 groups, manual cut offs
#' table(data_recode(x, split = c(3, 5)))
#'
#' set.seed(123)
#' x <- sample(1:100, size = 500, replace = TRUE)
#'
#' # into 5 groups, try to recode into intervals of similar length,
#' # i.e. the range within groups is the same for all groups
#' table(data_recode(x, split = "equal_length", n_groups = 5))
#'
#' # into 5 groups, try to return same range within groups
#' # i.e. 1-20, 21-40, 41-60, etc. Since the range of "x" is
#' # 1-100, and we have a range of 20, this results into 5
#' # groups, and thus is for this particular case identical
#' # to the previous result.
#' table(data_recode(x, split = "equal_range", range = 20))
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
data_recode.numeric <- function(x, split = "median", n_groups = NULL, range = NULL, lowest = 1, ...) {
  # check arguments
  if (is.character(split)) {
    split <- match.arg(split, choices = c("median", "mean", "quantile", "equal_length", "equal_range", "equal", "equal_distance", "range", "distance"))
  }

  if (is.character(split) && split %in% c("quantile", "equal_length") && is.null(n_groups)) {
    stop(insight::format_message("Recoding based on quantiles or equal-sized groups requires the 'n_groups' argument to be specified."), call. = FALSE)
  }

  if (is.character(split) && split == "equal_range" && is.null(n_groups) && is.null(range)) {
    stop(insight::format_message("Recoding into groups with equal range requires either the 'range' or 'n_groups' argument to be specified."), call. = FALSE)
  }


  # handle aliases
  if (identical(split, "equal_length")) {
    split <- "length"
  }

  if (identical(split, "equal_range")) {
    split <- "range"
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
      "length" = n_groups,
      "quantile" = stats::quantile(x, probs = seq_len(n_groups) / n_groups),
      "range" = .equal_range(x, range, n_groups, lowest),
      NULL
    )
  }

  # complete ranges, including minimum and maximum
  if (!identical(split, "length")) cutoffs <- unique(c(min(x), cutoffs, max(x)))

  # recode into groups
  out <- droplevels(cut(
    x,
    breaks = cutoffs,
    include.lowest = TRUE,
    right = FALSE
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
  as.factor(data_recode(as.numeric(x), ...))
}


#' @rdname data_recode
#' @export
data_recode.data.frame <- function(x, split = "median", n_groups = NULL, range = NULL, lowest = 1, select = NULL, exclude = NULL, force = FALSE, append = FALSE, ...) {
  # process arguments
  args <- .process_std_args(x, select, exclude, weights = NULL, append, append_suffix = "_r", force)

  # update processed arguments
  x <- args$x
  select <- args$select

  x[select] <- lapply(x[select], data_recode, split = split, n_groups = n_groups, range = range, lowest = lowest, ...)
  x

}




# tools --------------------

.equal_range <- function(x, range, n_groups, lowest = NULL) {
  if (is.null(lowest)) lowest <- 1
  if (is.null(range)) {
    size <- ceiling((max(x) - min(x)) / n_groups)
    range <- as.numeric(size)
  }
  seq(lowest, max(x), by = range)
}
