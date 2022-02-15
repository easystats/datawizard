#' @title Recode data
#' @name data_recode
#'
#' @description
#' Recode data.
#'
#' @param x A data frame, numeric vector or factor.
#' @param split Character vector, indicating where to split variables, or
#'   numeric values, indicating cut-off values. If character, may be one of
#'   `"median"`, `"mean"`, `"quantile"`, `"equal_size"` or `"equal_range"`.
#' @param n_groups If `split` is `"quantile"` (or its alias `"equal_size"`),
#'   this defines the number of requested groups (i.e. resulting number of
#'   levels or values) for the recoded variable(s).
#' @param range If `split = "equal_range"`, this defines the range of values
#'   that are recoded into a new value.
#' @param lowest Minimum value of the recoded variable.
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
#' # into 5 groups, try to return similar group sizes
#' table(data_recode(x, split = "quantile", n_groups = 5))
#'
#' # into 5 groups, try to return same range within groups
#' # i.e. 1-20, 21-40, 41-60, etc.
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
    split <- match.arg(split, choices = c("median", "mean", "quantile", "equal_size", "equal_range", "equal", "equal_distance", "range", "distance"))
  }

  if (is.character(split) && split %in% c("quantile", "equal_size") && is.null(n_groups)) {
    stop(insight::format_message("Recoding based on quantiles or equal-sized groups requires the 'n_groups' argument to be specified."), call. = FALSE)
  }

  if (is.character(split) && split == "equal_range" && is.null(range)) {
    stop(insight::format_message("Recoding into groups with equal range requires the 'range' argument to be specified."), call. = FALSE)
  }


  # handle aliases
  if (identical(split, "equal_size")) {
    split <- "size"
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
      "size" = ,
      "quantile" = stats::quantile(x, probs = seq_len(n_groups) / n_groups),
      "range" = .equal_range(x, range),
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
  args <- .process_std_args(x, select, exclude, weights, append, append_suffix = "_r", force)

  # update processed arguments
  x <- args$x
  select <- args$select

  x[select] <- lapply(x[select], data_recode, split = split, n_groups = n_groups, range = range, lowest = lowest, ...)
  x

}




# tools --------------------

.equal_range <- function(x, range) {
  if (is.null(range)) {
    size <- ceiling((max(x) - min(x)) / range)
    range <- as.numeric(size)
  }
  seq(min(x), max(x), by = range)
}
