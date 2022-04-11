#' @title Recode old values of variables into new values
#' @name data_recode
#'
#' @description
#' This functions divides the range of variables into intervals and recodes
#' the values inside these intervals according to their related interval.
#' It is basically a wrapper around base R's `cut()`, providing a simplified
#' and more accessible way to define the interval breaks (cut-off values).
#'
#' @param x A (grouped) data frame, numeric vector or factor.
#' @param split Character vector, indicating at which breaks to split variables,
#'   or numeric values with values indicating breaks. If character, may be one
#'   of `"median"`, `"mean"`, `"quantile"`, `"equal_length"`, or `"equal_range"`.
#'   `"median"` or `"mean"` will return dichotomous variables, split at their
#'   mean or median, respectively. `"quantile"` and `"equal_length"` will split
#'   the variable into `n_groups` groups, where each group refers to an interval
#'   of a specific range of values. Thus, the length of each interval will be
#'   based on the number of groups. `"equal_range"` also splits the variable
#'   into multiple groups, however, the length of the interval is given, and
#'   the number of resulting groups (and hence, the number of breaks) will be
#'   determined by how many intervals can be generated, based on the full range
#'   of the variable.
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
#' @param labels Character vector of value labels. If not `NULL`, `data_recode()`
#'   will returns factors instead of numeric variables, with `labels` used
#'   for labelling the factor levels.
#' @param force Logical, if `TRUE`, forces recoding of factors as well.
#' @param append Logical or string. If `TRUE`, recoded variables get new
#'   column names (with the suffix `"_r"`) and are appended (column bind) to `x`,
#'   thus returning both the original and the recoded variables. If `FALSE`,
#'   original variables in `x` will be overwritten by their recoded versions.
#'   If a character value, recoded variables are appended with new column
#'   names (using the defined suffix) to the original data frame.
#' @param ... not used.
#' @inheritParams find_columns
#'
#' @return `x`, recoded into groups. By default `x` is numeric, unless `labels`
#'   is specified. In this case, a factor is returned, where the factor levels
#'   (i.e. recoded groups are labelled accordingly.
#'
#' @details
#'
#'   \subsection{Splits and breaks (cut-off values)}{
#'   Breaks are in general _exclusive_, this means that these values indicate
#'   the lower bound of the next group or interval to begin. Take a simple
#'   example, a numeric variable with values from 1 to 9. The median would be 5,
#'   thus the first interval ranges from 1-4 and is recoded into 1, while 5-9
#'   would turn into 2 (compare `cbind(1:9, data_recode(1:9))`). The same variable,
#'   using `split = "quantile"` and `n_groups = 3` would define breaks at 3.67
#'   and 6.33 (see `quantile(1:9, probs = c(1/3, 2/3)`), which means that values
#'   from 1 to 3 belong to the first interval and are recoded into 1 (because
#'   the next interval starts at 3.67), 4 to 6 into 2 and 7 to 9 into 3.
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
#' # into 3 groups, user-defined break
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
#'
#' # return factor with value labels instead of numeric value
#' set.seed(123)
#' x <- sample(1:10, size = 30, replace = TRUE)
#' data_recode(x, "equal_length", n_groups = 3)
#' data_recode(x, "equal_length", n_groups = 3, labels = c("low", "mid", "high"))
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
data_recode.numeric <- function(x, recodes = NULL, verbose = TRUE, ...) {

  lapply(recodes, function(i) {
    # name of list element is old value
    value_string <- names(i)

    # replace placeholder
    value_string <- gsub("min", min(x, na.rm = TRUE), value_string)
    value_string <- gsub("max", max(x, na.rm = TRUE), value_string)

    old_values <- tryCatch(eval(parse(text = value_string)), error = function(e) NULL)
    if (!is.null(old_values)) {

    }
  })

  # save
  original_x <- x

  # no missings
  x <- stats::na.omit(x)

  # stop if all NA
  if (!length(x)) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("Variable contains only missing values. No recoding carried out."), call. = FALSE)
    }
    return(original_x)
  }


  original_x
}


#' @export
data_recode.factor <- function(x, ...) {
  original_x <- x
  levels(x) <- 1:nlevels(x)
  out <- as.factor(data_recode(as.numeric(x), ...))
  .set_back_labels(out, original_x, include_values = FALSE)
}


#' @rdname data_recode
#' @export
data_recode.data.frame <- function(x,
                                split = "median",
                                n_groups = NULL,
                                range = NULL,
                                lowest = 1,
                                labels = NULL,
                                force = FALSE,
                                append = FALSE,
                                select = NULL,
                                exclude = NULL,
                                ignore_case = FALSE,
                                verbose = TRUE,
                                ...) {
  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)

  # process arguments
  args <- .process_std_args(x, select, exclude, weights = NULL, append, append_suffix = "_r", force)

  # update processed arguments
  x <- args$x
  select <- args$select

  x[select] <- lapply(x[select], data_recode, split = split, n_groups = n_groups, range = range, lowest = lowest, labels = labels, verbose = verbose, ...)
  x
}

