#' @title Recode (or "cut" / "bin") data into groups of values.
#' @name categorize
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
#' @param breaks Character, indicating whether breaks for categorizing data are
#'   `"inclusive"` (values indicate the _upper_ bound of the _previous_ group or
#'   interval) or `"exclusive"` (values indicate the _lower_ bound of the _next_
#'   group or interval to begin). Use `labels = "range"` to make this behaviour
#'   easier to see.
#' @param labels Character vector of value labels. If not `NULL`, `categorize()`
#'   will returns factors instead of numeric variables, with `labels` used
#'   for labelling the factor levels. Can also be `"mean"`, `"median"`,
#'   `"range"` or `"observed"` for a factor with labels as the mean/median,
#'   the requested range (even if not all values of that range are present in
#'   the data) or observed range (range of the actual recoded values) of each
#'   group. See 'Examples'.
#' @param append Logical or string. If `TRUE`, recoded or converted variables
#'   get new column names and are appended (column bind) to `x`, thus returning
#'   both the original and the recoded variables. The new columns get a suffix,
#'   based on the calling function: `"_r"` for recode functions, `"_n"` for
#'   `to_numeric()`, `"_f"` for `to_factor()`, or `"_s"` for
#'   `slide()`. If `append=FALSE`, original variables in `x` will be
#'   overwritten by their recoded versions. If a character value, recoded
#'   variables are appended with new column names (using the defined suffix) to
#'   the original data frame.
#' @param ... not used.
#' @inheritParams extract_column_names
#'
#' @inherit data_rename seealso
#'
#' @details
#'
#' # Splits and breaks (cut-off values)
#'
#' Breaks are by default _exclusive_, this means that these values indicate
#' the lower bound of the next group or interval to begin. Take a simple
#' example, a numeric variable with values from 1 to 9. The median would be 5,
#' thus the first interval ranges from 1-4 and is recoded into 1, while 5-9
#' would turn into 2 (compare `cbind(1:9, categorize(1:9))`). The same variable,
#' using `split = "quantile"` and `n_groups = 3` would define breaks at 3.67
#' and 6.33 (see `quantile(1:9, probs = c(1/3, 2/3))`), which means that values
#' from 1 to 3 belong to the first interval and are recoded into 1 (because
#' the next interval starts at 3.67), 4 to 6 into 2 and 7 to 9 into 3.
#'
#' The opposite behaviour can be achieved using `breaks = "inclusive"`, in which
#' case
#'
#' # Recoding into groups with equal size or range
#'
#' `split = "equal_length"` and `split = "equal_range"` try to divide the
#' range of `x` into intervals of similar (or same) length. The difference is
#' that `split = "equal_length"` will divide the range of `x` into `n_groups`
#' pieces and thereby defining the intervals used as breaks (hence, it is
#' equivalent to `cut(x, breaks = n_groups)`), while  `split = "equal_range"`
#' will cut `x` into intervals that all have the length of `range`, where the
#' first interval by defaults starts at `1`. The lowest (or starting) value
#' of that interval can be defined using the `lowest` argument.
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @return `x`, recoded into groups. By default `x` is numeric, unless `labels`
#'   is specified. In this case, a factor is returned, where the factor levels
#'   (i.e. recoded groups are labelled accordingly.
#'
#' @examples
#' set.seed(123)
#' x <- sample(1:10, size = 50, replace = TRUE)
#'
#' table(x)
#'
#' # by default, at median
#' table(categorize(x))
#'
#' # into 3 groups, based on distribution (quantiles)
#' table(categorize(x, split = "quantile", n_groups = 3))
#'
#' # into 3 groups, user-defined break
#' table(categorize(x, split = c(3, 5)))
#'
#' set.seed(123)
#' x <- sample(1:100, size = 500, replace = TRUE)
#'
#' # into 5 groups, try to recode into intervals of similar length,
#' # i.e. the range within groups is the same for all groups
#' table(categorize(x, split = "equal_length", n_groups = 5))
#'
#' # into 5 groups, try to return same range within groups
#' # i.e. 1-20, 21-40, 41-60, etc. Since the range of "x" is
#' # 1-100, and we have a range of 20, this results into 5
#' # groups, and thus is for this particular case identical
#' # to the previous result.
#' table(categorize(x, split = "equal_range", range = 20))
#'
#' # return factor with value labels instead of numeric value
#' set.seed(123)
#' x <- sample(1:10, size = 30, replace = TRUE)
#' categorize(x, "equal_length", n_groups = 3)
#' categorize(x, "equal_length", n_groups = 3, labels = c("low", "mid", "high"))
#'
#' # cut numeric into groups with the mean or median as a label name
#' x <- sample(1:10, size = 30, replace = TRUE)
#' categorize(x, "equal_length", n_groups = 3, labels = "mean")
#' categorize(x, "equal_length", n_groups = 3, labels = "median")
#'
#' # cut numeric into groups with the requested range as a label name
#' # each category has the same range, and labels indicate this range
#' categorize(mtcars$mpg, "equal_length", n_groups = 5, labels = "range")
#' # in this example, each category has the same range, but labels only refer
#' # to the ranges of the actual values (present in the data) inside each group
#' categorize(mtcars$mpg, "equal_length", n_groups = 5, labels = "observed")
#' @export
categorize <- function(x, ...) {
  UseMethod("categorize")
}

#' @export
categorize.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert(
      paste0("Variables of class `", class(x)[1], "` can't be recoded and remain unchanged.")
    )
  }
  return(x)
}


#' @rdname categorize
#' @export
categorize.numeric <- function(x,
                               split = "median",
                               n_groups = NULL,
                               range = NULL,
                               lowest = 1,
                               breaks = "exclusive",
                               labels = NULL,
                               verbose = TRUE,
                               ...) {
  # sanity check
  split <- .sanitize_split_arg(split, n_groups, range)

  # handle aliases
  if (identical(split, "equal_length")) split <- "length"
  if (identical(split, "equal_range")) split <- "range"

  # check for valid values
  breaks <- match.arg(breaks, c("exclusive", "inclusive"))

  # save
  original_x <- x

  # no missings
  x <- stats::na.omit(x)

  # stop if all NA
  if (!length(x)) {
    if (isTRUE(verbose)) {
      insight::format_alert(
        "Variable contains only missing values. No recoding carried out."
      )
    }
    return(original_x)
  }

  if (is.numeric(split)) {
    category_splits <- split
  } else {
    category_splits <- switch(split,
      median = stats::median(x),
      mean = mean(x),
      length = n_groups,
      quantile = stats::quantile(x, probs = seq_len(n_groups) / n_groups),
      range = .equal_range(x, range, n_groups, lowest),
      NULL
    )
  }

  # complete ranges, including minimum and maximum
  if (!identical(split, "length")) {
    category_splits <- unique(c(min(x), category_splits, max(x)))
  }

  # recode into groups
  out <- droplevels(cut(
    x,
    breaks = category_splits,
    include.lowest = TRUE,
    right = identical(breaks, "inclusive")
  ))
  cut_result <- out
  levels(out) <- 1:nlevels(out)

  # fix lowest value, add back into original vector
  out <- as.numeric(out)
  if (!is.null(lowest)) {
    out <- out - (min(out) - lowest)
  }
  original_x[!is.na(original_x)] <- out

  # turn into factor?
  .original_x_to_factor(original_x, x, cut_result, labels, out, verbose, ...)
}


#' @export
categorize.factor <- function(x, ...) {
  original_x <- x
  levels(x) <- 1:nlevels(x)
  out <- as.factor(categorize(as.numeric(x), ...))
  .set_back_labels(out, original_x, include_values = FALSE)
}


#' @rdname categorize
#' @export
categorize.data.frame <- function(x,
                                  select = NULL,
                                  exclude = NULL,
                                  split = "median",
                                  n_groups = NULL,
                                  range = NULL,
                                  lowest = 1,
                                  breaks = "exclusive",
                                  labels = NULL,
                                  append = FALSE,
                                  ignore_case = FALSE,
                                  regex = FALSE,
                                  verbose = TRUE,
                                  ...) {
  # evaluate arguments
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  # when we append variables, we call ".process_append()", which will
  # create the new variables and updates "select", so new variables are processed
  if (!isFALSE(append)) {
    # process arguments
    my_args <- .process_append(
      x,
      select,
      append,
      append_suffix = "_r"
    )
    # update processed arguments
    x <- my_args$x
    select <- my_args$select
  }

  x[select] <- lapply(
    x[select],
    categorize,
    split = split,
    n_groups = n_groups,
    range = range,
    lowest = lowest,
    breaks = breaks,
    labels = labels,
    verbose = verbose,
    ...
  )
  x
}


#' @export
categorize.grouped_df <- function(x,
                                  select = NULL,
                                  exclude = NULL,
                                  split = "median",
                                  n_groups = NULL,
                                  range = NULL,
                                  lowest = 1,
                                  breaks = "exclusive",
                                  labels = NULL,
                                  append = FALSE,
                                  ignore_case = FALSE,
                                  regex = FALSE,
                                  verbose = TRUE,
                                  ...) {
  grps <- attr(x, "groups", exact = TRUE)[[".rows"]]

  attr_data <- attributes(x)

  # evaluate arguments
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    remove_group_var = TRUE,
    verbose = verbose
  )

  # when we append variables, we call ".process_append()", which will
  # create the new variables and updates "select", so new variables are processed
  if (!isFALSE(append)) {
    # process arguments
    my_args <- .process_append(
      x,
      select,
      append,
      append_suffix = "_r"
    )
    # update processed arguments
    x <- my_args$x
    select <- my_args$select
  }

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- categorize(
      x[rows, , drop = FALSE],
      split = split,
      n_groups = n_groups,
      range = range,
      lowest = lowest,
      breaks = breaks,
      labels = labels,
      select = select,
      exclude = exclude,
      append = FALSE, # need to set to FALSE here, else variable will be doubled
      ignore_case = ignore_case,
      verbose = verbose,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  x <- .replace_attrs(x, attr_data)
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


.sanitize_split_arg <- function(split, n_groups, range) {
  # check arguments
  if (is.character(split)) {
    split <- match.arg(
      split,
      choices = c(
        "median", "mean", "quantile", "equal_length", "equal_range",
        "equal", "equal_distance", "range", "distance"
      )
    )
  }

  if (is.character(split) && split %in% c("quantile", "equal_length") && is.null(n_groups)) {
    insight::format_error(
      "Recoding based on quantiles or equal-sized groups requires the `n_groups` argument to be specified."
    )
  }

  if (is.character(split) && split == "equal_range" && is.null(n_groups) && is.null(range)) {
    insight::format_error(
      "Recoding into groups with equal range requires either the `range` or `n_groups` argument to be specified."
    )
  }

  split
}


.original_x_to_factor <- function(original_x, x, cut_result, labels, out, verbose, ...) {
  if (!is.null(labels)) {
    if (length(labels) == length(unique(out))) {
      original_x <- as.factor(original_x)
      levels(original_x) <- labels
    } else if (length(labels) == 1 && labels %in% c("mean", "median", "range", "observed")) {
      original_x <- as.factor(original_x)
      no_na_x <- original_x[!is.na(original_x)]
      out <- switch(labels,
        mean = stats::aggregate(x, list(no_na_x), FUN = mean, na.rm = TRUE)$x,
        median = stats::aggregate(x, list(no_na_x), FUN = stats::median, na.rm = TRUE)$x,
        # labels basically like what "cut()" returns
        range = levels(cut_result),
        # range based on the values that are actually present in the data
        {
          temp <- stats::aggregate(x, list(no_na_x), FUN = range, na.rm = TRUE)$x
          apply(temp, 1, function(i) paste0("(", paste(as.vector(i), collapse = "-"), ")"))
        }
      )
      levels(original_x) <- insight::format_value(out, ...)
    } else if (isTRUE(verbose)) {
      insight::format_warning(
        "Argument `labels` and levels of the recoded variable are not of the same length.",
        "Variable will not be converted to factor."
      )
    }
  }
  original_x
}
