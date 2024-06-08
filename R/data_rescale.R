#' Rescale Variables to a New Range
#'
#' Rescale variables to a new range. Can also be used to reverse-score variables
#' (change the keying/scoring direction), or to expand a range.
#'
#' @inheritParams categorize
#' @inheritParams extract_column_names
#' @inheritParams standardize.data.frame
#'
#' @param to Numeric vector of length 2 giving the new range that the variable
#'   will have after rescaling. To reverse-score a variable, the range should
#'   be given with the maximum value first. See examples.
#' @param multiply If not `NULL`, `to` is ignored and `multiply` will be used,
#'   giving the factor by which the actual range of `x` should be expanded.
#'   For example, if a vector ranges from 5 to 15 and `multiply = 1.1`, the current
#'   range of 10 will be expanded by the factor of 1.1, giving a new range of
#'   11. Thus, the rescaled vector would range from 4.5 to 15.5.
#' @param add A vector of length 1 or 2. If not `NULL`, `to` is ignored and `add`
#'   will be used, giving the amount by which the minimum and maximum of the
#'   actual range of `x` should be expanded. For example, if a vector ranges from
#'   5 to 15 and `add = 1`, the range will be expanded from 4 to 16. If `add` is
#'   of length 2, then the first value is used for the lower bound and the second
#'   value for the upper bound.
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
#'
#' # "expand" ranges by a factor or a given value
#' x <- 5:15
#' x
#' # both will expand the range by 10%
#' rescale(x, multiply = 1.1)
#' rescale(x, add = 0.5)
#'
#' # expand range by different values
#' rescale(x, add = c(1, 3))
#'
#' # Specify list of multipliers
#' d <- data.frame(x = 5:15, y = 5:15)
#' rescale(d, multiply = list(x = 1.1, y = 0.5))
#'
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
                            multiply = NULL,
                            add = NULL,
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

  # check if user specified "multiply" or "add", and then update "to"
  to <- .update_to(x, to, multiply, add)

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

  out <- as.vector((new_max - new_min) / (max_value - min_value) * (x - min_value) + new_min)

  attr(out, "min_value") <- min_value
  attr(out, "max_value") <- max_value
  attr(out, "new_min") <- new_min
  attr(out, "new_max") <- new_max
  attr(out, "range_difference") <- max_value - min_value
  attr(out, "to_range") <- c(new_min, new_max)
  # don't add attribute when we call data frame methods
  if (!isFALSE(dot_args$add_transform_class)) {
    class(out) <- c("dw_transformer", class(out))
  }

  out
}


#' @export
rescale.grouped_df <- function(x,
                               select = NULL,
                               exclude = NULL,
                               to = c(0, 100),
                               multiply = NULL,
                               add = NULL,
                               range = NULL,
                               append = FALSE,
                               ignore_case = FALSE,
                               regex = FALSE,
                               verbose = FALSE,
                               ...) {
  info <- attributes(x)


  grps <- attr(x, "groups", exact = TRUE)[[".rows"]]

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
      append_suffix = "_r",
      preserve_value_labels = TRUE
    )
    # update processed arguments
    x <- my_args$x
    select <- my_args$select
  }

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- rescale(
      x[rows, , drop = FALSE],
      select = select,
      exclude = exclude,
      to = to,
      multiply = multiply,
      add = add,
      range = range,
      append = FALSE, # need to set to FALSE here, else variable will be doubled
      add_transform_class = FALSE,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- utils::modifyList(info, attributes(x))
  x
}



#' @rdname rescale
#' @export
rescale.data.frame <- function(x,
                               select = NULL,
                               exclude = NULL,
                               to = c(0, 100),
                               multiply = NULL,
                               add = NULL,
                               range = NULL,
                               append = FALSE,
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

  # Transform the range so that it is a list now
  if (!is.null(range) && !is.list(range)) {
    range <- stats::setNames(rep(list(range), length(select)), select)
  }
  # Transform the 'to' so that it is a list now
  if (!is.list(to)) {
    to <- stats::setNames(rep(list(to), length(select)), select)
  }
  # Transform the 'multiply' so that it is a list now
  if (!is.null(multiply) && !is.list(multiply)) {
    multiply <- stats::setNames(rep(list(multiply), length(select)), select)
  }
  # Transform the 'add' so that it is a list now
  if (!is.null(add) && !is.list(add)) {
    add <- stats::setNames(rep(list(add), length(select)), select)
  }
  # update "to" if user specified "multiply" or "add"
  to[] <- lapply(names(to), function(i) {
    .update_to(x[[i]], to[[i]], multiply[[i]], add[[i]])
  })

  x[select] <- as.data.frame(sapply(select, function(n) {
    rescale(x[[n]], to = to[[n]], range = range[[n]], add_transform_class = FALSE)
  }, simplify = FALSE))
  x
}


# helper ----------------------------------------------------------------------

# expand the new target range by multiplying or adding
.update_to <- function(x, to, multiply, add) {
  # check if user specified "multiply" or "add", and if not, return "to"
  if (is.null(multiply) && is.null(add)) {
    return(to)
  }
  # only one of "multiply" or "add" can be specified
  if (!is.null(multiply) && !is.null(add)) {
    insight::format_error("Only one of `multiply` or `add` can be specified.")
  }
  # multiply? If yes, calculate the "add" value
  if (!is.null(multiply)) {
    # check for correct length
    if (length(multiply) > 1) {
      insight::format_error("The length of `multiply` must be 1.")
    }
    add <- (diff(range(x, na.rm = TRUE)) * (multiply - 1)) / 2
  }
  # add?
  if (!is.null(add)) {
    # add must be of length 1 or 2
    if (length(add) > 2) {
      insight::format_error("The length of `add` must be 1 or 2.")
    }
    # if add is of length 2, then the first value is used for the lower bound
    # and the second value for the upper bound
    if (length(add) == 2) {
      add_low <- add[1]
      add_high <- add[2]
    } else {
      add_low <- add_high <- add
    }
    to <- c(min(x, na.rm = TRUE) - add_low, max(x, na.rm = TRUE) + add_high)
  }
  to
}
