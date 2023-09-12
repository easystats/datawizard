#' Normalize numeric variable to 0-1 range
#'
#' Performs a normalization of data, i.e., it scales variables in the range
#' 0 - 1. This is a special case of [rescale()]. `unnormalize()` is the
#' counterpart, but only works for variables that have been normalized with
#' `normalize()`.
#'
#' @param x A numeric vector, (grouped) data frame, or matrix. See 'Details'.
#' @param include_bounds Numeric or logical. Using this can be useful in case of
#'   beta-regression, where the response variable is not allowed to include
#'   zeros and ones. If `TRUE`, the input is normalized to a range that includes
#'   zero and one. If `FALSE`, the return value is compressed, using
#'   Smithson and Verkuilen's (2006) formula `(x * (n - 1) + 0.5) / n`, to avoid
#'   zeros and ones in the normalized variables. Else, if numeric (e.g., `0.001`),
#'   `include_bounds` defines the "distance" to the lower and upper bound, i.e.
#'   the normalized vectors are rescaled to a range from `0 + include_bounds` to
#'   `1 - include_bounds`.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams standardize.data.frame
#' @inheritParams find_columns
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @details
#'
#' - If `x` is a matrix, normalization is performed across all values (not
#'   column- or row-wise). For column-wise normalization, convert the matrix to a
#'   data.frame.
#' - If `x` is a grouped data frame (`grouped_df`), normalization is performed
#'   separately for each group.
#'
#' @seealso See [makepredictcall.dw_transformer()] for use in model formulas.
#'
#' @examples
#'
#' normalize(c(0, 1, 5, -5, -2))
#' normalize(c(0, 1, 5, -5, -2), include_bounds = FALSE)
#' # use a value defining the bounds
#' normalize(c(0, 1, 5, -5, -2), include_bounds = .001)
#'
#' head(normalize(trees))
#'
#' @references
#'
#' Smithson M, Verkuilen J (2006). A Better Lemon Squeezer? Maximum-Likelihood
#' Regression with Beta-Distributed Dependent Variables. Psychological Methods,
#' 11(1), 54–71.
#'
#' @family transform utilities
#'
#' @return A normalized object.
#'
#' @export
normalize <- function(x, ...) {
  UseMethod("normalize")
}



#' @rdname normalize
#' @export
normalize.numeric <- function(x, include_bounds = TRUE, verbose = TRUE, ...) {
  # Warning if all NaNs or infinite
  if (all(is.infinite(x) | is.na(x))) {
    return(x)
  }

  # safe name, for later use
  if (is.null(names(x))) {
    name <- insight::safe_deparse(substitute(x))
  } else {
    name <- names(x)
  }

  # Get infinite and replace by NA (so that the normalization doesn't fail)
  infinite_idx <- is.infinite(x)
  infinite_vals <- x[infinite_idx]
  x[infinite_idx] <- NA


  # called from "makepredictcal()"? Then we have additional arguments
  dot_args <- list(...)
  flag_predict <- FALSE
  required_dot_args <- c(
    "range_difference", "min_value", "vector_length",
    "flag_bounds"
  )

  if (all(required_dot_args %in% names(dot_args))) {
    # we gather informatiom about the original data, which is needed
    # for "predict()" to work properly when "normalize()" is called
    # in formulas on-the-fly, e.g. "lm(mpg ~ normalize(hp), data = mtcars)"
    range_difference <- dot_args$range_difference
    min_value <- dot_args$min_value
    vector_length <- dot_args$vector_length
    flag_bounds <- dot_args$flag_bounds
    flag_predict <- TRUE
  } else {
    range_difference <- diff(range(x, na.rm = TRUE))
    min_value <- min(x, na.rm = TRUE)
    vector_length <- length(x)
    flag_bounds <- NULL
  }


  # Warning if only one value
  if (!flag_predict && insight::has_single_value(x)) {
    if (verbose) {
      insight::format_warning(
        paste0(
          "Variable `",
          name,
          "` contains only one unique value and will not be normalized."
        )
      )
    }
    return(x)
  }


  # Warning if logical vector
  if (insight::n_unique(x) == 2 && verbose) {
    insight::format_warning(
      paste0(
        "Variable `",
        name,
        "` contains only two unique values. Consider converting it to a factor."
      )
    )
  }

  # rescale
  out <- as.vector((x - min_value) / range_difference)

  # if we don't have information on whether bounds are included or not,
  # get this information here.
  if (is.null(flag_bounds)) {
    flag_bounds <- (any(out == 0) || any(out == 1))
  }

  if (!isTRUE(include_bounds) && flag_bounds) {
    if (isFALSE(include_bounds)) {
      out <- (out * (vector_length - 1) + 0.5) / vector_length
    } else if (is.numeric(include_bounds) && include_bounds > 0 && include_bounds < 1) {
      out <- rescale(out, to = c(0 + include_bounds, 1 - include_bounds))
    } else if (verbose) {
      insight::format_warning(
        "`include_bounds` must be either logical or numeric (between 0 and 1).",
        "Bounds (zeros and ones) are included in the returned value."
      )
    }
  }

  # Re-insert infinite values
  out[infinite_idx] <- infinite_vals

  attr(out, "include_bounds") <- include_bounds
  attr(out, "flag_bounds") <- isTRUE(flag_bounds)
  attr(out, "min_value") <- min_value
  attr(out, "vector_length") <- vector_length
  attr(out, "range_difference") <- range_difference
  # don't add attribute when we call data frame methods
  if (!isFALSE(dot_args$add_transform_class)) {
    class(out) <- c("dw_transformer", class(out))
  }

  out
}


#' @export
normalize.factor <- function(x, ...) {
  x
}


#' @export
normalize.grouped_df <- function(x,
                                 select = NULL,
                                 exclude = NULL,
                                 include_bounds = TRUE,
                                 append = FALSE,
                                 ignore_case = FALSE,
                                 regex = FALSE,
                                 verbose = TRUE,
                                 ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    remove_group_var = TRUE,
    verbose = verbose
  )

  info <- attributes(x)
  # works only for dplyr >= 0.8.0
  grps <- attr(x, "groups", exact = TRUE)[[".rows"]]

  # when we append variables, we call ".process_append()", which will
  # create the new variables and updates "select", so new variables are processed
  if (!isFALSE(append)) {
    # process arguments
    args <- .process_append(
      x,
      select,
      append,
      append_suffix = "_n"
    )
    # update processed arguments
    x <- args$x
    select <- args$select
  }

  x <- as.data.frame(x)

  # create column(s) to store dw_transformer attributes
  for (i in select) {
    info$groups[[paste0("attr_", i)]] <- rep(NA, length(grps))
  }

  for (rows in seq_along(grps)) {
    tmp <- normalize(
      x[grps[[rows]], , drop = FALSE],
      select = select,
      exclude = exclude,
      include_bounds = include_bounds,
      verbose = verbose,
      append = FALSE, # need to set to FALSE here, else variable will be doubled
      add_transform_class = FALSE,
      ...
    )

    # store dw_transformer_attributes
    for (i in select) {
      info$groups[rows, paste0("attr_", i)][[1]] <- list(unlist(attributes(tmp[[i]])))
    }

    x[grps[[rows]], ] <- tmp
  }

  # last column of "groups" attributes must be called ".rows"
  info$groups <- data_relocate(info$groups, ".rows", after = -1)

  # set back class, so data frame still works with dplyr
  attributes(x) <- utils::modifyList(info, attributes(x))
  class(x) <- c("grouped_df", class(x))
  x
}


#' @rdname normalize
#' @export
normalize.data.frame <- function(x,
                                 select = NULL,
                                 exclude = NULL,
                                 include_bounds = TRUE,
                                 append = FALSE,
                                 ignore_case = FALSE,
                                 regex = FALSE,
                                 verbose = TRUE,
                                 ...) {
  # evaluate select/exclude, may be select-helpers
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
    args <- .process_append(
      x,
      select,
      append,
      append_suffix = "_n"
    )
    # update processed arguments
    x <- args$x
    select <- args$select
  }

  x[select] <- lapply(
    x[select],
    normalize,
    include_bounds = include_bounds,
    verbose = verbose,
    add_transform_class = FALSE
  )

  x
}


#' @export
normalize.matrix <- function(x, ...) {
  matrix(normalize(as.numeric(x), ...), nrow = nrow(x))
}
