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


  # Warning if only one value
  if (insight::n_unique(x) == 1) {
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

  out <- as.vector((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)))

  if (!isTRUE(include_bounds) && (any(out == 0) || any(out == 1))) {
    if (isFALSE(include_bounds)) {
      out <- (out * (length(out) - 1) + 0.5) / length(out)
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

  attr(out, "include_bounds") <- isTRUE(include_bounds)
  attr(out, "min_value") <- min(x, na.rm = TRUE)
  attr(out, "range_difference") <- diff(range(x, na.rm = TRUE))

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

  info <- attributes(x)
  # works only for dplyr >= 0.8.0
  grps <- attr(x, "groups", exact = TRUE)[[".rows"]]

  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  x <- as.data.frame(x)
  for (rows in grps) {
    x[rows, ] <- normalize(
      x[rows, , drop = FALSE],
      select = select,
      exclude = exclude,
      include_bounds = include_bounds,
      verbose = verbose,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- info
  x
}


#' @rdname normalize
#' @export
normalize.data.frame <- function(x,
                                 select = NULL,
                                 exclude = NULL,
                                 include_bounds = TRUE,
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

  x[select] <- lapply(
    x[select],
    normalize,
    include_bounds = include_bounds,
    verbose = verbose
  )

  x
}


#' @export
normalize.matrix <- function(x, ...) {
  matrix(normalize(as.numeric(x), ...), nrow = nrow(x))
}
