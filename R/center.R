#' Centering (Grand-Mean Centering)
#'
#' Performs a grand-mean centering of data.
#'
#' @param x A (grouped) data frame, a (numeric or character) vector or a factor.
#' @param force Logical, if `TRUE`, forces centering of factors as
#'   well. Factors are converted to numerical values, with the lowest level
#'   being the value `1` (unless the factor has numeric levels, which are
#'   converted to the corresponding numeric value).
#' @param robust Logical, if `TRUE`, centering is done by subtracting the
#'   median from the variables. If `FALSE`, variables are centered by
#'   subtracting the mean.
#' @param append Logical or string. If `TRUE`, centered variables get new
#'   column names (with the suffix `"_c"`) and are appended (column bind) to `x`,
#'   thus returning both the original and the centered variables. If `FALSE`,
#'   original variables in `x` will be overwritten by their centered versions.
#'   If a character value, centered variables are appended with new column
#'   names (using the defined suffix) to the original data frame.
#' @param verbose Toggle warnings and messages.
#' @param weights Can be `NULL` (for no weighting), or:
#'   - For data frames: a numeric vector of weights, or a character of the
#'   name of a column in the `data.frame` that contains the weights.
#'   - For numeric vectors: a numeric vector of weights.
#' @param center Numeric value, which can be used as alternative to
#'   `reference` to define a reference centrality. If `center` is of length 1,
#'   it will be recycled to match the length of selected variables for centering.
#'   Else, `center` must be of same length as the number of selected variables.
#'   Values in `center` will be matched to selected variables in the provided
#'   order, unless a named vector is given. In this case, names are matched
#'   against the names of the selected variables.
#' @param ... Currently not used.
#' @inheritParams find_columns
#' @inheritParams standardize
#'
#' @section Selection of variables - the `select` argument:
#' For most functions that have a `select` argument (including this function),
#' the complete input data frame is returned, even when `select` only selects
#' a range of variables. That is, the function is only applied to those variables
#' that have a match in `select`, while all other variables remain unchanged.
#' In other words: for this function, `select` will not omit any non-included
#' variables, so that the returned data frame will include all variables
#' from the input data frame.
#'
#' @note
#' **Difference between centering and standardizing**: Standardized variables
#' are computed by subtracting the mean of the variable and then dividing it by
#' the standard deviation, while centering variables involves only the
#' subtraction.
#'
#' @seealso If centering within-clusters (instead of grand-mean centering)
#'   is required, see [demean()]. For standardizing, see [standardize()].
#'
#' @return The centered variables.
#'
#' @examples
#' data(iris)
#'
#' # entire dataframe or a vector
#' head(iris$Sepal.Width)
#' head(center(iris$Sepal.Width))
#' head(center(iris))
#' head(center(iris, force = TRUE))
#'
#' # only the selected columns from a dataframe
#' center(anscombe, select = c("x1", "x3"))
#' center(anscombe, exclude = c("x1", "x3"))
#'
#' # centering with reference center and scale
#' d <- data.frame(
#'   a = c(-2, -1, 0, 1, 2),
#'   b = c(3, 4, 5, 6, 7)
#' )
#'
#' # default centering at mean
#' center(d)
#'
#' # centering, using 0 as mean
#' center(d, center = 0)
#'
#' # centering, using -5 as mean
#' center(d, center = -5)
#' @export
center <- function(x, ...) {
  UseMethod("center")
}

#' @rdname center
#' @export
centre <- center


#' @export
center.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    message(insight::format_message(sprintf("Centering currently not possible for variables of class '%s'.", class(x)[1])))
  }
  x
}


#' @rdname center
#' @export
center.numeric <- function(x,
                           robust = FALSE,
                           weights = NULL,
                           reference = NULL,
                           center = NULL,
                           verbose = TRUE,
                           ...) {
  args <- .process_std_center(x, weights, robust, verbose, reference, center, scale = NULL)

  if (is.null(args)) { # all NA?
    return(x)
  } else if (is.null(args$check)) {
    vals <- rep(0, length(args$vals)) # If only unique value
  } else {
    vals <- as.vector(args$vals - args$center)
  }

  centered_x <- rep(NA, length(args$valid_x))
  centered_x[args$valid_x] <- vals
  attr(centered_x, "center") <- args$center
  attr(centered_x, "scale") <- args$scale
  attr(centered_x, "robust") <- robust
  # labels
  .set_back_labels(centered_x, x, include_values = FALSE)
}


#' @export
center.factor <- function(x,
                          robust = FALSE,
                          weights = NULL,
                          force = FALSE,
                          verbose = TRUE,
                          ...) {
  if (!force) {
    return(x)
  }
  center(.factor_to_numeric(x), weights = weights, robust = robust, verbose = verbose, ...)
}

#' @export
center.logical <- center.factor

#' @export
center.character <- center.factor

#' @export
center.Date <- center.factor

#' @export
center.AsIs <- center.numeric

#' @rdname center
#' @inheritParams standardize.data.frame
#' @export
center.data.frame <- function(x,
                              select = NULL,
                              exclude = NULL,
                              robust = FALSE,
                              weights = NULL,
                              reference = NULL,
                              center = NULL,
                              force = FALSE,
                              remove_na = c("none", "selected", "all"),
                              append = FALSE,
                              ignore_case = FALSE,
                              verbose = TRUE,
                              ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select, x, exclude, ignore_case, verbose = verbose)

  # process arguments
  args <- .process_std_args(x, select, exclude, weights, append,
    append_suffix = "_c", force, remove_na, reference,
    .center = center, .scale = NULL
  )

  # set new values
  x <- args$x

  for (var in args$select) {
    x[[var]] <- center(
      x[[var]],
      robust = robust,
      weights = args$weights,
      verbose = FALSE,
      reference = reference[[var]],
      center = args$center[var],
      force = force
    )
  }

  attr(x, "center") <- sapply(x[args$select], function(z) attributes(z)$center)
  attr(x, "scale") <- sapply(x[args$select], function(z) attributes(z)$scale)
  attr(x, "robust") <- robust
  x
}



#' @export
center.grouped_df <- function(x,
                              select = NULL,
                              exclude = NULL,
                              robust = FALSE,
                              weights = NULL,
                              reference = NULL,
                              center = NULL,
                              force = FALSE,
                              remove_na = c("none", "selected", "all"),
                              append = FALSE,
                              ignore_case = FALSE,
                              verbose = TRUE,
                              ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select, x, exclude, ignore_case, verbose = verbose)

  args <- .process_grouped_df(x, select, exclude, append,
    append_suffix = "_c",
    reference, weights, force
  )

  for (rows in args$grps) {
    args$x[rows, ] <- center(
      args$x[rows, , drop = FALSE],
      select = args$select,
      exclude = NULL,
      robust = robust,
      weights = args$weights,
      remove_na = remove_na,
      verbose = verbose,
      force = force,
      append = FALSE,
      center = center,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(args$x) <- args$info
  args$x
}
