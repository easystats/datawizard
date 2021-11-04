#' Centering (Grand-Mean Centering)
#'
#' Performs a grand-mean centering of data.
#'
#' @param x A data frame, a (numeric or character) vector or a factor.
#' @param select Character vector of column names. If `NULL` (the default),
#'   all variables will be selected.
#' @param exclude Character vector of column names to be excluded from selection.
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
#' @param ... Currently not used.
#'
#' @note
#' **Difference between centering and standardizing**: Standardized variables
#' are computed by subtracting the mean of the variable and then dividing it by
#' the standard deviation, while centering variables involves only the
#' subtraction.
#'
#' @seealso If centering within-clusters (instead of grand-mean centering)
#'   is required, see [demean()].
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
#' @export
center <- function(x, ...) {
  UseMethod("center")
}

#' @rdname center
#' @export
centre <- center

#' @rdname center
#' @export
center.numeric <- function(x,
                           robust = FALSE,
                           weights = NULL,
                           verbose = TRUE,
                           ...) {

  args <- .process_std_center(x, weights, robust, verbose)

  if (is.null(args$check)) {
    vals <- rep(0, length(args$vals))  # If only unique value
  } else {
    vals <- as.vector(args$vals - args$center)
  }

  centered_x <- rep(NA, length(args$valid_x))
  centered_x[args$valid_x] <- vals
  attr(centered_x, "center") <- args$center
  attr(centered_x, "scale") <- args$scale
  attr(centered_x, "robust") <- robust
  centered_x
}


#' @export
center.factor <- function(x, weights = NULL, robust = FALSE, verbose = TRUE, ...) {
  center(.factor_to_numeric(x), weights = weights, robust = robust, verbose = verbose, ...)
}

#' @export
center.logical <- center.factor

#' @export
center.character <- center.factor


#' @rdname center
#' @inheritParams standardize.data.frame
#' @export
center.data.frame <- function(x,
                              robust = FALSE,
                              weights = NULL,
                              verbose = TRUE,
                              select = NULL,
                              exclude = NULL,
                              remove_na = c("none", "selected", "all"),
                              force = FALSE,
                              append = FALSE,
                              ...) {
  # process arguments
  args <- .process_std_args(x, select, exclude, weights, append,
                            append_suffix = "_c", force, remove_na)

  # set new values
  x <- args$x

  x[args$select] <- lapply(
    x[args$select],
    center,
    robust = robust,
    weights = args$weights,
    verbose = FALSE
  )

  attr(x, "center") <- sapply(x[args$select], function(z) attributes(z)$center)
  attr(x, "scale") <- sapply(x[args$select], function(z) attributes(z)$scale)
  attr(x, "robust") <- robust
  x
}
