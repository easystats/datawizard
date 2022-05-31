#' Standardization (Z-scoring)
#'
#' Performs a standardization of data (z-scoring), i.e., centering and scaling,
#' so that the data is expressed in terms of standard deviation (i.e., mean = 0,
#' SD = 1) or Median Absolute Deviance (median = 0, MAD = 1). When applied to a
#' statistical model, this function extracts the dataset, standardizes it, and
#' refits the model with this standardized version of the dataset. The
#' [normalize()] function can also be used to scale all numeric variables within
#' the 0 - 1 range.
#' \cr\cr
#' For model standardization, see [`standardize.default()`].
#'
#' @param x A (grouped) data frame, a vector or a statistical model (for
#'   `unstandardize()` cannot be a model).
#' @param robust Logical, if `TRUE`, centering is done by subtracting the
#'   median from the variables and dividing it by the median absolute deviation
#'   (MAD). If `FALSE`, variables are standardized by subtracting the
#'   mean and dividing it by the standard deviation (SD).
#' @param two_sd If `TRUE`, the variables are scaled by two times the deviation
#'   (SD or MAD depending on `robust`). This method can be useful to obtain
#'   model coefficients of continuous parameters comparable to coefficients
#'   related to binary predictors, when applied to **the predictors** (not the
#'   outcome) (Gelman, 2008).
#' @param weights Can be `NULL` (for no weighting), or:
#' - For model: if `TRUE` (default), a weighted-standardization is carried out.
#' - For `data.frame`s: a numeric vector of weights, or a character of the
#'   name of a column in the `data.frame` that contains the weights.
#' - For numeric vectors: a numeric vector of weights.
#' @param verbose Toggle warnings and messages on or off.
#' @param remove_na How should missing values (`NA`) be treated: if `"none"`
#'   (default): each column's standardization is done separately, ignoring
#'   `NA`s. Else, rows with `NA` in the columns selected with `select` /
#'   `exclude` (`"selected"`) or in all columns (`"all"`) are dropped before
#'   standardization, and the resulting data frame does not include these cases.
#' @param force Logical, if `TRUE`, forces standardization of factors and dates
#'   as well. Factors are converted to numerical values, with the lowest level
#'   being the value `1` (unless the factor has numeric levels, which are
#'   converted to the corresponding numeric value).
#' @param append Logical or string. If `TRUE`, standardized variables get new
#'   column names (with the suffix `"_z"`) and are appended (column bind) to `x`,
#'   thus returning both the original and the standardized variables. If `FALSE`,
#'   original variables in `x` will be overwritten by their standardized versions.
#'   If a character value, standardized variables are appended with new column
#'   names (using the defined suffix) to the original data frame.
#' @param reference A data frame or variable from which the centrality and
#'   deviation will be computed instead of from the input variable. Useful for
#'   standardizing a subset or new data according to another data frame.
#' @param center,scale
#' * For `standardize()`: \cr
#'   Numeric values, which can be used as alternative to `reference` to define
#'   a reference centrality and deviation. If `scale` and `center` are of
#'   length 1, they will be recycled to match the length of selected variables
#'   for standardization. Else, `center` and `scale` must be of same length as
#'   the number of selected variables. Values in `center` and `scale` will be
#'   matched to selected variables in the provided order, unless a named vector
#'   is given. In this case, names are matched against the names of the selected
#'   variables.
#'
#' * For `unstandardize()`: \cr
#'   `center` and `scale` correspond to the center (the mean / median) and the scale (SD / MAD) of
#'   the original non-standardized data (for data frames, should be named, or
#'   have column order correspond to the numeric column). However, one can also
#'   directly provide the original data through `reference`, from which the
#'   center and the scale will be computed (according to `robust` and `two_sd`).
#'   Alternatively, if the input contains the attributes `center` and `scale`
#'   (as does the output of `standardize()`), it will take it from there if the
#'   rest of the arguments are absent.
#' @param force Logical, if `TRUE`, forces recoding of factors and character
#'   vectors as well.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams find_columns
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @return The standardized object (either a standardize data frame or a
#'   statistical model fitted on standardized data).
#'
#' @note When `x` is a vector or a data frame with `remove_na = "none")`,
#'   missing values are preserved, so the return value has the same length /
#'   number of rows as the original input.
#'
#' @seealso See [center()] for grand-mean centering of variables.
#'
#' @family transform utilities
#' @family standardize
#'
#' @examples
#' d <- iris[1:4, ]
#'
#' # vectors
#' standardise(d$Petal.Length)
#'
#' # Data frames
#' # overwrite
#' standardise(d, select = c("Sepal.Length", "Sepal.Width"))
#'
#' # append
#' standardise(d, select = c("Sepal.Length", "Sepal.Width"), append = TRUE)
#'
#' # append, suffix
#' standardise(d, select = c("Sepal.Length", "Sepal.Width"), append = "_std")
#'
#' # standardizing with reference center and scale
#' d <- data.frame(
#'   a = c(-2, -1, 0, 1, 2),
#'   b = c(3, 4, 5, 6, 7)
#' )
#'
#' # default standardization, based on mean and sd of each variable
#' standardize(d) # means are 0 and 5, sd ~ 1.581139
#'
#' # standardization, based on mean and sd set to the same values
#' standardize(d, center = c(0, 5), scale = c(1.581, 1.581))
#'
#' # standardization, mean and sd for each variable newly defined
#' standardize(d, center = c(3, 4), scale = c(2, 4))
#'
#' # standardization, taking same mean and sd for each variable
#' standardize(d, center = 1, scale = 3)
#' @export
standardize <- function(x, ...) {
  UseMethod("standardize")
}

#' @rdname standardize
#' @export
standardise <- standardize


# Default method is in effectsize

# standardize.default <- function(x, verbose = TRUE, ...) {
#   if (isTRUE(verbose)) {
#     message(insight::format_message(sprintf("Standardizing currently not possible for variables of class '%s'.", class(x)[1])))
#   }
#   x
# }


#' @rdname standardize
#' @export
standardize.numeric <- function(x,
                                robust = FALSE,
                                two_sd = FALSE,
                                weights = NULL,
                                reference = NULL,
                                center = NULL,
                                scale = NULL,
                                verbose = TRUE,
                                ...) {
  args <- .process_std_center(x, weights, robust, verbose, reference, center, scale)

  # Perform standardization
  if (is.null(args)) { # all NA?
    return(x)
  } else if (is.null(args$check)) {
    vals <- rep(0, length(args$vals)) # If only unique value
  } else {
    if (two_sd) {
      vals <- as.vector((args$vals - args$center) / (2 * args$scale))
    } else {
      vals <- as.vector((args$vals - args$center) / args$scale)
    }
  }

  scaled_x <- rep(NA, length(args$valid_x))
  scaled_x[args$valid_x] <- vals
  attr(scaled_x, "center") <- args$center
  attr(scaled_x, "scale") <- args$scale
  attr(scaled_x, "robust") <- robust
  # labels
  .set_back_labels(scaled_x, x, include_values = FALSE)
}

#' @export
standardize.double <- standardize.numeric

#' @export
standardize.integer <- standardize.numeric

#' @export
standardize.matrix <- function(x, ...) {
  xl <- lapply(seq_len(ncol(x)), function(i) x[, i])

  xz <- lapply(xl, datawizard::standardize, ...)

  x_out <- do.call(cbind, xz)
  dimnames(x_out) <- dimnames(x)

  attr(x_out, "center") <- sapply(xz, attr, "center")
  attr(x_out, "scale") <- sapply(xz, attr, "scale")
  attr(x_out, "robust") <- sapply(xz, attr, "robust")[1]

  x_out
}


#' @rdname standardize
#' @export
standardize.factor <- function(x,
                               robust = FALSE,
                               two_sd = FALSE,
                               weights = NULL,
                               force = FALSE,
                               verbose = TRUE,
                               ...) {
  if (!force) {
    return(x)
  }

  standardize(.factor_to_numeric(x),
    robust = robust, two_sd = two_sd, weights = weights, verbose = verbose, ...
  )
}


#' @export
standardize.character <- standardize.factor

#' @export
standardize.logical <- standardize.factor

#' @export
standardize.Date <- standardize.factor

#' @export
standardize.AsIs <- standardize.numeric


# Data frames -------------------------------------------------------------


#' @rdname standardize
#' @export
standardize.data.frame <- function(x,
                                   select = NULL,
                                   exclude = NULL,
                                   robust = FALSE,
                                   two_sd = FALSE,
                                   weights = NULL,
                                   reference = NULL,
                                   center = NULL,
                                   scale = NULL,
                                   remove_na = c("none", "selected", "all"),
                                   force = FALSE,
                                   append = FALSE,
                                   ignore_case = FALSE,
                                   verbose = TRUE,
                                   ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select, x, exclude, ignore_case, verbose = verbose)

  # process arguments
  args <- .process_std_args(x, select, exclude, weights, append,
    append_suffix = "_z", force, remove_na, reference,
    .center = center, .scale = scale
  )

  # set new values
  x <- args$x

  # Loop through variables and standardize it
  for (var in args$select) {
    x[[var]] <- standardize(x[[var]],
      robust = robust,
      two_sd = two_sd,
      weights = args$weights,
      reference = reference[[var]],
      center = args$center[var],
      scale = args$scale[var],
      verbose = FALSE,
      force = force
    )
  }


  attr(x, "center") <- sapply(x[args$select], function(z) attributes(z)$center)
  attr(x, "scale") <- sapply(x[args$select], function(z) attributes(z)$scale)
  attr(x, "robust") <- robust
  x
}



#' @export
standardize.grouped_df <- function(x,
                                   select = NULL,
                                   exclude = NULL,
                                   robust = FALSE,
                                   two_sd = FALSE,
                                   weights = NULL,
                                   reference = NULL,
                                   center = NULL,
                                   scale = NULL,
                                   remove_na = c("none", "selected", "all"),
                                   force = FALSE,
                                   append = FALSE,
                                   ignore_case = FALSE,
                                   verbose = TRUE,
                                   ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select, x, exclude, ignore_case, verbose = verbose)

  args <- .process_grouped_df(x, select, exclude, append,
    append_suffix = "_z",
    reference, weights, force
  )

  for (rows in args$grps) {
    args$x[rows, ] <- standardize(
      args$x[rows, , drop = FALSE],
      select = args$select,
      exclude = NULL,
      robust = robust,
      two_sd = two_sd,
      weights = args$weights,
      remove_na = remove_na,
      verbose = verbose,
      force = force,
      append = FALSE,
      center = center,
      scale = scale,
      ...
    )
  }
  # set back class, so data frame still works with dplyr
  attributes(args$x) <- args$info
  args$x
}





# Datagrid ----------------------------------------------------------------

#' @export
standardize.datagrid <- function(x, ...) {
  x[names(x)] <- standardize(as.data.frame(x), reference = attributes(x)$data, ...)
  x
}

#' @export
standardize.visualisation_matrix <- standardize.datagrid
