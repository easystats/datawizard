#' @title Shift numeric value range
#' @name data_shift
#'
#' @description
#' This functions shifts the value range of a numeric variable, so that the
#' new range starts at a given value.
#'
#' @param x A data frame or numeric vector.
#' @param verbose Toggle warnings.
#' @param ... not used.
#' @inheritParams data_to_numeric
#'
#' @return `x`, where the range of numeric variables starts at a new value.
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @inherit data_rename seealso
#'
#' @examples
#' # numeric
#' head(mtcars$gear)
#' head(data_shift(mtcars$gear))
#' head(data_shift(mtcars$gear, lowest = 10))
#'
#' # data frame
#' sapply(data_shift(mtcars, lowest = 1), min)
#' sapply(mtcars, min)
#' @export
data_shift <- function(x, ...) {
  UseMethod("data_shift")
}


#' @export
data_shift.default <- function(x, lowest = 0, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    message(insight::format_message(
      "Shifting non-numeric variables is not possible.",
      "Try using 'data_to_numeric()' and specify the 'lowest' argument."
    ))
  }
  x
}


#' @rdname data_shift
#' @export
data_shift.numeric <- function(x, lowest = 0, ...) {
  original_x <- x
  minval <- min(x, na.rm = TRUE)
  difference <- minval - lowest
  x <- x - difference
  .set_back_labels(x, original_x)
}


#' @rdname data_shift
#' @export
data_shift.data.frame <- function(x,
                                  select = NULL,
                                  exclude = NULL,
                                  lowest = 0,
                                  append = FALSE,
                                  ignore_case = FALSE,
                                  verbose = TRUE,
                                  ...) {
  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)

  # process arguments
  args <- .process_std_args(
    x,
    select,
    exclude,
    weights = NULL,
    append,
    append_suffix = "_s",
    force = FALSE
  )

  # update processed arguments
  x <- args$x
  select <- args$select

  x[select] <- lapply(
    x[select],
    data_shift,
    lowest = lowest,
    verbose = verbose,
    ...
  )

  x
}
