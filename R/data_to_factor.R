#' @title Convert data to factors
#' @name data_to_factor
#'
#' @details
#' Convert data to numeric by converting characters to factors and factors to
#' either numeric levels or dummy variables. The "counterpart" to convert
#' variables into numeric is `data_to_numeric()`.
#'
#' @param x A data frame or vector.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams find_columns
#' @inheritParams data_cut
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @return A factor, or a data frame of factors.
#'
#' @examples
#' str(data_to_factor(iris))
#'
#' # use labels as levels
#' data(efc)
#' str(efc$c172code)
#' head(data_to_factor(efc$c172code))
#' @export
data_to_factor <- function(x, ...) {
  UseMethod("data_to_factor")
}

#' @export
data_to_factor.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    message(insight::format_message(sprintf("Converting into factors values currently not possible for variables of class '%s'.", class(x)[1])))
  }
  x
}

#' @export
data_to_factor.factor <- function(x, ...) {
  x
}

#' @export
data_to_factor.numeric <- function(x, ...) {
  # preserve labels
  variable_label <- attr(x, "label", exact = TRUE)
  value_labels <- attr(x, "labels", exact = TRUE)

  # make sure we have matching labels
  if (!is.null(value_labels)) {
    value_labels <- value_labels[value_labels %in% x]
  }

  # to factor
  x <- as.factor(x)

  # use value labels as levels
  if (!is.null(value_labels)) {
    try(levels(x) <- names(value_labels), silent = TRUE)
  }

  # add back variable label
  attr(x, "label") <- variable_label
  x
}

#' @export
data_to_factor.logical <- data_to_factor.numeric

#' @export
data_to_factor.character <- data_to_factor.numeric

#' @export
data_to_factor.Date <- data_to_factor.numeric

#' @rdname data_to_factor
#' @export
data_to_factor.data.frame <- function(x,
                                      select = NULL,
                                      exclude = NULL,
                                      ignore_case = FALSE,
                                      append = FALSE,
                                      verbose = TRUE,
                                      ...) {
  # sanity check, return as is for complete numeric
  if (all(sapply(x, is.factor))) {
    return(x)
  }

  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)

  # drop factors, when append is not FALSE
  if (!isFALSE(append)) {
    select <- colnames(x[select])[!sapply(x[select], is.factor)]
  }

  # process arguments
  args <- .process_std_args(
    x,
    select,
    exclude,
    weights = NULL,
    append,
    append_suffix = "_f",
    force = FALSE,
    preserve_value_labels = TRUE,
    keep_character = TRUE
  )

  # update processed arguments
  x <- args$x
  select <- args$select

  x[select] <- lapply(x[select], data_to_factor, verbose = verbose, ...)
  x
}
