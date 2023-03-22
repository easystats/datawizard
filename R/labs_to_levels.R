#' @title Convert data to factors
#' @name to_factor
#'
#' @details
#' Convert variables or data into factors. If the data is labelled, value labels
#' will be used as factor levels. The counterpart to convert variables into
#' numeric is `to_numeric()`.
#'
#' @param x A data frame or vector.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams find_columns
#' @inheritParams categorize
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @return A factor, or a data frame of factors.
#'
#' @examples
#' str(to_factor(iris))
#'
#' # use labels as levels
#' data(efc)
#' str(efc$c172code)
#' head(to_factor(efc$c172code))
#' @export
to_factor <- function(x, ...) {
  UseMethod("to_factor")
}


#' @export
to_factor.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert(
      sprintf("Converting into factors values currently not possible for variables of class `%s`.", class(x)[1])
    )
  }
  x
}

#' @export
to_factor.factor <- function(x, ...) {
  x
}

#' @export
to_factor.numeric <- function(x, ...) {
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
    try(levels(x) <- names(value_labels), silent = TRUE) # nolint
  }

  # add back variable label
  attr(x, "label") <- variable_label
  x
}

#' @export
to_factor.logical <- to_factor.numeric

#' @export
to_factor.character <- to_factor.numeric

#' @export
to_factor.Date <- to_factor.numeric

#' @rdname to_factor
#' @export
to_factor.data.frame <- function(x,
                                 select = NULL,
                                 exclude = NULL,
                                 ignore_case = FALSE,
                                 append = FALSE,
                                 regex = FALSE,
                                 verbose = TRUE,
                                 ...) {
  # sanity check, return as is for complete numeric
  if (all(vapply(x, is.factor, FUN.VALUE = logical(1L)))) {
    return(x)
  }

  # evaluate arguments
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  # drop factors, when append is not FALSE
  if (!isFALSE(append)) {
    select <- colnames(x[select])[!vapply(x[select], is.factor, FUN.VALUE = logical(1L))]
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

  x[select] <- lapply(x[select], to_factor, verbose = verbose, ...)
  x
}
