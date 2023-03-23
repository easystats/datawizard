#' @title Convert data to factors
#' @name to_factor
#'
#' @details
#' Convert variables or data into factors. If the data is labelled, value labels
#' will be used as factor levels. The counterpart to convert variables into
#' numeric is `to_numeric()`.
#'
#' @param x A data frame or vector.
#' @param labs_to_levels Logical, if `TRUE`, value labels are used as factor
#' levels after `x` was converted to factor. Else, factor levels are based on
#' the values of `x` (i.e. as if using `as.factor()`).
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

#' @rdname to_factor
#' @export
to_factor.numeric <- function(x, labs_to_levels = TRUE, verbose = TRUE, ...) {
  # preserve labels
  variable_label <- attr(x, "label", exact = TRUE)
  value_labels <- attr(x, "labels", exact = TRUE)

  # to factor
  x <- as.factor(x)

  # add back labels
  attr(x, "label") <- variable_label
  attr(x, "labels") <- value_labels

  # value labels to factor levels
  if (labs_to_levels) {
    x <- .value_labels_to_levels(x, verbose = verbose, ...)
  }
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



# helper -----------------------

## TODO: either remove or enable "remove_attr" argument

.value_labels_to_levels <- function(x, remove_attr = TRUE, verbose = TRUE, ...) {
  # extract value labels
  value_labels <- attr(x, "labels", exact = TRUE)
  # return, if none
  if (is.null(value_labels)) {
    return(x)
  }
  # check positions of matching values and levels
  levels_in_labs <- stats::na.omit(match(value_labels, levels(x)))
  labs_in_levels <- stats::na.omit(match(levels(x), value_labels))
  # sanity check - if labelled values and levels don't match
  if (!length(levels_in_labs) || !length(labs_in_levels)) {
    if (verbose) {
      insight::format_alert(
        "Could not use value labels as factor levels.",
        "Labelled values and factor levels had no match."
      )
    }
    return(x)
  }
  # check if all levels have matching labels, and if not, tell user
  if (verbose && nlevels(x) != length(levels_in_labs)) {
    insight::format_alert(
      "Not all factor levels had a matching value label. Non-matching levels were preserved."
    )
  }
  levels(x)[levels_in_labs] <- names(value_labels[labs_in_levels])
  if (remove_attr) {
    attr(x, "labels") <- NULL
  }
  x
}
