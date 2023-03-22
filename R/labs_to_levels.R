#' @title Convert value labels into factor levels
#' @name labs_to_levels
#'
#' @details
#' Convert variables or data into factors and uses value labels as factor levels.
#' This function is similar to `to_factor()`, however, it is specially designed
#' for factors with value labels attributes. While `to_factor()` returns factors
#' as is, `labs_to_levels()` always uses value labels as factor levels, even
#' for factors.
#'
#' @param x A data frame or vector.
#' @param remove_attr Logical, if `"labels"` attribute should be removed after
#' value labels are set as factor levels.
#' @param ... Arguments passed to or from other methods, in this in particular
#' to `to_factor()`.
#' @inheritParams find_columns
#' @inheritParams categorize
#'
#' @return A factor, or a data frame of factors.
#'
#' @examples
#' str(labs_to_levels(iris))
#'
#' # use labels as levels
#' data(efc)
#' # create factor
#' x <- as.factor(efc$c172code)
#' # add value labels - these are not factor levels yet
#' x <- add_labs(x, values = c(`1` = "low", `2` = "mid", `3` = "high"))
#' levels(x)
#' data_tabulate(x)
#'
#' x <- labs_to_levels(x)
#' levels(x)
#' data_tabulate(x)
#' @export
labs_to_levels <- function(x, ...) {
  UseMethod("labs_to_levels")
}


#' @export
labs_to_levels.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert(
      sprintf("Converting into factors values currently not possible for variables of class `%s`.", class(x)[1])
    )
  }
  x
}

#' @rdname labs_to_levels
#' @export
labs_to_levels.factor <- function(x, remove_attr = TRUE, verbose = TRUE, ...) {
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
        "Labelled values and factor levels didn't macth."
      )
    }
    return(x)
  }
  levels(x)[levels_in_labs] <- names(value_labels[labs_in_levels])
  if (remove_attr) {
    attr(x, "labels") <- NULL
  }
  x
}

#' @export
labs_to_levels.numeric <- function(x, remove_attr = TRUE, verbose = TRUE, ...) {
  labs_to_levels(
    to_factor(x, verbose = verbose, ...),
    remove_attr = remove_attr,
    verbose = verbose,
    ...
  )
}

#' @export
labs_to_levels.logical <- labs_to_levels.numeric

#' @export
labs_to_levels.character <- labs_to_levels.numeric

#' @export
labs_to_levels.Date <- labs_to_levels.numeric

#' @rdname labs_to_levels
#' @export
labs_to_levels.data.frame <- function(x,
                                      select = NULL,
                                      exclude = NULL,
                                      ignore_case = FALSE,
                                      append = FALSE,
                                      regex = FALSE,
                                      remove_attr = TRUE,
                                      verbose = TRUE,
                                      ...) {
  # evaluate arguments
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  # process arguments
  args <- .process_std_args(
    x,
    select,
    exclude,
    weights = NULL,
    append,
    append_suffix = "_l",
    force = FALSE,
    preserve_value_labels = TRUE,
    keep_character = TRUE
  )

  # update processed arguments
  x <- args$x
  select <- args$select

  x[select] <- lapply(x[select], labs_to_levels, remove_attr = remove_attr, verbose = verbose, ...)
  x
}
