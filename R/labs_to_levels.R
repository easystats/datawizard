#' @title Convert value labels into factor levels
#' @name labs_to_levels
#'
#' @details
#' Convert variables or data into factors and uses value labels as factor levels.
#' This function is similar to `to_factor()`, however, it is provides additional
#' options for factors with value labels attributes. While `to_factor()` returns
#' factors as is, `labs_to_levels()` always uses value labels as factor levels,
#' even for factors.
#'
#' @param x A data frame or vector.
#' @param remove_attr Logical, if `"labels"` attribute should be removed after
#' value labels are set as factor levels.
#' @param ... Arguments passed to or from other methods, in this in particular
#' case to `to_factor()`.
#' @inheritParams find_columns
#' @inheritParams categorize
#'
#' @return A factor, or a data frame of factors.
#'
#' @examples
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
  if (is.null(attr(x, "labels", exact = TRUE))) {
    insight::format_error("Could not change factor levels. Variable had no value labels.")
  }
  .value_labels_to_levels(x, remove_attr, verbose)
}

#' @export
labs_to_levels.numeric <- function(x, remove_attr = TRUE, verbose = TRUE, ...) {
  if (is.null(attr(x, "labels", exact = TRUE))) {
    insight::format_error(
      "Could not convert to factor and set value labels as factor level.",
      "Variable had no value labels."
    )
  }
  to_factor(x, labs_to_levels = TRUE, verbose = verbose, ...)
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
