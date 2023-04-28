#' @title Convert value labels into factor levels
#' @name labels_to_levels
#'
#' @details
#' `labels_to_levels()` allows to use value labels of factors as their levels.
#'
#' @param x A data frame or factor. Other variable types (e.g. numerics) are not
#' allowed.
#' @param ... Currently not used.
#' @inheritParams find_columns
#' @inheritParams categorize
#'
#' @return `x`, where for all factors former levels are replaced by their value
#' labels.
#'
#' @examples
#' data(efc)
#' # create factor
#' x <- as.factor(efc$c172code)
#' # add value labels - these are not factor levels yet
#' x <- assign_labels(x, values = c(`1` = "low", `2` = "mid", `3` = "high"))
#' levels(x)
#' data_tabulate(x)
#'
#' x <- labels_to_levels(x)
#' levels(x)
#' data_tabulate(x)
#' @export
labels_to_levels <- function(x, ...) {
  UseMethod("labels_to_levels")
}


#' @export
labels_to_levels.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert("`labels_to_levels()` only works for factors.")
  }
  x
}

#' @rdname labels_to_levels
#' @export
labels_to_levels.factor <- function(x, verbose = TRUE, ...) {
  if (is.null(attr(x, "labels", exact = TRUE))) {
    insight::format_error("Could not change factor levels. Variable had no value labels.")
  }
  .value_labels_to_levels(x, verbose = verbose)
}

#' @rdname labels_to_levels
#' @export
labels_to_levels.data.frame <- function(x,
                                        select = NULL,
                                        exclude = NULL,
                                        ignore_case = FALSE,
                                        append = FALSE,
                                        regex = FALSE,
                                        verbose = TRUE,
                                        ...) {
  # sanity check, return as is for complete factor
  if (all(vapply(x, is.factor, TRUE))) {
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

  # keep only factors
  select <- colnames(x[select])[vapply(x[select], is.factor, TRUE)]

  # when we append variables, we call ".process_append()", which will
  # create the new variables and updates "select", so new variables are processed
  if (!isFALSE(append)) {
    # process arguments
    args <- .process_append(
      x,
      select,
      append,
      append_suffix = "_l",
      preserve_value_labels = TRUE,
      keep_factors = TRUE,
      keep_character = FALSE
    )
    # update processed arguments
    x <- args$x
    select <- args$select
  }

  x[select] <- lapply(
    x[select],
    labels_to_levels,
    verbose = verbose,
    ...
  )
  x
}
