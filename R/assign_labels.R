#' @title Assign variable and value labels
#' @name assign_labels
#'
#' @description
#' Assign variable and values labels to a variable or variables in a data frame.
#' Labels are stored as attributes (`"label"` for variable labels and `"labels"`)
#' for value labels.
#'
#' @param x A data frame, factor or vector.
#' @param variable The variable label as string.
#' @param values The value labels as (named) character vector. If `values` is
#' *not* a named vector, the length of labels must be equal to the length of
#' unique values. For a named vector, the left-hand side (LHS) is the value in
#' `x`, the right-hand side (RHS) the associated value label. Non-matching
#' labels are omitted.
#' @param ... Currently not used.
#' @inheritParams extract_column_names
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @return A labelled variable, or a data frame of labelled variables.
#'
#' @examples
#' x <- 1:3
#' # labelling by providing required number of labels
#' assign_labels(
#'   x,
#'   variable = "My x",
#'   values = c("one", "two", "three")
#' )
#'
#' # labelling using named vectors
#' data(iris)
#' out <- assign_labels(
#'   iris$Species,
#'   variable = "Labelled Species",
#'   values = c(`setosa` = "Spec1", `versicolor` = "Spec2", `virginica` = "Spec3")
#' )
#' str(out)
#'
#' # data frame example
#' out <- assign_labels(
#'   iris,
#'   select = "Species",
#'   variable = "Labelled Species",
#'   values = c(`setosa` = "Spec1", `versicolor` = "Spec2", `virginica` = "Spec3")
#' )
#' str(out$Species)
#'
#' # Partial labelling
#' x <- 1:5
#' assign_labels(
#'   x,
#'   variable = "My x",
#'   values = c(`1` = "lowest", `5` = "highest")
#' )
#' @export
assign_labels <- function(x, ...) {
  UseMethod("assign_labels")
}


#' @export
assign_labels.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert(
      sprintf("Adding labels currently not possible for variables of class `%s`.", class(x)[1])
    )
  }
  x
}

#' @rdname assign_labels
#' @export
assign_labels.numeric <- function(x, variable = NULL, values = NULL, ...) {
  # add variable label
  if (!is.null(variable)) {
    if (is.character(variable) && length(variable) == 1L) {
      attr(x, "label") <- variable
    } else {
      insight::format_error(
        "Variable labels (argument `variable`) must be provided as a single character string, e.g. `variable = \"mylabel\"`." # nolint
      )
    }
  }

  # if user just wants to add a variable label, skip next steps
  if (!is.null(values)) {
    # extract unique values
    unique_values <- as.vector(sort(stats::na.omit(unique(x))))
    value_labels <- NULL

    # do we have a names vector for "values"?
    # else check if number of labels and values match
    if (is.null(names(values))) {
      if (length(values) == length(unique_values)) {
        value_labels <- stats::setNames(unique_values, values)
      } else {
        insight::format_error(
          "Cannot add labels. Number of unique values and number of value labels are not equal.",
          sprintf("There are %i unique values and %i provided labels.", length(unique_values), length(values))
        )
      }
    } else {
      # check whether we have matches of labels and values
      matching_labels <- names(values) %in% unique_values
      if (!all(matching_labels)) {
        insight::format_error(
          "Following labels were associated with values that don't exist:",
          text_concatenate(paste0(values[!matching_labels], " (", names(values)[!matching_labels], ")"), enclose = "`")
        )
      }
      values <- values[names(values) %in% unique_values]

      if (length(values)) {
        # we need to switch names and values
        value_labels <- stats::setNames(coerce_to_numeric(names(values)), values)
      }
    }

    attr(x, "labels") <- value_labels
  }

  x
}

#' @export
assign_labels.factor <- assign_labels.numeric

#' @export
assign_labels.character <- assign_labels.numeric

#' @rdname assign_labels
#' @export
assign_labels.data.frame <- function(x,
                                     select = NULL,
                                     exclude = NULL,
                                     values = NULL,
                                     ignore_case = FALSE,
                                     regex = FALSE,
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

  x[select] <- lapply(x[select], assign_labels, values = values, verbose = verbose, ...)
  x
}
