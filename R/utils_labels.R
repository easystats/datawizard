# after data transformation, label attributes get lost. This function
# extracts label attributes from the original vector and adds them back
# to the transformed vector

#' @keywords internal
.set_back_labels <- function(new, old, include_values = TRUE) {
  # labelled data?
  attr(new, "label") <- attr(old, "label", exact = TRUE)
  labels <- attr(old, "labels", exact = TRUE)
  if (isTRUE(include_values) && !is.null(labels)) {
    attr(new, "labels") <- stats::setNames(rev(labels), names(labels))
  } else if (isFALSE(include_values)) {
    attr(new, "labels") <- NULL
  }
  new
}


# This functions converts value labels that are saved as attributes
# into factor levels

#' @keywords internal
.value_labels_to_levels <- function(x, verbose = TRUE, ...) {
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
  # when length of value_labels and levels_in_labs is identical, we can simply
  # replace the levels with the value labels. Else, we need to select only those
  # value labels that have a matching level (labs_in_levels)
  if (length(value_labels) == length(levels_in_labs)) {
    levels(x)[levels_in_labs] <- names(value_labels)
  } else {
    levels(x)[levels_in_labs] <- names(value_labels[labs_in_levels])
  }
  attr(x, "labels") <- NULL

  x
}
