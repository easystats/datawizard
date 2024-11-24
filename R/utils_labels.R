# after data transformation, label attributes get lost. This function
# extracts label attributes from the original vector and adds them back
# to the transformed vector

#' @keywords internal
.set_back_labels <- function(new, old, include_values = TRUE, reverse_values = FALSE) {
  # labelled data?
  attr(new, "label") <- attr(old, "label", exact = TRUE)
  value_labels <- attr(old, "labels", exact = TRUE)
  # "include_values" is used to preserve value labels
  if (isTRUE(include_values) && !is.null(value_labels)) {
    if (reverse_values) {
      # reverse values? Used for "reverse_scale()"
      attr(new, "labels") <- stats::setNames(rev(value_labels), names(value_labels))
    } else if (is.numeric(new)) {
      # keep value oder? Used for "to_numeric()"
      if (any(grepl("[^0-9]", value_labels))) {
        # if we have any non-numeric characters, convert to numeric
        attr(new, "labels") <- stats::setNames(as.numeric(as.factor(value_labels)), names(value_labels))
      } else {
        # if we have numeric, or "numeric character" (like "1", "2", "3" etc.)
        attr(new, "labels") <- stats::setNames(as.numeric(value_labels), names(value_labels))
      }
    } else {
      attr(new, "labels") <- stats::setNames(value_labels, names(value_labels))
    }
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
  # validation check - if labelled values and levels don't match
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
  # we need to find out which levels have no labelled value
  missing_levels <- levels(x)[!levels(x) %in% value_labels]

  # and we need to remove those value labels that don't have a matching level
  value_labels <- value_labels[value_labels %in% levels(x)]

  # for levels that have no label, we just keep the original factor level
  value_labels <- c(value_labels, stats::setNames(missing_levels, missing_levels))

  # now we can add back levels
  levels(x) <- names(value_labels)[order(as.numeric(value_labels))]
  attr(x, "labels") <- NULL

  x
}
