#' @title Find variables by their names, variable or value labels
#' @name data_seek
#'
#' @description This functions seeks variables in a data frame, based on patterns
#' that either match the variable name (column name), variable labels, value labels
#' or factor levels. Matching variable and value labels only works for "labelled"
#' data, i.e. when the variables either have a `label` attribute or `labels`
#' attribute.
#'
#' `data_seek()` is particular useful for larger data frames with labelled
#' data - finding the correct variable name can be a challenge. This function
#' helps to find the required variables, when only certain patterns of variable
#' names or labels are known.
#'
#' @param data A data frame.
#' @param pattern Character string (regular expression) to be matched in `data`.
#' May also be a character vector of length > 1. `pattern` is searched for in
#' column names, variable label and value labels attributes, or factor levels of
#' variables in `data`.
#' @param seek Character vector, indicating where `pattern` is sought. Use one
#' or more of the following options:
#'
#' - `"names"`: Searches in column names. `"column_names"` and `"columns"` are
#'   aliases for `"names"`.
#' - `"labels"`: Searches in variable labels. Only applies when a `label` attribute
#'   is set for a variable.
#' - `"values"`: Searches in value labels or factor levels. Only applies when a
#'   `labels` attribute is set for a variable, or if a variable is a factor.
#'   `"levels"` is an alias for `"values"`.
#' - `"all"`: Searches in all of the above.
#' @param fuzzy Logical. If `TRUE`, "fuzzy matching" (partial and close distance
#' matching) will be used to find `pattern`.
#'
#' @return A data frame with three columns: the column index, the column name
#' and - if available - the variable label of all matched variables in `data`.
#'
#' @examples
#' # seek variables with "Length" in variable name or labels
#' data_seek(iris, "Length")
#'
#' # seek variables with "dependency" in names or labels
#' # column "e42dep" has a label-attribute "elder's dependency"
#' data(efc)
#' data_seek(efc, "dependency")
#'
#' # "female" only appears as value label attribute - default search is in
#' # variable names and labels only, so no match
#' data_seek(efc, "female")
#' # when we seek in all sources, we find the variable "e16sex"
#' data_seek(efc, "female", seek = "all")
#'
#' # typo, no match
#' data_seek(iris, "Lenght")
#' # typo, fuzzy match
#' data_seek(iris, "Lenght", fuzzy = TRUE)
#' @export
data_seek <- function(data, pattern, seek = c("names", "labels"), fuzzy = FALSE) {
  # check valid args
  if (!is.data.frame(data)) {
    insight::format_error("`data` must be a data frame.")
  }

  # check valid args
  seek <- intersect(seek, c("names", "labels", "values", "levels", "column_names", "columns", "all"))
  if (is.null(seek) || !length(seek)) {
    insight::format_error("`seek` must be one of \"names\", \"labels\", \"values\", a combination of these options, or \"all\".")
  }

  pos1 <- pos2 <- pos3 <- NULL

  pos <- unlist(lapply(pattern, function(search_pattern) {
    # search in variable names?
    if (any(seek %in% c("names", "columns", "column_names", "all"))) {
      pos1 <- which(grepl(search_pattern, colnames(data)))
      # find in near distance?
      if (fuzzy) {
        pos1 <- c(pos1, .fuzzy_grep(x = colnames(data), pattern = search_pattern))
      }
    }

    # search in variable labels?
    if (any(seek %in% c("labels", "all"))) {
      labels <- insight::compact_character(unlist(lapply(data, attr, which = "label", exact = TRUE)))
      if (!is.null(labels) && length(labels)) {
        found <- grepl(search_pattern, labels)
        pos2 <- match(names(labels)[found], colnames(data))
        # find in near distanc?
        if (fuzzy) {
          found <- .fuzzy_grep(x = labels, pattern = search_pattern)
          if (length(found)) {
            pos2 <- c(pos2, match(names(labels)[found], colnames(data)))
          }
        }
      }
    }

    # search for pattern in value labels or levels?
    if (any(seek %in% c("values", "levels", "all"))) {
      values <- insight::compact_list(lapply(data, function(i) {
        l <- attr(i, "labels", exact = TRUE)
        if (is.null(l) && is.factor(i)) {
          levels(i)
        } else {
          names(l)
        }
      }))
      if (!is.null(values) && length(values)) {
        found <- vapply(values, function(i) any(grepl(search_pattern, i)), logical(1))
        pos3 <- match(names(found)[found], colnames(data))
        # find in near distance
        if (fuzzy) {
          found <- vapply(
            values,
            function(i) {
              length(.fuzzy_grep(x = i, pattern = search_pattern)) > 0
            },
            logical(1)
          )
          if (any(found)) {
            pos3 <- c(pos3, match(names(found)[found], colnames(data)))
          }
        }
      }
    }
    c(pos1, pos2, pos3)
  }))

  # clean up
  pos <- unique(pos)

  # variable labels of matching variables
  labels <- vapply(
    colnames(data[pos]),
    function(i) {
      l <- attr(data[[i]], "label", exact = TRUE)
      if (is.null(l)) {
        i
      } else {
        l
      }
    },
    character(1)
  )

  out <- data.frame(
    index = pos,
    column = colnames(data)[pos],
    labels = labels,
    stringsAsFactors = FALSE
  )
  # no row names
  rownames(out) <- NULL

  class(out) <- c("data_seek", "data.frame")
  out
}


# methods ---------------------------------------------------------------------

#' @export
print.data_seek <- function(x, ...) {
  if (nrow(x) == 0) {
    cat("No matches found.\n")
  } else {
    cat(insight::export_table(x, ...))
  }
}
