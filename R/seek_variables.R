#' @title Find variables by its name, variable or value labels
#' @name seek_variables
#'
#' @description This functions seeks variables in a data frame, based on patterns
#' that either match the variable name (column name), variable labels, value labels
#' or factor levels. Matching variable and value labels only works for "labelled"
#' data, i.e. when the variables either have a `label` attribute or `labels`
#' attribute.
#'
#' `seek_variables()` is particular useful for larger data frames with labelled
#' data - finding the correct variable name can be a challenge. This function
#' helps to find the required variables, when only certain patterns of variable
#' names or labels are known.
#'
#' @param data A data frame.
#' @param pattern Character string (regular expression) to be matched in `data`.
#' May also be a character vector of length > 1. `pattern` is searched for in
#' column names, variable label and value labels attributes, or factor levels of
#' variables in `data`.
#' @param source Character vector, indicating where `pattern` is sought. Use one
#' of more of the following options:
#'
#' - `"name"`: searches in column names.
#' - `"labels"`: searches in variable labels. Only applies when a `label` attribute
#'   is set for a variable.
#' - `"values"`: searches in value labels or factor levels. Only applies when a
#'   `labels` attribute is set for a variable, or if a variable is a factor.
#' - `"all"`: searches in all of the above.
#' @param fuzzy Logical, if `TRUE`, "fuzzy matching" (partial and close distance
#' matching) will be used to find `pattern`.
#'
#' @return A data frame with three columns: the column index, the column name
#' and - if available - the variable label of all matched variables in `data`.
#'
#' @examples
#' # seek variables with "Length" in variable name or labels
#' seek_variables(iris, "Length")
#'
#' # seek variables with "dependency" in names or labels
#' # column "e42dep" has a label-attribute "elder's dependency"
#' data(efc)
#' seek_variables(efc, "dependency")
#'
#' # "female" only appears as value label attribute - default search is in
#' # variable names and labels only, so no match
#' seek_variables(efc, "female")
#' # when we seek in all sources, we find the variable "e16sex"
#' seek_variables(efc, "female", source = "all")
#'
#' # typo, no match
#' seek_variables(iris, "Lenght")
#' # typo, fuzzy match
#' seek_variables(iris, "Lenght", fuzzy = TRUE)
#' @export
seek_variables <- function(data, pattern, source = c("names", "labels"), fuzzy = FALSE) {
  # check valid args
  if (!is.data.frame(data)) {
    insight::format_error("`data` must be a data frame.")
  }

  # check valid args
  source <- intersect(source, c("names", "labels", "values", "levels", "column_names", "columns", "all"))
  if (is.null(source) || !length(source)) {
    insight::format_error("`source` must be one of \"names\", \"labels\", \"values\", a combination of these options, or \"all\".")
  }

  pos1 <- pos2 <- pos3 <- NULL

  pos <- unlist(lapply(pattern, function(search_pattern) {
    # search in variable names?
    if (any(source %in% c("names", "columns", "column_names", "all"))) {
      pos1 <- which(grepl(search_pattern, colnames(data)))
      # find in near distance?
      if (fuzzy) {
        pos1 <- c(pos1, .fuzzy_grep(x = colnames(data), pattern = search_pattern))
      }
    }

    # search in variable labels?
    if (any(source %in% c("labels", "all"))) {
      labels <- insight::compact_character(unlist(lapply(data, attr, which = "label", exact = TRUE)))
      if (!is.null(labels) && length(labels)) {
        found <- grepl(search_pattern, labels)
        pos2 <- match(names(labels)[found], colnames(data))
        # find in near distanc?
        if (fuzzy) {
          pos2 <- c(pos2, .fuzzy_grep(x = labels, pattern = search_pattern))
        }
      }
    }

    # search for pattern in value labels or levels?
    if (any(source %in% c("values", "levels", "all"))) {
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
          pos3 <- which(vapply(
            values,
            function(i) {
              p <- .fuzzy_grep(
                x = i,
                pattern = search_pattern
              )
              !insight::is_empty_object(p[1])
            },
            logical(1)
          ))
        }
      }
    }
    # get unique variable indices
    c(pos1, pos2, pos3)
  }))

  # clean up
  pos <- unique(pos)
  # remove -1
  pos <- pos[which(pos != -1)]

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

  data.frame(
    index = pos,
    column = colnames(data)[pos],
    labels = labels,
    stringsAsFactors = FALSE
  )
}
