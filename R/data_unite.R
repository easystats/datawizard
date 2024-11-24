#' @title Unite ("merge") multiple variables
#' @name data_unite
#'
#' @description
#' Merge values of multiple variables per observation into one new variable.
#'
#' @param data A data frame.
#' @param new_column The name of the new column, as a string.
#' @param separator A character to use between values.
#' @param append Logical, if `FALSE` (default), removes original columns that
#' were united. If `TRUE`, all columns are preserved and the new column is
#' appended to the data frame.
#' @param remove_na Logical, if `TRUE`, missing values (`NA`) are not included
#' in the united values. If `FALSE`, missing values are represented as `"NA"`
#' in the united values.
#' @param ... Currently not used.
#' @inheritParams extract_column_names
#'
#' @seealso [`data_separate()`]
#'
#' @return `data`, with a newly created variable.
#'
#' @examples
#' d <- data.frame(
#'   x = 1:3,
#'   y = letters[1:3],
#'   z = 6:8
#' )
#' d
#' data_unite(d, new_column = "xyz")
#' data_unite(d, new_column = "xyz", remove = FALSE)
#' data_unite(d, new_column = "xyz", select = c("x", "z"))
#' data_unite(d, new_column = "xyz", select = c("x", "z"), append = TRUE)
#' @export
data_unite <- function(data,
                       new_column = NULL,
                       select = NULL,
                       exclude = NULL,
                       separator = "_",
                       append = FALSE,
                       remove_na = FALSE,
                       ignore_case = FALSE,
                       verbose = TRUE,
                       regex = FALSE,
                       ...) {
  # we need a name for the new column
  if (is.null(new_column)) {
    insight::format_error(
      "No name for the new column was provided.",
      "Please use `new_column` to define a name for the newly created column."
    )
  }

  # only one column name
  if (length(new_column) > 1) {
    insight::format_error(
      "Please provide only a single string for `new_column`, no character vector with multiple values."
    )
  }

  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    data,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  if (is.null(select) || length(select) <= 1) {
    insight::format_error(
      "At least two columns in `select` are required for `data_unite()`."
    )
  }

  # unite
  out <- data.frame(
    new_col = do.call(paste, c(data[select], sep = separator)),
    stringsAsFactors = FALSE
  )
  colnames(out) <- new_column

  # remove missings
  if (remove_na) {
    # remove trailing and leading "NA_" and "_NA"
    out[[new_column]] <- gsub(paste0("^NA", separator), "", out[[new_column]])
    out[[new_column]] <- gsub(paste0(separator, "NA$"), "", out[[new_column]])
    # remove _NA_ inside string, add separator back. This ensure we match
    # whole-word NA and do not break strings like "COUNTRY_NATION"
    out[[new_column]] <- gsub(paste0(separator, "NA", separator), separator, out[[new_column]], fixed = TRUE)
  }

  # remove old columns
  if (!isTRUE(append)) {
    data[select] <- NULL
  }

  # overwrite?
  if (new_column %in% colnames(data) && verbose) {
    insight::format_alert(
      "The name for `new_column` already exists as variable name in the data.",
      "This variable will be replaced by `new_column`."
    )
  }

  # overwrite or append
  data[[new_column]] <- out[[new_column]]

  # fin
  data
}
