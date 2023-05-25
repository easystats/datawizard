#' @title Unite ("merge") multiple variables
#' @name data_unite
#'
#' @description
#' Merges values of multiple variables per observation into one new variable.
#'
#' @param data A data frame.
#' @param new_column New column name.
#' @param ... Currently not used.
#' @inheritParams find_columns
#'
#' @inheritSection center Selection of variables - the `select` argument
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
#' @export
data_unite <- function(data,
                       new_column = NULL,
                       select = NULL,
                       exclude = NULL,
                       separator = "_",
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

  # overwrite?
  if (new_column %in% colnames(data) && verbose) {
    insight::format_alert(
      "The name for `new_column` already exists as variable name in the data.",
      "This variable will be replaced by `new_column`."
    )
  }

  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  # transpose and unite
  d <- data[select]
  out <- as.data.frame(do.call(rbind, lapply(data_transpose(d, verbose = FALSE), paste0, collapse = separator)))
  colnames(out) <- new_column

  # overwrite or append
  if (new_column %in% colnames(data)) {
    data[[new_column]] <- out[[new_column]]
  } else {
    data <- cbind(data, out)
  }

  # fin
  data
}
