#' @title Expand (i.e. replicate rows) a data frame
#' @name data_replicate
#'
#' @description
#' Expand a data frame by replicating rows based on another variable that
#' contains the counts of replications per row.
#'
#' @param data A data frame.
#' @param expand The name of the column that contains the counts of replications
#' for each row. Can also be a numeric value, indicating the position of that
#' column. Note that the variable indicated by `expand` must be an integer vector.
#' @param remove_na Logical. If `TRUE`, missing values in the column
#' provided in `expand` are removed from the data frame. If `FALSE` and `expand`
#' contains missing values, the function will throw an error.
#' @param ... Currently not used.
#' @inheritParams extract_column_names
#'
#' @return A dataframe with each row replicated as many times as defined in `expand`.
#'
#' @examples
#' data(mtcars)
#' data_replicate(head(mtcars), "carb")
#' @export
data_replicate <- function(data,
                           expand = NULL,
                           select = NULL,
                           exclude = NULL,
                           remove_na = FALSE,
                           ignore_case = FALSE,
                           verbose = TRUE,
                           regex = FALSE,
                           ...) {
  # we need a name for the new column
  if (is.null(expand)) {
    insight::format_error(
      "No column that should be used to expand the data frame was provided. Please use `expand` to define a column."
    )
  }

  # only one column name
  if (length(expand) > 1) {
    insight::format_error(
      "Please provide only a single string for `expand`, no character vector with multiple values."
    )
  }

  # check if numerics, and if so, use column name
  if (is.numeric(expand)) {
    expand <- colnames(data)[expand]
  }

  # check if in data
  if (!expand %in% colnames(data)) {
    insight::format_error(
      "The column provided in `expand` does not exist in the data frame.",
      .misspelled_string(colnames(data), expand, "Possibly misspelled?")
    )
  }

  # check that "expand" contains no Inf
  if (any(is.infinite(data[[expand]]))) {
    insight::format_error(
      "The column provided in `expand` contains infinite values. Please provide a column that does not contain infinite values." # nolint
    )
  }

  # check that "expand" is integer
  if (!.is_integer(data[[expand]])) {
    insight::format_error(
      "The column provided in `expand` is not of type integer. Please provide a column that contains integer values." # nolint
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

  # extract variable that contains the counts of replicates
  replicates <- data[[expand]]
  # we can remove that column now
  data[[expand]] <- NULL

  # also remove "expand" from "select" string
  select <- setdiff(select, expand)

  # if user doesn't want to remove "NA", but replicates contain "NA",
  # give informative error here
  if (!remove_na && anyNA(replicates)) {
    insight::format_error(
      "The column provided in `expand` contains missing values, but `remove_na` is set to `FALSE`.",
      "Please set `remove_na` to `TRUE` or remove the missing values from the `expand` variable."
    )
  }

  # remove rows where "expand" is NA
  data <- data[!is.na(replicates), ]
  replicates <- replicates[!is.na(replicates)]

  # fin
  as.data.frame(do.call(cbind, lapply(data[select], rep.int, times = replicates)))
}


# is.integer(c(1, 2)) -> FALSE
# all(c(1, 2) %% 1 == 0) -> TRUE
.is_integer <- function(x, remove_na = TRUE) {
  if (remove_na) {
    x <- x[!is.na(x)]
  }
  tryCatch(
    all(x %% 1 == 0),
    warning = function(w) is.integer(x),
    error = function(e) FALSE
  )
}
