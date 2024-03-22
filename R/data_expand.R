#' @title Expand (i.e. replicate rows) a data frame
#' @name data_expand
#'
#' @description
#' Expand a data frame by replicating rows based on another variable that
#' contains the counts of replications per row.
#'
#' @param data A data frame.
#' @param expand The name of the column that contains the counts of replications
#' for each row.
#' @param remove_na Logical. If `TRUE`, missing values in the column
#' provided in `expand` are removed from the data frame. If `FALSE` and `expand`
#' contains missing values, the function will throw an error.
#' @param ... Currently not used.
#' @inheritParams find_columns
#'
#' @return `data`, with each row replicated as many times as defined in `expand`.
#'
#' @examples
#' data(mtcars)
#' data_expand(head(mtcars), "carb")
#' @export
data_expand <- function(data,
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

  # check if in data
  if (!expand %in% colnames(data)) {
    insight::format_error(
      "The column provided in `expand` does not exist in the data frame.",
      .misspelled_string(colnames(data), expand, "Possibly misspelled?")
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
      "Please set `remove_na` to `TRUE` or remove the missing values from the data frame."
    )
  }

  # remove rows where "expand" is NA
  data <- data[!is.na(replicates), ]
  replicates <- replicates[!is.na(replicates)]

  # fin
  as.data.frame(do.call(cbind, lapply(data[select], function(variable) {
    unlist(Map(rep, variable, replicates), use.names = FALSE)
  })))
}
