#' Add a prefix or suffix to column names
#'
#' @rdname data_prefix_suffix
#' @inheritParams extract_column_names
#' @param pattern A character string, which will be added as prefix or suffix
#' to the column names.
#' @param ... Other arguments passed to or from other functions.
#'
#' @seealso
#' [data_rename()] for more fine-grained column renaming.
#' @examples
#' # Add prefix / suffix to all columns
#' head(data_addprefix(iris, "NEW_"))
#' head(data_addsuffix(iris, "_OLD"))
#'
#' @export
data_addprefix <- function(data,
                           pattern,
                           select = NULL,
                           exclude = NULL,
                           ignore_case = FALSE,
                           regex = FALSE,
                           verbose = TRUE,
                           ...) {
  # evaluate arguments
  select <- .select_nse(select,
    data,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  selected_columns <- colnames(data) %in% select
  colnames(data)[selected_columns] <- paste0(pattern, colnames(data)[selected_columns])
  data
}


#' @rdname data_prefix_suffix
#' @export
data_addsuffix <- function(data,
                           pattern,
                           select = NULL,
                           exclude = NULL,
                           ignore_case = FALSE,
                           regex = FALSE,
                           verbose = TRUE,
                           ...) {
  # evaluate arguments
  select <- .select_nse(select,
    data,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  selected_columns <- colnames(data) %in% select
  colnames(data)[selected_columns] <- paste0(colnames(data)[selected_columns], pattern)
  data
}
