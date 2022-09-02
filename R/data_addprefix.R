#' @rdname data_rename
#' @inheritParams find_columns
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


#' @rdname data_rename
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
