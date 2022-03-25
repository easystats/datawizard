#' @rdname find_columns
#' @export
get_columns <- function(data,
                        select = NULL,
                        exclude = NULL,
                        ignore_case = FALSE,
                        regex = FALSE,
                        verbose = TRUE,
                        ...) {
  columns <- .select_nse(
    select,
    data,
    exclude,
    ignore_case = ignore_case,
    regex = regex,
    verbose = FALSE
  )

  # save attributes
  a <- attributes(data)

  if (!length(columns) || is.null(columns)) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("No column names that matched the required search pattern were found."), call. = FALSE)
    }
    return(NULL)
  }

  out <- data[columns]

  # add back attributes
  attributes(out) <- utils::modifyList(a, attributes(out))
  out
}


#' @rdname find_columns
#' @export
data_select <- get_columns
