#' @rdname extract_column_names
#' @export
data_select <- function(data,
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
    allow_rename = TRUE,
    verbose = FALSE
  )

  # save attributes
  a <- attributes(data)

  if (!length(columns) || is.null(columns)) {
    if (isTRUE(verbose)) {
      insight::format_warning("No column names that matched the required search pattern were found.")
    }
    return(NULL)
  }

  out <- data[columns]

  # for named character vectors, we offer the service to directly rename the columns
  if (!is.null(names(columns))) {
    colnames(out) <- names(columns)
  }

  # add back attributes
  out <- .replace_attrs(out, a)
  out
}
