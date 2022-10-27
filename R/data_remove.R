#' @inheritParams find_columns
#' @rdname data_relocate
#' @examples
#' # Remove columns
#' head(data_remove(iris, "Sepal.Length"))
#' head(data_remove(iris, starts_with("Sepal")))
#' @export
data_remove <- function(data,
                        select = NULL,
                        exclude = NULL,
                        ignore_case = FALSE,
                        regex = FALSE,
                        verbose = FALSE,
                        ...) {
  ## TODO set verbose = TRUE by default in a later update?

  # evaluate arguments
  select <- .select_nse(
    select,
    data,
    exclude = NULL,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose
  )

  # nothing to remove?
  if (!length(select)) {
    return(data)
  }

  out <- data[!colnames(data) %in% select]
  out <- .replace_attrs(out, attributes(data))

  out
}
