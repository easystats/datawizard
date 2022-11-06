#' Generate a codebook of a data frame.
#'
#' `data_codebook()` does...
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @inheritParams standardize.data.frame
#' @inheritParams find_columns
#'
#' @return A data frame.
#'
#' @examples
#' data(iris)
#' data_codebook(iris, select = starts_with("Sepal"))
#'
#' data(efc)
#' data_codebook(efc)
#' @export
data_codebook <- function(data,
                          select = NULL,
                          exclude = NULL,
                          columns = NULL,
                          ignore_case = FALSE,
                          regex = FALSE,
                          verbose = TRUE,
                          ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    data,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

}