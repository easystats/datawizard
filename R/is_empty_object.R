#' Check if object is empty
#'
#' @param x A list, a vector, or a dataframe.
#'
#' @return A logical indicating whether the entered object is empty.
#'
#' @examples
#' is_empty_object(c(1, 2, 3, NA))
#' is_empty_object(list(NULL, c(NA, NA)))
#' is_empty_object(list(NULL, NA))
#' @export

is_empty_object <- function(x) {
  if (is.list(x)) {
    x <- tryCatch(
      {
        compact_list(x)
      },
      error = function(x) {
        x
      }
    )
  }

  if (inherits(x, c("tbl_df", "tbl"))) x <- as.data.frame(x)
  x <- suppressWarnings(x[!is.na(x)])
  length(x) == 0 || is.null(x)
}
