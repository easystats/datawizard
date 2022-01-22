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
  flag_empty <- FALSE

  # precaution to take for a tibble
  if (inherits(x, c("tbl_df", "tbl"))) x <- as.data.frame(x)

  if (inherits(x, "data.frame")) {
    x <- as.data.frame(x)
    if (nrow(x) > 0 && ncol(x) > 0) {
      x <- x[, !sapply(x, function(i) all(is.na(i))), drop = FALSE]
      # this is much faster than apply(x, 1, FUN)
      flag_empty <- all(rowSums(is.na(x)) == ncol(x))
    } else {
      flag_empty <- TRUE
    }
    # a list but not a data.frame
  } else if (is.list(x) && length(x) > 0) {
    x <- tryCatch(
      {
        compact_list(x)
      },
      error = function(x) {
        x
      }
    )
  } else if (!is.null(x)) {
    x <- stats::na.omit(x)
  }

  # need to check for is.null for R 3.4
  isTRUE(flag_empty) ||
    length(x) == 0 ||
    is.null(x) ||
    isTRUE(nrow(x) == 0) ||
    isTRUE(ncol(x) == 0)
}
