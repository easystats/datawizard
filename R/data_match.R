#' Find row indices of a data frame matching a specific condition
#'
#' Find row indices of a data frame that match a specific condition.
#'
#' @param x A data frame.
#' @param to A data frame matching the specified conditions, or a logical
#'   expression indicating which rows to keep.
#' @param match String, indicating with which logical operation matching
#'   conditions should be combined. Can be `"and"` (or `"&"`), `"or"` (or `"|"`)
#'   or `"not"` (or `"!"`).
#' @param return_indices Logical, if `FALSE`, return the vector of rows that can be used to filter the original data frame. If `FALSE` (default), returns directly the filtered data frame
#'   instead of the row indices.
#' @param ... Not used.
#'
#' @return The row indices that match the specified configuration.
#'
#' @examples
#' data_match(mtcars, data.frame(vs = 0, am = 1))
#' data_match(mtcars, data.frame(vs = 0, am = c(0, 1)))
#'
#' # observations where "vs" is NOT 0 AND "am" is NOT 1
#' data_match(mtcars, data.frame(vs = 0, am = 1), match = "not")
#' # equivalent to
#' data_match(mtcars, vs != 0 & am != 1)
#'
#' # observations where EITHER "vs" is 0 OR "am" is 1
#' data_match(mtcars, data.frame(vs = 0, am = 1), match = "or")
#' # equivalent to
#' data_match(mtcars, vs == 0 | am == 1)
#'
#' @inherit data_rename seealso
#' @export
data_match <- function(x, to, match = "and", return_indices = FALSE, ...) {

  # Input checks
  is_data_frame <- try(is.data.frame(to), silent = TRUE)

  # if is.data.frame() errors, we may have a logical condition here that
  # we can use for subset(), skipping all the remaining part of the function
  if (inherits(is_data_frame, "try-error")) {
    condition <- substitute(to)
    out <- do.call(subset, list(x, subset = condition))
    # restore value and variable labels
    for (i in colnames(out)) {
      attr(out[[i]], "label") <- attr(x[[i]], "label", exact = TRUE)
      attr(out[[i]], "labels") <- attr(x[[i]], "labels", exact = TRUE)
    }
    return(out)

  } else if (isFALSE(is_data_frame)) {
    to <- as.data.frame(to)
  }

  original_x <- x

  # evaluate
  match <- match.arg(tolower(match), c("and", "&", "&&", "or", "|", "||", "!", "not"))
  match <- switch(match,
    "&" = ,
    "&&" = ,
    "and" = "and",
    "!" = ,
    "not" = "not",
    "or"
  )

  # sanity check
  shared_columns <- intersect(colnames(x), colnames(to))
  if (is.null(shared_columns) || length(shared_columns) == 0) {
    stop(insight::format_message("None of the columns from the data frame with matching conditions were found in `x`."))
  }

  # only select common columns
  x <- x[shared_columns]

  # prepare
  if (identical(match, "or")) {
    idx <- c()
  } else {
    # remove missings before matching
    x <- x[stats::complete.cases(x), , drop = FALSE]
    idx <- 1:nrow(x)
  }

  # Find matching rows
  for (col in names(to)) {
    values <- x[[col]]
    if (match == "or") {
      idx <- union(idx, which(values %in% to[[col]]))
    } else if (match == "not") {
      idx <- idx[!values[idx] %in% to[[col]]]
    } else {
      idx <- idx[values[idx] %in% to[[col]]]
    }
  }

  # prepare output
  if (isFALSE(return_indices)) {
    out <- original_x[idx, , drop = FALSE]
    # restore value and variable labels
    for (i in colnames(out)) {
      attr(out[[i]], "label") <- attr(original_x[[i]], "label", exact = TRUE)
      attr(out[[i]], "labels") <- attr(original_x[[i]], "labels", exact = TRUE)
    }
  } else {
    out <- idx
  }

  out
}
