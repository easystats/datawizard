#' Arrange rows by column values
#'
#' `data_arrange()` orders the rows of a data frame by the values of selected
#' columns.
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @param select Character vector of column names. Use a dash just before column
#'   name to arrange in decreasing order, for example `"-x1"`.
#' @param safe Do not throw an error if one of the variables specified doesn't
#'   exist.
#'
#' @return A data frame.
#'
#' @examples
#' \dontrun{
#' # Arrange using several variables
#' data_arrange(head(mtcars), "gear", "carb")
#'
#' # Arrange in decreasing order
#' data_arrange(head(mtcars), "-carb")
#'
#' # Throw an error if one of the variables specified doesn't exist
#' data_arrange(head(mtcars), "gear", "foo", safe = FALSE)
#' }
#' @export
data_arrange <- function(data, select = NULL, safe = TRUE) {
  if (is.null(select) || length(select) == 0) {
    return(data)
  }

  # coerce to data frame?
  if (!is.data.frame(data)) {
    data <- tryCatch(as.data.frame(data, stringsAsFactors = FALSE),
      error = function(e) {
        stop("Could not coerce `data` into a data frame.", call. = FALSE)
      }
    )
  }

  # find which vars should be decreasing
  desc <- select[grepl("^-", select)]
  desc <- gsub("^-", "", desc)
  select <- gsub("^-", "", select)

  # check for variables that are not in data
  dont_exist <- select[which(!select %in% names(data))]
  if (length(dont_exist) > 0) {
    if (!safe) {
      insight::format_error(
        paste0(
          "The following column(s) don't exist in the dataset: ",
          text_concatenate(dont_exist), "."
        )
      )
    }
    select <- select[-which(select %in% dont_exist)]
  }

  if (length(select) == 0) {
    return(data)
  }

  out <- data

  # reverse order for variables that should be decreasing
  if (length(desc) > 0) {
    for (i in desc) {
      out[[i]] <- -xtfrm(out[[i]])
    }
  }

  # apply ordering
  if (length(select) == 1) {
    data[order(out[[select]]), ]
  } else {
    data[do.call(order, out[, select]), ]
  }
}
