#' @title Return or remove variables or observations that are completely missing
#' @name remove_empty
#' @rdname remove_empty
#'
#' @description
#'
#' These functions check which rows or columns of a data frame completely
#' contain missing values, i.e. which observations or variables completely have
#' missing values, and either (1) returns their indices; or (2) removes them
#' from the data frame.
#'
#' @param x A data frame.
#' @param include_empty_string If `TRUE` also removes character vectors that
#' completely have empty elements (i.e. where `nchar() == 0`).
#'
#' @return
#'
#' - For `empty_columns()` and `empty_rows()`, a numeric (named) vector with row
#' or column indices of those variables that completely have missing values.
#'
#' - For `remove_empty_columns()` and `remove_empty_rows()`, a data frame with
#' "empty" columns or rows removed, respectively.
#'
#' - For `remove_empty`, **both** empty rows and columns will be removed.
#'
#' @examples
#' tmp <- data.frame(
#'   a = c(1, 2, 3, NA, 5),
#'   b = c(1, NA, 3, NA, 5),
#'   c = c(NA, NA, NA, NA, NA),
#'   d = c(1, NA, 3, NA, 5)
#' )
#'
#' tmp
#'
#' # indices of empty columns or rows
#' empty_columns(tmp)
#' empty_rows(tmp)
#'
#' # remove empty columns or rows
#' remove_empty_columns(tmp)
#' remove_empty_rows(tmp)
#'
#' # remove empty columns and rows
#' remove_empty(tmp)
#' @export
empty_columns <- function(x, include_empty_string = TRUE) {
  if ((!is.matrix(x) && !is.data.frame(x)) || ncol(x) < 2) {
    vector("numeric")
  } else {
    all_na <- colSums(is.na(x)) == nrow(x)
    if (include_empty_string) {
      all_empty <- is.character(x) & max(nchar(x), na.rm = TRUE) == 0
      which(all_na | all_empty)
    } else {
      which(all_na)
    }
  }
}


#' @rdname remove_empty
#' @export
empty_rows <- function(x) {
  if ((!is.matrix(x) && !is.data.frame(x)) || nrow(x) < 2) {
    vector("numeric")
  } else {
    which(rowSums(is.na(x)) == ncol(x))
  }
}


#' @rdname remove_empty
#' @export
remove_empty_columns <- function(x, include_empty_string = TRUE) {
  # check if we have any empty columns at all
  ec <- empty_columns(x, include_empty_string)

  # if yes, removing works, else an empty df would be returned
  if (length(ec)) {
    x <- x[-ec]
  }

  x
}


#' @rdname remove_empty
#' @export
remove_empty_rows <- function(x) {
  # check if we have any empty rows at all
  er <- empty_rows(x)

  # if yes, removing works, else an empty df would be returned
  if (length(er)) {
    att <- attributes(x)
    x <- x[-er, ]
    attributes(x) <- utils::modifyList(att, attributes(x))
  }

  x
}

#' @rdname remove_empty
#' @export
remove_empty <- function(x) {
  x <- remove_empty_rows(x)
  x <- remove_empty_columns(x)
  x
}
