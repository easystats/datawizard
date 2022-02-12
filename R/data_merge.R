#' @title Merge (join) two data frames, or a list of data frames
#' @name data_merge
#'
#' @description
#' Merge (join) two data frames, or a list of data frames.
#'
#' @param x,y A data frame to merge
#' @param join Character vector, indicating the way of joining the data frames.
#' @param by Specifications of the columns used for merging.
#' @param id Optional name for ID column that will be created to indicate the source data frames for appended rows.
#' @param ... Not used.
#'
#' @return
#' A merged data frame.
#'
#' @examples
#' data(iris)
#' @export
data_merge <- function(x, ...) {
  UseMethod("data_merge")
}

#' @export
data_join <- data_merge

#' @rdname data_merge
#' @export
data_merge.data.frame <- function(x, y, join = "left", by = NULL, id = NULL, ...) {
  attr_x <- attributes(x)
  attr_y <- attributes(y)

  # check merge columns
  if (is.null(by)) {
    by <- intersect(colnames(x), colnames(y))
  }

  attributes(out) <- utils::modifyList(attr_y, attributes(out))
  attributes(out) <- utils::modifyList(attr_x, attributes(out))
  out
}
