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
#' @param verbose Toggle warnings.
#' @param ... Not used.
#'
#' @return
#' A merged data frame.
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[1:5, 1:3]
#' y <- mtcars[28:32, 3:5]
#'
#' # left-join, add new variables from y to x
#' data_merge(x, y)
#' @export
data_merge <- function(x, ...) {
  UseMethod("data_merge")
}

#' @export
data_join <- data_merge

#' @rdname data_merge
#' @export
data_merge.data.frame <- function(x, y, join = "left", by = NULL, id = NULL, verbose = TRUE, ...) {
  attr_x <- attributes(x)
  attr_y <- attributes(y)


  # check join-argument ----------------------

  join <- match.arg(join, choices = c("full", "left", "right", "inner", "semi", "anti"))


  # check merge columns ("by"-argument) ----------------------

  if (is.null(by)) {
    by <- intersect(colnames(x), colnames(y))
  }

  if (!all(by %in% colnames(x)) || !all(by %in% colnames(y))) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("Not all columns specified in `by` were found in the data frames.",
                                      "Using all columns that are present in both data frames."), call. = FALSE)
    }
    by <- intersect(colnames(x), colnames(y))
  }

  if (!length(by)) {
    if (isTRUE(verbose) && !identical(join, "full")) {
      warning(insight::format_message("Found no matching columns in the data frames. Fully merging both data frames now."), call. = FALSE)
    }
    by <- NULL
    join <- "full"
  }


  # check valid combination of "join" and "by"

  if (join %in% c("anti", "semi") && (is.null(by) || length(by) > 1)) {
    stop(insight::format_message(sprintf("For `join='%s'`, `by` needs to be a name of only one variable that is present in both data frames.", join)), call. = FALSE)
  }


  # merge --------------------

  out <- x

  out <- switch(
    join,
    "full" = merge(out, y, all = TRUE, sort = FALSE, by = by),
    "left" = merge(out, y, all.x = TRUE, sort = FALSE, by = by),
    "right" = merge(out, y, all.y = TRUE, sort = FALSE, by = by),
    "inner" = merge(out, y, sort = FALSE, by = by),
    "semi" = out[out[[by]] %in% y[[by]], , drop = FALSE],
    "anti" = out[!out[[by]] %in% y[[by]], , drop = FALSE]
  )


  attributes(out) <- utils::modifyList(attr_y, attributes(out))
  attributes(out) <- utils::modifyList(attr_x, attributes(out))
  out
}
