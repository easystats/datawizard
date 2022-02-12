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
#' y <- mtcars[28:32, 4:6]
#'
#' # add ID common column
#' x$id <- 1:5
#' y$id <- 3:7
#'
#' # left-join, add new variables from y to x, where "id" values match
#' data_merge(x, y)
#'
#' # right-join, add new variables from x to y, where "id" values match
#' data_merge(x, y, join = "right")
#'
#' # full-join
#' data_merge(x, y, join = "full")
#'
#'
#' data(mtcars)
#' x <- mtcars[1:5, 1:3]
#' y <- mtcars[28:32, c(1, 4:5)]
#'
#' # add ID common column
#' x$id <- 1:5
#' y$id <- 3:7
#'
#' # left-join, no matching rows (because columns "id" and "disp" are used)
#' # new variables get all NA values
#' data_merge(x, y)
#'
#' # one common value in "mpg", so one row from y is copied to x
#' data_merge(x, y, by = "mpg")
#'
#' # only keep rows with matching values in by-column
#' data_merge(x, y, join = "semi", by = "mpg")
#'
#' # only keep rows with non-matching values in by-column
#' data_merge(x, y, join = "anti", by = "mpg")
#'
#' # merge list of data frames
#' x <- mtcars[1:5, 1:3]
#' y <- mtcars[28:32, 3:5]
#' z <- mtcars[11:15, 6:8]
#' x$id <- 1:5
#' y$id <- 3:7
#' z$id <- 2:6
#' data_merge(list(x, y, z), by = "id")
#' @export
data_merge <- function(x, ...) {
  UseMethod("data_merge")
}

#' @rdname data_merge
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


  # check valid combination of "join" and "by" -----------------------

  if (join %in% c("anti", "semi") && (is.null(by) || length(by) > 1)) {
    stop(insight::format_message(sprintf("For `join='%s'`, `by` needs to be a name of only one variable that is present in both data frames.", join)), call. = FALSE)
  }


  # merge --------------------

  out <- x

  # for later sorting
  out$.data_merge_id <- 1:nrow(out)

  out <- switch(
    join,
    "full" = merge(out, y, all = TRUE, sort = FALSE, by = by),
    "left" = merge(out, y, all.x = TRUE, sort = FALSE, by = by),
    "right" = merge(out, y, all.y = TRUE, sort = FALSE, by = by),
    "inner" = merge(out, y, sort = FALSE, by = by),
    "semi" = out[out[[by]] %in% y[[by]], , drop = FALSE],
    "anti" = out[!out[[by]] %in% y[[by]], , drop = FALSE]
  )


  # sort rows, add attributes, and return results -------------------------

  out <- out[order(out$.data_merge_id), ]
  out$.data_merge_id <- NULL

  attributes(out) <- utils::modifyList(attr_y, attributes(out))
  attributes(out) <- utils::modifyList(attr_x, attributes(out))
  out
}



#' @export
data_merge.list <- function(x, join = "left", by = NULL, id = NULL, verbose = TRUE, ...) {
  out <- x[[1]]
  for (i in 2:length(x)) {
    out <- data_merge(out, x[[i]], join = join, by = by, id = id, verbose = verbose, ...)
  }
  out
}
