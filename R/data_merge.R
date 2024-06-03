#' @title Merge (join) two data frames, or a list of data frames
#' @name data_merge
#'
#' @description
#' Merge (join) two data frames, or a list of data frames. However, unlike
#' base R's `merge()`, `data_merge()` offers a few more methods to join data
#' frames, and it does not drop data frame nor column attributes.
#'
#' @param x,y A data frame to merge. `x` may also be a list of data frames
#'   that will be merged. Note that the list-method has no `y` argument.
#' @param join Character vector, indicating the method of joining the data frames.
#'   Can be `"full"`, `"left"` (default), `"right"`, `"inner"`, `"anti"`, `"semi"`
#'   or `"bind"`. See details below.
#' @param by Specifications of the columns used for merging.
#' @param id Optional name for ID column that will be created to indicate the
#'   source data frames for appended rows. Only applies if `join = "bind"`.
#' @param verbose Toggle warnings.
#' @param ... Not used.
#'
#' @return
#' A merged data frame.
#'
#' @section Merging data frames:
#'
#' Merging data frames is performed by adding rows (cases), columns
#' (variables) or both from the source data frame (`y`) to the target
#' data frame (`x`). This usually requires one or more variables which
#' are included in both data frames and that are used for merging, typically
#' indicated with the `by` argument. When `by` contains a variable present
#' in both data frames, cases are matched and filtered by identical values
#' of `by` in `x` and `y`.
#'
#' @section Left- and right-joins:
#'
#' Left- and right joins usually don't add new rows (cases), but only new
#' columns (variables) for existing cases in `x`. For `join = "left"` or
#' `join = "right"` to work, `by` *must* indicate one or more columns that
#' are included in both data frames. For `join = "left"`, if `by` is an
#' identifier variable, which is included in both `x` and `y`, all variables
#' from `y` are copied to `x`, but only those cases from `y` that have
#' matching values in their identifier variable in `x` (i.e. all cases
#' in `x` that are also found in `y` get the related values from the new
#' columns in `y`). If there is no match between identifiers in `x` and `y`,
#' the copied variable from `y` will get a `NA` value for this particular
#' case. Other variables that occur both in `x` and `y`, but are not used
#' as identifiers (with `by`), will be renamed to avoid multiple identical
#' variable names. Cases in `y` where values from the identifier have no
#' match in `x`'s identifier are removed. `join = "right"` works in
#' a similar way as `join = "left"`, just that only cases from `x` that
#' have matching values in their identifier variable in `y` are chosen.
#'
#' In base R, these are equivalent to `merge(x, y, all.x = TRUE)` and
#' `merge(x, y, all.y = TRUE)`.
#'
#' @section Full joins:
#'
#' Full joins copy all cases from `y` to `x`. For matching cases in both
#' data frames, values for new variables are copied from `y` to `x`. For
#' cases in `y` not present in `x`, these will be added as new rows to `x`.
#' Thus, full joins not only add new columns (variables), but also might
#' add new rows (cases).
#'
#' In base R, this is equivalent to `merge(x, y, all = TRUE)`.
#'
#' @section Inner joins:
#'
#' Inner joins merge two data frames, however, only those rows (cases) are
#' kept that are present in both data frames. Thus, inner joins usually
#' add new columns (variables), but also remove rows (cases) that only
#' occur in one data frame.
#'
#' In base R, this is equivalent to `merge(x, y)`.
#'
#' @section Binds:
#'
#' `join = "bind"` row-binds the complete second data frame `y` to `x`.
#' Unlike simple `rbind()`, which requires the same columns for both data
#' frames, `join = "bind"` will bind shared columns from `y` to `x`, and
#' add new columns from `y` to `x`.
#'
#' @examples
#'
#' x <- data.frame(a = 1:3, b = c("a", "b", "c"), c = 5:7, id = 1:3)
#' y <- data.frame(c = 6:8, d = c("f", "g", "h"), e = 100:102, id = 2:4)
#'
#' x
#' y
#'
#' # "by" will default to all shared columns, i.e. "c" and "id". new columns
#' # "d" and "e" will be copied from "y" to "x", but there are only two cases
#' # in "x" that have the same values for "c" and "id" in "y". only those cases
#' # have values in the copied columns, the other case gets "NA".
#' data_merge(x, y, join = "left")
#'
#' # we change the id-value here
#' x <- data.frame(a = 1:3, b = c("a", "b", "c"), c = 5:7, id = 1:3)
#' y <- data.frame(c = 6:8, d = c("f", "g", "h"), e = 100:102, id = 3:5)
#'
#' x
#' y
#'
#' # no cases in "y" have the same matching "c" and "id" as in "x", thus
#' # copied variables from "y" to "x" copy no values, all get NA.
#' data_merge(x, y, join = "left")
#'
#' # one case in "y" has a match in "id" with "x", thus values for this
#' # case from the remaining variables in "y" are copied to "x", all other
#' # values (cases) in those remaining variables get NA
#' data_merge(x, y, join = "left", by = "id")
#'
#' data(mtcars)
#' x <- mtcars[1:5, 1:3]
#' y <- mtcars[28:32, 4:6]
#'
#' # add ID common column
#' x$id <- 1:5
#' y$id <- 3:7
#'
#' # left-join, add new variables and copy values from y to x,
#' # where "id" values match
#' data_merge(x, y)
#'
#' # right-join, add new variables and copy values from x to y,
#' # where "id" values match
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
#' # merge list of data frames. can be of different rows
#' x <- mtcars[1:5, 1:3]
#' y <- mtcars[28:31, 3:5]
#' z <- mtcars[11:18, c(1, 3:4, 6:8)]
#' x$id <- 1:5
#' y$id <- 4:7
#' z$id <- 3:10
#' data_merge(list(x, y, z), join = "bind", by = "id", id = "source")
#' @inherit data_rename seealso
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
  class_x <- class(x)

  # save variable attributes
  attr_x_vars <- lapply(x, attributes)
  attr_y_vars <- lapply(y, attributes)
  attr_vars <- c(attr_x_vars, attr_y_vars[names(attr_y_vars)[!names(attr_y_vars) %in% names(attr_x_vars)]])


  # check join-argument ----------------------

  join <- match.arg(join, choices = c("full", "left", "right", "inner", "semi", "anti", "bind"))


  # check id-argument ----------------------

  all_columns <- union(colnames(x), colnames(y))

  if (join == "bind" && !is.null(id) && id %in% all_columns) {
    # ensure unique ID
    id <- make.unique(c(all_columns, id), sep = "_")[length(all_columns) + 1]
    # and also tell user...
    if (isTRUE(verbose)) {
      insight::format_warning(
        sprintf("Value of `id` already exists as column name. ID column was renamed to `%s`.", id)
      )
    }
  }

  if (!is.null(id) && join == "bind") {
    x[[id]] <- 1
    y[[id]] <- 2
  }


  # check merge columns ("by"-argument) ----------------------

  if (join != "bind") {
    # we need a value for "by". If not provided, use all shared column names
    if (is.null(by)) {
      by <- intersect(colnames(x), colnames(y))
    }

    # If not all column names specified in "by" are present, yield warning
    # and use all shared column names
    if (!all(by %in% colnames(x)) || !all(by %in% colnames(y))) {
      missing_in_x <- setdiff(by, colnames(x))
      missing_in_y <- setdiff(by, colnames(y))
      stop_message <- c(
        "Not all columns specified in `by` were found in the data frames.",
        if (length(missing_in_x) > 0L) {
          paste0("Following columns are in `by` but absent in `x`: ", text_concatenate(missing_in_x))
        },
        if (length(missing_in_y) > 0L) {
          paste0("Following columns are in `by` but absent in `y`: ", text_concatenate(missing_in_y))
        }
      )
      if (isTRUE(verbose)) {
        insight::format_error(stop_message)
      }
    }

    # if still both data frames have no common columns, do a full join
    if (!length(by)) {
      if (isTRUE(verbose)) {
        insight::format_warning(
          "Found no matching columns in the data frames. Fully merging both data frames now.",
          "Note that this can lead to unintended results, because rows in `x` and `y` are possibly duplicated.",
          "You probably want to use `data_merge(x, y, join = \"bind\")` instead."
        )
      }
      by <- NULL
      join <- "full"
    }
  }


  # check valid combination of "join" and "by" -----------------------

  if (join %in% c("anti", "semi") && (is.null(by) || length(by) != 1)) {
    insight::format_error(
      sprintf(
        "For `join = \"%s\"`, `by` needs to be a name of only one variable that is present in both data frames.",
        join
      )
    )
  }


  # merge --------------------

  # for later sorting
  if (join != "bind") {
    if (nrow(x) > 0L) {
      x$.data_merge_id_x <- seq_len(nrow(x))
    }
    if (nrow(y) > 0L) {
      y$.data_merge_id_y <- (seq_len(nrow(y))) + nrow(x)
    }
  }
  all_columns <- union(colnames(x), colnames(y))

  out <- switch(join,
    full = merge(x, y, all = TRUE, sort = FALSE, by = by),
    left = merge(x, y, all.x = TRUE, sort = FALSE, by = by),
    right = merge(x, y, all.y = TRUE, sort = FALSE, by = by),
    inner = merge(x, y, sort = FALSE, by = by),
    semi = x[x[[by]] %in% y[[by]], , drop = FALSE],
    anti = x[!x[[by]] %in% y[[by]], , drop = FALSE],
    bind = .bind_data_frames(x, y)
  )


  # sort rows, add attributes, and return results -------------------------

  if (".data_merge_id_x" %in% colnames(out)) {
    # for full joins, we have no complete sorting id, but NAs for each
    # data frame. we now "merge" the two sorting IDs from each data frame.
    if (anyNA(out$.data_merge_id_x) && ".data_merge_id_y" %in% colnames(out)) {
      out$.data_merge_id_x[is.na(out$.data_merge_id_x)] <- out$.data_merge_id_y[is.na(out$.data_merge_id_x)]
    }
    out <- out[order(out$.data_merge_id_x), ]
    out$.data_merge_id_x <- NULL
    out$.data_merge_id_y <- NULL
  }

  # try to restore original column order as good as possible. Therefore, we
  # first take all column names of the original input data frames, then
  # we add all new columns, like duplicated from merging (name.x and name.y,
  # if "name" was in both data frames, but not used in "by"), and then do a
  # final check that all column names are present in "out" (e.g., "name" would)
  # no longer be there if we have "name.x" and "name.y").

  all_columns <- c(all_columns, setdiff(colnames(out), all_columns))
  all_columns <- all_columns[all_columns %in% colnames(out)]
  out <- out[all_columns]

  # add back attributes
  out <- .replace_attrs(out, attributes(y))
  out <- .replace_attrs(out, attributes(x))

  for (i in colnames(out)) {
    if (is.list(attr_vars[[i]])) {
      if (is.list(attributes(out[[i]]))) {
        attributes(out[[i]]) <- utils::modifyList(attr_vars[[i]], attributes(out[[i]]))
      } else {
        attributes(out[[i]]) <- attr_vars[[i]]
      }
    }
  }

  class(out) <- unique(c(class_x, "data.frame"))
  out
}


#' @rdname data_merge
#' @export
data_merge.list <- function(x, join = "left", by = NULL, id = NULL, verbose = TRUE, ...) {
  out <- x[[1]]
  df_id <- rep(1, times = nrow(out))

  for (i in 2:length(x)) {
    out <- data_merge(out, x[[i]], join = join, by = by, id = NULL, verbose = verbose, ...)
    df_id <- c(df_id, rep(i, times = nrow(x[[i]])))
  }

  # we need separate handling for list of data frames and id-variable here
  if (!is.null(id) && join == "bind") {
    if (id %in% colnames(out)) {
      # ensure unique ID
      id <- make.unique(c(colnames(out), id), sep = "_")[length(colnames(out)) + 1]
      # and also tell user...
      if (isTRUE(verbose)) {
        insight::format_warning(
          sprintf("Value of `id` already exists as column name. ID column was renamed to `%s`.", id)
        )
      }
    }
    out[[id]] <- df_id
  }

  out
}


.bind_data_frames <- function(x, y) {
  # merge and sort. "rbind()" is faster than "merge()" if all columns present
  if (all(colnames(x) %in% colnames(y)) && ncol(x) == ncol(y)) {
    # we may have different column order
    out <- rbind(x, y[match(colnames(x), colnames(y))])
  } else {
    # add ID for merging
    if (nrow(x) > 0L) {
      x$.data_merge_row <- seq_len(nrow(x))
    }
    if (nrow(y) > 0L) {
      y$.data_merge_row <- (nrow(x) + 1):(nrow(x) + nrow(y))
    }
    merge_by <- intersect(colnames(x), colnames(y))
    out <- merge(x, y, all = TRUE, sort = FALSE, by = merge_by)
  }

  # for empty df's, merge() may return an empty character vector
  # make sure it's a data frame object.
  if (!is.data.frame(out)) {
    out <- as.data.frame(out)
  }

  if (".data_merge_row" %in% colnames(out)) {
    out <- out[order(out$.data_merge_row), ]
  }

  out$.data_merge_row <- NULL
  out
}
