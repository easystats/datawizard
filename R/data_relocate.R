#' @title Relocate (reorder) columns of a data frame
#' @name data_relocate
#'
#' @description
#' `data_relocate()` will reorder columns to specific positions, indicated by
#' `before` or `after`. `data_reorder()` will instead move selected columns to
#' the beginning of a data frame. Finally, `data_remove()` removes columns
#' from a data frame. All functions support select-helpers that allow flexible
#' specification of a search pattern to find matching columns, which should
#' be reordered or removed.
#'
#' @param data A data frame.
#' @param before,after Destination of columns. Supplying neither will move
#'   columns to the left-hand side; specifying both is an error. Can be a
#'   character vector, indicating the name of the destination column, or a
#'   numeric value, indicating the index number of the destination column.
#'   If `-1`, will be added before or after the last column.
#' @inheritParams extract_column_names
#' @inheritParams data_rename
#'
#' @inherit data_rename seealso
#'
#' @return A data frame with reordered columns.
#'
#' @examples
#' # Reorder columns
#' head(data_relocate(iris, select = "Species", before = "Sepal.Length"))
#' head(data_relocate(iris, select = "Species", before = "Sepal.Width"))
#' head(data_relocate(iris, select = "Sepal.Width", after = "Species"))
#' # which is same as
#' head(data_relocate(iris, select = "Sepal.Width", after = -1))
#'
#' # Reorder multiple columns
#' head(data_relocate(iris, select = c("Species", "Petal.Length"), after = "Sepal.Width"))
#' # which is same as
#' head(data_relocate(iris, select = c("Species", "Petal.Length"), after = 2))
#'
#' # Reorder columns
#' head(data_reorder(iris, c("Species", "Sepal.Length")))
#'
#' @export
data_relocate <- function(data,
                          select,
                          before = NULL,
                          after = NULL,
                          ignore_case = FALSE,
                          regex = FALSE,
                          verbose = TRUE,
                          ...) {
  # Sanitize
  if (!is.null(before) && !is.null(after)) {
    insight::format_error("You must supply only one of `before` or `after`.")
  }

  # allow numeric values
  if (!is.null(before) && is.numeric(before)) {
    if (before == -1) {
      before <- names(data)[ncol(data)]
    } else if (before >= 1 && before <= ncol(data)) {
      before <- names(data)[before]
    } else {
      insight::format_error("No valid position defined in `before`.")
    }
  }

  # allow numeric values
  if (!is.null(after) && is.numeric(after)) {
    if (after == -1) {
      after <- names(data)[ncol(data)]
    } else if (after >= 1 && after <= ncol(data)) {
      after <- names(data)[after]
    } else {
      insight::format_error("No valid position defined in `after`.")
    }
  }

  cols <- .select_nse(select,
    data,
    exclude = NULL,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose
  )

  # save attributes
  attr_data <- attributes(data)

  # Move columns to the right hand side
  data <- data[c(setdiff(names(data), cols), cols)]

  # Get columns and their original position
  data_cols <- names(data)
  position <- which(data_cols %in% cols)

  # remember original values, for more informative messages
  original_before <- before
  original_after <- after

  # Find new positions
  # nolint start
  if (!is.null(before)) {
    before <- before[before %in% data_cols][1] # Take first that exists (if vector is supplied)
    if (length(before) != 1 || is.na(before)) {
      # guess the misspelled column
      insight::format_error(
        "The column passed to `before` wasn't found.",
        .misspelled_string(data_cols, original_before[1], default_message = "Possibly misspelled?")
      )
    }
    where <- min(match(before, data_cols))
    position <- c(setdiff(position, where), where)
  } else if (!is.null(after)) {
    after <- after[after %in% data_cols][1] # Take first that exists (if vector is supplied)
    if (length(after) != 1 || is.na(after)) {
      # guess the misspelled column
      insight::format_error(
        "The column passed to `after` wasn't found.",
        .misspelled_string(data_cols, original_after[1], default_message = "Possibly misspelled?")
      )
    }
    where <- max(match(after, data_cols))
    position <- c(where, setdiff(position, where))
  } else {
    where <- 1
    position <- union(position, where)
  }
  # nolint end

  # Set left and right side
  lhs <- setdiff(seq(1, where - 1), position)
  rhs <- setdiff(seq(where + 1, ncol(data)), position)
  position <- unique(c(lhs, position, rhs))
  position <- position[position <= length(data_cols)]

  out <- data[position]
  out <- .replace_attrs(out, attr_data)

  out
}



#' @rdname data_relocate
#' @export
data_reorder <- function(data,
                         select,
                         exclude = NULL,
                         ignore_case = FALSE,
                         regex = FALSE,
                         verbose = TRUE,
                         ...) {
  cols <- .select_nse(select,
    data,
    exclude = NULL,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose
  )
  remaining_columns <- setdiff(colnames(data), cols)

  out <- data[c(cols, remaining_columns)]
  out <- .replace_attrs(out, attributes(data))
  out
}
