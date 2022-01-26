#' Relocate (reorder) columns of a data frame
#'
#' @param data A data frame to pivot.
#' @param cols A character vector indicating the names of the columns to move.
#' @param before,after Destination of columns. Supplying neither will move
#'   columns to the left-hand side; specifying both is an error. Can be a
#'   character vector, indicating the name of the destination column, or a
#'   numeric value, indicating the index number of the destination column.
#'   If `-1`, will be added before or after the last column.
#' @param safe If `TRUE`, will disregard non-existing columns.
#' @inheritParams data_rename
#'
#' @examples
#' # Reorder columns
#' head(data_relocate(iris, cols = "Species", before = "Sepal.Length"))
#' head(data_relocate(iris, cols = "Species", before = "Sepal.Width"))
#' head(data_relocate(iris, cols = "Sepal.Width", after = "Species"))
#' # same as
#' head(data_relocate(iris, cols = "Sepal.Width", after = -1))
#'
#' # reorder multiple columns
#' head(data_relocate(iris, cols = c("Species", "Petal.Length"), after = "Sepal.Width"))
#' # same as
#' head(data_relocate(iris, cols = c("Species", "Petal.Length"), after = 2))
#' @return A data frame with reordered columns.
#'
#' @export
data_relocate <- function(data,
                          cols,
                          before = NULL,
                          after = NULL,
                          safe = TRUE,
                          ...) {

  # Sanitize
  if (!is.null(before) && !is.null(after)) {
    stop("You must supply only one of `before` or `after`.")
  }

  # allow numeric values
  if (!is.null(before) && is.numeric(before)) {
    if (before == -1) {
      before <- names(data)[ncol(data)]
    } else if (before >= 1 && before <= ncol(data)) {
      before <- names(data)[before]
    } else {
      stop("No valid position defined in 'before'.", call. = FALSE)
    }
  }

  # allow numeric values
  if (!is.null(after) && is.numeric(after)) {
    if (after == -1) {
      after <- names(data)[ncol(data)]
    } else if (after >= 1 && after <= ncol(data)) {
      after <- names(data)[after]
    } else {
      stop("No valid position defined in 'before'.", call. = FALSE)
    }
  }

  if (safe) {
    if (!is.null(cols) && is.numeric(cols)) {
      cols <- data[intersect(cols, seq_len(ncol(data)))]
    }
    cols <- cols[cols %in% names(data)]
  }

  # save attributes
  att <- attributes(data)

  # Move columns to the right hand side
  data <- data[c(setdiff(names(data), cols), cols)]

  # Get columns and their original position
  data_cols <- names(data)
  position <- which(data_cols %in% cols)

  # Find new positions
  if (!is.null(before)) {
    before <- before[before %in% data_cols][1] # Take first that exists (if vector is supplied)
    if (length(before) != 1) {
      stop("The column passed to 'before' wasn't found. Possibly mispelled.")
    }
    where <- min(match(before, data_cols))
    position <- c(setdiff(position, where), where)
  } else if (!is.null(after)) {
    after <- after[after %in% data_cols][1] # Take first that exists (if vector is supplied)
    if (length(after) != 1) {
      stop("The column passed to 'after' wasn't found. Possibly mispelled.")
    }
    where <- max(match(after, data_cols))
    position <- c(where, setdiff(position, where))
  } else {
    where <- 1
    position <- union(position, where)
  }

  # Set left and right side
  lhs <- setdiff(seq(1, where - 1), position)
  rhs <- setdiff(seq(where + 1, ncol(data)), position)
  position <- unique(c(lhs, position, rhs))
  position <- position[position <= length(data_cols)]

  out <- data[position]
  attributes(out) <- utils::modifyList(att, attributes(out))

  out
}
