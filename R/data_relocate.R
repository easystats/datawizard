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
#' @inheritParams find_columns
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
#' # same as
#' head(data_relocate(iris, select = "Sepal.Width", after = -1))
#'
#' # reorder multiple columns
#' head(data_relocate(iris, select = c("Species", "Petal.Length"), after = "Sepal.Width"))
#' # same as
#' head(data_relocate(iris, select = c("Species", "Petal.Length"), after = 2))
#'
#' # Reorder columns
#' head(data_reorder(iris, c("Species", "Sepal.Length")))
#' head(data_reorder(iris, c("Species", "dupa"))) # Safe for non-existing cols
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
    stop("You must supply only one of `before` or `after`.", call. = FALSE)
  }

  # allow numeric values
  if (!is.null(before) && is.numeric(before)) {
    if (before == -1) {
      before <- names(data)[ncol(data)]
    } else if (before >= 1 && before <= ncol(data)) {
      before <- names(data)[before]
    } else {
      stop("No valid position defined in `before`.", call. = FALSE)
    }
  }

  # allow numeric values
  if (!is.null(after) && is.numeric(after)) {
    if (after == -1) {
      after <- names(data)[ncol(data)]
    } else if (after >= 1 && after <= ncol(data)) {
      after <- names(data)[after]
    } else {
      stop("No valid position defined in `after`.", call. = FALSE)
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
      stop("The column passed to `before` wasn't found. Possibly mispelled.", call. = FALSE)
    }
    where <- min(match(before, data_cols))
    position <- c(setdiff(position, where), where)
  } else if (!is.null(after)) {
    after <- after[after %in% data_cols][1] # Take first that exists (if vector is supplied)
    if (length(after) != 1) {
      stop("The column passed to `after` wasn't found. Possibly mispelled.", call. = FALSE)
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
  attributes(out) <- utils::modifyList(attributes(data), attributes(out))
  out
}
