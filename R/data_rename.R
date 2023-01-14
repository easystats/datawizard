#' @title Rename columns and variable names
#' @name data_rename
#'
#' @description Safe and intuitive functions to rename variables or rows in
#'   data frames. `data_rename()` will rename column names, i.e. it facilitates
#'   renaming variables `data_addprefix()` or `data_addsuffix()` add prefixes
#'   or suffixes to column names. `data_rename_rows()` is a convenient shortcut
#'   to add or rename row names of a data frame, but unlike `row.names()`, its
#'   input and output is a data frame, thus, integrating smoothly into a possible
#'   pipe-workflow.
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @param pattern Character vector. For `data_rename()`, indicates columns that
#'   should be selected for renaming. Can be `NULL` (in which case all columns
#'   are selected). For `data_addprefix()` or `data_addsuffix()`, a character
#'   string, which will be added as prefix or suffix to the column names.
#' @param replacement Character vector. Indicates the new name of the columns
#'   selected in `pattern`. Can be `NULL` (in which case column are numbered
#'   in sequential order). If not `NULL`, `pattern` and `replacement` must be
#'   of the same length.
#' @param rows Vector of row names.
#' @param safe Do not throw error if for instance the variable to be
#'   renamed/removed doesn't exist.
#' @param verbose Toggle warnings and messages.
#' @param ... Other arguments passed to or from other functions.
#'
#' @return A modified data frame.
#'
#' @examples
#' # Rename columns
#' head(data_rename(iris, "Sepal.Length", "length"))
#' # data_rename(iris, "FakeCol", "length", safe=FALSE)  # This fails
#' head(data_rename(iris, "FakeCol", "length")) # This doesn't
#' head(data_rename(iris, c("Sepal.Length", "Sepal.Width"), c("length", "width")))
#'
#' # Reset names
#' head(data_rename(iris, NULL))
#'
#' # Change all
#' head(data_rename(iris, replacement = paste0("Var", 1:5)))
#'
#' @seealso
#' - Functions to rename stuff: [data_rename()], [data_rename_rows()], [data_addprefix()], [data_addsuffix()]
#' - Functions to reorder or remove columns: [data_reorder()], [data_relocate()], [data_remove()]
#' - Functions to reshape, pivot or rotate data frames: [data_to_long()], [data_to_wide()], [data_rotate()]
#' - Functions to recode data: [rescale()], [reverse()], [categorize()], [recode_values()], [slide()]
#' - Functions to standardize, normalize, rank-transform: [center()], [standardize()], [normalize()], [ranktransform()], [winsorize()]
#' - Split and merge data frames: [data_partition()], [data_merge()]
#' - Functions to find or select columns: [data_select()], [data_find()]
#' - Functions to filter rows: [data_match()], [data_filter()]
#'
#' @export
data_rename <- function(data,
                        pattern = NULL,
                        replacement = NULL,
                        safe = TRUE,
                        verbose = TRUE,
                        ...) {
  # change all names if no pattern specified
  if (is.null(pattern)) {
    pattern <- names(data)
  }

  if (!is.character(pattern)) {
    insight::format_error("Argument `pattern` must be of type character.")
  }

  # name columns 1, 2, 3 etc. if no replacement
  if (is.null(replacement)) {
    replacement <- paste0(seq_along(pattern))
  }

  # if duplicated names in replacement, append ".2", ".3", etc. to duplicates
  # ex: c("foo", "foo") -> c("foo", "foo.2")
  if (anyDuplicated(replacement) > 0L) {
    dup <- as.data.frame(table(replacement))
    dup <- dup[dup$Freq > 1, ]
    for (i in dup$replacement) {
      to_replace <- which(replacement == i)[-1]
      new_replacement <- paste0(i, ".", 1 + seq_along(to_replace))
      replacement[to_replace] <- new_replacement
    }
  }

  if (length(replacement) > length(pattern) && verbose) {
    insight::format_alert(
      paste0(
        "There are more names in `replacement` than in `pattern`. The last ",
        length(replacement) - length(pattern), " names of `replacement` are not used."
      )
    )
  } else if (length(replacement) < length(pattern) && verbose) {
    insight::format_alert(
      paste0(
        "There are more names in `pattern` than in `replacement`. The last ",
        length(pattern) - length(replacement), " names of `pattern` are not modified."
      )
    )
  }

  for (i in seq_along(pattern)) {
    if (!is.na(replacement[i])) {
      data <- .data_rename(data, pattern[i], replacement[i], safe, verbose)
    }
  }

  data
}

#' @keywords internal
.data_rename <- function(data, pattern, replacement, safe = TRUE, verbose = TRUE) {
  if (!pattern %in% names(data)) {
    if (isTRUE(safe)) {
      # only give message when verbose is TRUE
      if (verbose) {
        insight::format_alert(paste0("Variable `", pattern, "` is not in your data frame :/"))
      }
      # if not safe, always error, no matter what verbose is
    } else {
      insight::format_error(paste0("Variable `", pattern, "` is not in your data frame :/"))
    }
  }

  names(data) <- replace(names(data), names(data) == pattern, replacement)

  data
}


# Row.names ----------------------------------------------------------------

#' @rdname data_rename
#' @export
data_rename_rows <- function(data, rows = NULL) {
  row.names(data) <- rows
  data
}
