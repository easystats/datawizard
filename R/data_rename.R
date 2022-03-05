#' Rename columns and variable names
#'
#' Safe and intuitive functions to rename variables or rows in dataframes.
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @param pattern,replacement Character strings.
#' @param rows Vector of row names.
#' @param safe Do not throw error if for instance the variable to be
#'   renamed/removed doesn't exist.
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
#' head(data_rename(iris, paste0("Var", 1:5)))
#'
#' @seealso
#' - Functions to rename stuff: [data_rename()], [data_rename_rows()], [data_addprefix()], [data_addsuffix()]
#' - Functions to reorder, find and remove columns: [data_findcols()], [data_reorder()], [data_remove()]
#' - Functions to reshape, pivot or rotate dataframes: [data_to_long()], [data_to_wide()], [data_rotate()]
#' - Functions to rescale and reverse: [data_rescale()], [data_reverse()]
#' - Functions to standardize, normalize, rank-transform: [standardize()], [normalize()], [ranktransform()], [winsorize()]
#' - Split, cut and merge dataframes: [data_partition()], [data_cut()], [data_match()], [data_merge()]
#'
#' @export
data_rename <- function(data, pattern = NULL, replacement = NULL, safe = TRUE, ...) {

  # sanity checks
  if (is.null(replacement) && is.null(pattern)) {
    names(data) <- c(1:ncol(data))
    return(data)
  }

  if (is.null(replacement) && !is.null(pattern)) {
    names(data) <- pattern
    return(data)
  }

  if (!is.null(replacement) && is.null(pattern)) {
    names(data) <- replacement
    return(data)
  }


  if (length(pattern) != length(replacement)) {
    stop("The 'replacement' names must be of the same length than the variable names.")
  }

  for (i in 1:length(pattern)) {
    data <- .data_rename(data, pattern[i], replacement[i], safe)
  }

  data
}

#' @keywords internal
.data_rename <- function(data, pattern, replacement, safe = TRUE) {
  if (isFALSE(safe) & !pattern %in% names(data)) {
    stop(paste0("Variable '", pattern, "' is not in your dataframe :/"))
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
