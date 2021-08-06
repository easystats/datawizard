#' Convenient dataframe manipulation functionalities
#'
#' Safe and intuitive functions to manipulate dataframes.
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @param pattern,replacement,starts_with,ends_with Character strings.
#' @param cols,rows Vector of column or row names.
#' @param safe Do not throw error if for instance the variable to be
#'   renamed/removed doesn't exist.
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
#' # Find columns names by pattern
#' head(data_findcols(iris, starts_with = "Sepal"))
#' head(data_findcols(iris, ends_with = "Width"))
#' head(data_findcols(iris, pattern = "\\."))
#'
#' # Remove columns
#' head(data_remove(iris, "Sepal.Length"))
#'
#' # Reorder columns
#' head(data_reorder(iris, c("Species", "Sepal.Length")))
#' head(data_reorder(iris, c("Species", "dupa")))
#'
#' # Add prefix / suffix
#' head(data_addprefix(iris, "NEW_"))
#' head(data_addsuffix(iris, "_OLD"))
#' @export
data_rename <- function(data, pattern, replacement, safe = TRUE) {
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
