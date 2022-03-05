#' @param pattern A regular expression (as character string), representing the
#'   pattern to be matched in the in column names.
#' @param starts_with,ends_with Character string, containing the string to be
#'   matched in the column names. `starts_with` finds matches at the beginning
#'   of column names, `ends_with` finds matches at the end of column names.
#' @rdname data_relocate
#' @examples
#' # Find columns names by pattern
#' data_findcols(iris, starts_with = "Sepal")
#' data_findcols(iris, ends_with = "Width")
#' data_findcols(iris, pattern = "\\.")
#' data_findcols(iris, c("Petal.Width", "Sepal.Length"))
#'
#' @export
data_findcols <- function(data,
                          pattern = NULL,
                          starts_with = NULL,
                          ends_with = NULL,
                          ...) {
  # TODO: Need to extend this to work with NSE so that the following shoud work:
  # - data_findcols(iris, Sepal.Length)
  # - data_findcols(iris, starts_with("Sepal"))
  # - data_findcols(iris, contains("Sepal"))
  n <- names(data)
  if (!is.null(pattern)) {
    match <- c()
    for (i in pattern) {
      match <- c(match, n[grepl(i, n)])
    }
  }
  if (!is.null(starts_with)) {
    match <- n[grepl(paste0("^", starts_with), n)]
  }
  if (!is.null(ends_with)) {
    match <- n[grepl(paste0(ends_with, "$"), n)]
  }
  match
}
