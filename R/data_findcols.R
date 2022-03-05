#' @param pattern A regular expression (as character string), representing the
#'   pattern to be matched in the in column names. Can also be one of the
#'   following select-helpers: `starts_with("")`, `end_with("")` or
#'   `contains("")`
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
#' # using select helpers
#' data_findcols(iris, starts_with("Sepal"))
#' data_findcols(iris, ends_with("Width"))
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

  # init
  n <- names(data)
  match <- c()
  p <- substitute(pattern)

  # evaluate pattern, can be function like "starts_with()"
  pattern <- tryCatch(
    {
      eval(p)
    },
    error = function(e) {
      .evaluate_pattern(.safe_deparse(p))
    }
  )

  if (!is.null(pattern)) {
    for (i in pattern) {
      match <- c(match, n[grepl(i, n)])
    }
  }
  if (!is.null(starts_with)) {
    match <- c(match, n[grepl(paste0("^", starts_with), n)])
  }
  if (!is.null(ends_with)) {
    match <- c(match, n[grepl(paste0(ends_with, "$"), n)])
  }
  match
}


.evaluate_pattern <- function(x) {
  if (grepl("starts_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("^", gsub("starts_with\\(\"(.*)\"\\)", "\\1", x))
  } else if (grepl("ends_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0(gsub("ends_with\\(\"(.*)\"\\)", "\\1", x), "$")
  } else if (grepl("contains\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("\\Q", gsub("contains\\(\"(.*)\"\\)", "\\1", x), "\\E")
  } else {
    pattern <- x
  }
  gsub("\\\\", "\\", pattern, fixed = TRUE)
}
