#' @rdname data_rename
#' @export
data_findcols <- function(data,
                          pattern = NULL,
                          starts_with = NULL,
                          ends_with = NULL) {
  n <- names(data)
  if (!is.null(pattern)) {
    match <- c()
    for (i in c(pattern)) {
      match <- c(match, n[grepl(i, n)])
    }
  }
  if (!is.null(starts_with)) {
    match <- n[grepl(paste0(starts_with, ".*"), n)]
  }
  if (!is.null(ends_with)) {
    match <- n[grepl(paste0(".*", ends_with), n)]
  }
  match
}
