#' @keywords internal
.mode <- function(x) {
  uniqv <- unique(x)
  tab <- tabulate(match(x, uniqv))
  idx <- which.max(tab)
  uniqv[idx]
}

#' @keywords internal
.trim <- function(x) gsub("^\\s+|\\s+$", "", x)

#' @keywords internal
.safe_deparse <- function(string) {
  paste0(sapply(deparse(string, width.cutoff = 500), .trim, simplify = TRUE), collapse = " ")
}
