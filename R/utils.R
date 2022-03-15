#' @keywords internal
.mode <- function(x) {
  uniqv <- unique(x)
  tab <- tabulate(match(x, uniqv))
  idx <- which.max(tab)
  uniqv[idx]
}

