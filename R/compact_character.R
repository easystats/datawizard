#' Remove empty strings from character
#'
#' @param x A single character or a vector of characters.
#'
#' @return
#'
#' A character or a character vector with empty strings removed.
#'
#' @examples
#' compact_character(c("x", "y", NA))
#' compact_character(c("x", NULL, "", "y"))
#' compact_character(list("x", "y", NA))
#' compact_character(list("x", NULL, "", "y"))
#'
#' @export

compact_character <- function(x) {
  x[!sapply(x, function(i) nchar(i) == 0 || is.null(i) || any(i == "NULL", na.rm = TRUE))]
}

