#' @keywords internal
.get_model_info <- function(model, model_info = NULL, ...) {
  if (is.null(model_info)) model_info <- insight::model_info(model)

  model_info
}

#' Print a message saying that an argument is deprecated and that the user
#' should use its replacement instead.
#'
#' @param arg Argument that is deprecated
#' @param replacement Argument that replaces the deprecated argument
#' @keywords internal
.is_deprecated <- function(arg, replacement) {
  warning(insight::format_message(
    paste0("Argument `", arg, "` is deprecated. Please use `", replacement, "` instead.")
  ), call. = FALSE)
}

#' `NULL` coalescing operator
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
