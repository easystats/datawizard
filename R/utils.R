#' @keywords internal
.mode <- function(x) {
  uniqv <- unique(x)
  tab <- tabulate(match(x, uniqv))
  idx <- which.max(tab)
  uniqv[idx]
}


#' @keywords internal
.get_model_info <- function(model, model_info = NULL, ...) {
  if (is.null(model_info)) model_info <- insight::model_info(model)

  model_info
}
