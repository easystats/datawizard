

#' @export
convert_to_na <- function(x, ...) {
  UseMethod("convert_to_na")
}



#' @rdname convert_to_na
#' @export
convert_to_na.data.frame <- function(x,
                                   robust = FALSE,
                                   two_sd = FALSE,
                                   weights = NULL,
                                   verbose = TRUE,
                                   reference = NULL,
                                   select = NULL,
                                   exclude = NULL,
                                   remove_na = c("none", "selected", "all"),
                                   force = FALSE,
                                   append = FALSE,
                                   center = NULL,
                                   scale = NULL,
                                   ...) {


  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  # check append argument, and set default
  if (isFALSE(append)) {
    append <- NULL
  } else if (isTRUE(append)) {
    append <- append_suffix
  }

  if (!is.null(weights) && is.character(weights)) {
    if (weights %in% colnames(x)) {
      exclude <- c(exclude, weights)
    } else {
      warning(insight::format_message("Could not find weighting column '", weights, "'. Weighting not carried out."))
      weights <- NULL
    }
  }

  select <- .select_variables(x, select, exclude, force)


}
