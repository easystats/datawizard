#' @rdname normalize
#' @export
unnormalize <- function(x, ...) {
  UseMethod("unnormalize")
}


#' @export
unnormalize.numeric <- function(x, verbose = TRUE, ...) {
  x
}


#' @rdname normalize
#' @export
unnormalize.numeric <- function(x, verbose = TRUE, ...) {
  ## TODO implement algorithm include_bounds = FALSE
  include_bounds <- attr(x, "include_bounds")
  min_value <- attr(x, "min_value")
  range_difference <- attr(x, "range_difference")
  to_range <- attr(x, "to_range")

  if (is.null(min_value) || is.null(range_difference)) {
    if (verbose) {
      warning(insight::format_message("Can't unnormalize variable. Information about range and/or minimum value is missing."), call. = FALSE)
    }
    return(x)
  }

  if (is.null(to_range)) {
    x * range_difference + min_value
  } else {
    (x - to_range[1]) * (range_difference / diff(to_range)) + min_value
  }
}


#' @rdname normalize
#' @export
unnormalize.data.frame <- function(x,
                                   select = NULL,
                                   exclude = NULL,
                                   ignore_case = FALSE,
                                   verbose = TRUE,
                                   ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select, x, exclude, ignore_case, verbose = verbose)
  x[select] <- lapply(x[select], unnormalize, verbose = verbose)

  x
}
