#' @rdname normalize
#' @export
unnormalize <- function(x, ...) {
  UseMethod("unnormalize")
}


#' @export
unnormalize.default <- function(x, ...) {
  insight::format_error(
    "Variables of class '", class(x)[1], "' can't be unnormalized."
  )
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
      insight::format_warning("Can't unnormalize variable. Information about range and/or minimum value is missing.")
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
                                   regex = FALSE,
                                   verbose = TRUE,
                                   ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )
  x[select] <- lapply(x[select], unnormalize, verbose = verbose)

  x
}
