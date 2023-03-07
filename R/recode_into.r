#' @title Recode values from one or more variables into a new variable
#' @name recode_into
#'
#' @description
#' This functions recodes values from one or more variables into a new variable.
#' It is a convenient function to avoid nested [`ifelse()`] statements, which
#' is similar to `dplyr::case_when()`.
#'
#' @param ... A sequence of two-sided formulas, where the left hand side (LHS)
#' is a logical condition that determines which values match this case. The
#' right hand side (RHS) indicates the replacement value.
#' @param data Optional, name of a data frame. This can be used to avoid writing
#' the data name multiple times in `...`. See 'Examples'.
#' @param default Indicates the default value that is chosen when no match in
#' the formulas in `...` is found. If not provided, `NA` is used as default
#' value.
#'
#' @return A vector with recoded values.
#'
#' @examples
#' x <<- 1:30
#' recode_into(
#'   x > 15 ~ "a",
#'   x > 10 & x <= 15 ~ "b",
#'   default = "c"
#' )
#'
#' set.seed(123)
#' d <<- data.frame(
#'   x = sample(1:5, 30, TRUE),
#'   y = sample(letters[1:5], 30, TRUE),
#'   stringsAsFactors = FALSE
#' )
#'
#' # from different variables into new vector
#' recode_into(
#'   d$x %in% 1:3 & d$y %in% c("a", "b") ~ 1,
#'   d$x > 3 ~ 2,
#'   default = 0
#' )
#'
#' # no need to write name of data frame each time
#' recode_into(
#'   x %in% 1:3 & y %in% c("a", "b") ~ 1,
#'   x > 3 ~ 2,
#'   data = d,
#'   default = 0
#' )
#'
#' # pipe-style
#' if (getRversion() >= "4.1.0") {
#'   d |>
#'     recode_into(
#'       data = _,
#'       x %in% 1:3 & y %in% c("a", "b") ~ 1,
#'       x > 3 ~ 2,
#'       default = 0
#'     )
#' }
#' @export
recode_into <- function(..., data = NULL, default = NA, verbose = TRUE) {
  dots <- list(...)

  # get length of vector, so we know the length of the output vector
  len <- if (is.null(data)) {
    length(eval(dots[[1]][[2]]))
  } else {
    length(with(data, eval(dots[[1]][[2]])))
  }

  # how many expressions (recode-formulas) do we have?
  n_params <- length(dots)

  # last expression should always be the default value
  if (is.null(default)) {
    default <- NA
    if (verbose) {
      insight::format_warning("Default value can't be `NULL`, setting to `NA` now.")
    }
  }

  # create default output vector
  out <- rep(default, times = len)

  # check recode values
  for (i in seq_len(n_params)) {
    # get type of all recode values
    if (is.null(data)) {
      all_recodes <- typeof(eval(dots[[i]][[3]]))
    } else {
      all_recodes <- typeof(with(data, eval(dots[[i]][[3]])))
    }
    # if we have mixed types, warn user
    if (!(all_recodes == all_recodes[1]) && verbose) {
      insight::format_warning(
        "Not all recode values are of the same type.",
        "Trying to coerce all values into a common type, please check your recoded variable!"
      )
    }
  }

  # iterate all expressions
  for (i in seq_len(n_params)) {
    if (is.null(data)) {
      index <- eval(dots[[i]][[2]])
      value <- eval(dots[[i]][[3]])
    } else {
      index <- with(data, eval(dots[[i]][[2]]))
      value <- with(data, eval(dots[[i]][[3]]))
    }
    out[index] <- value
  }

  out
}
