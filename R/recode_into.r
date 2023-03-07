#' @title Recode values from one or more variables into a new variable
#' @name recode_into
#'
#' @description
#' This functions recodes values from one or more variables into a new variable.
#' It is a convenient function to avoid nested [`ifelse()`] statements.
#'
#' @param ... A sequence of two-sided formulas, where the left hand side (LHS)
#' is a logical condition that determines which values match this case. The
#' right hand side (RHS) indicates the replacement value. The last formula can
#' be used to indicate the default value. If the `default` argument is provided,
#' the last formula is considered as further recode-pattern.
#' @param data Optional, name of a data frame. This can be used to avoid writing
#' the data name multiple times in `...`. See 'Examples'.
#' @param default Indicates the default value that is chosen when no match in
#' the formulas in `...` is found. If `NULL`, the default value needs to be
#' determined as last formula in `...`. See 'Examples'.
#'
#' @return A vector with recoded values.
#'
#' @examples
#' x <- 1:30
#' recode_into(
#'   x > 15 ~ "a",
#'   x > 10 & x <= 15 ~ "b",
#'   ~ "c"
#' )
#'
#' # use default argument
#' recode_into(
#'   x > 15 ~ "a",
#'   x > 10 & x <= 15 ~ "b",
#'   default = "c"
#' )
#'
#' set.seed(123)
#' d <- data.frame(
#'   x = sample(1:5, 30, TRUE),
#'   y = sample(letters[1:5], 30, TRUE),
#'   stringsAsFactors = FALS
#' )
#'
#' # from different variables into new vector
#' recode_into(
#'   d$x %in% 1:3 & d$y %in% c("a", "b") ~ 1,
#'   d$x > 3 ~ 2,
#'   TRUE ~ 0 # the LHS of this formula is arbitrary and can be omitted
#' )
#'
#' # no need to write name of data frame each time
#' recode_into(
#'   x %in% 1:3 & y %in% c("a", "b") ~ 1,
#'   x > 3 ~ 2,
#'   data = d,
#'   default = 0
#' )
#' @export
recode_into <- function(..., data = NULL, default = NULL) {
  dots <- list(...)

  # get length of vector, so we know the length of the output vector
  len <- if (is.null(data)) {
    length(eval(dots[[1]][[2]]))
  } else {
    length(with(data, eval(dots[[1]][[2]])))
  }

  # how many expressions (recodes) do we have? last is assumed to default
  n_params <- length(dots) - 1

  # last expression should always be the default value
  if (!is.null(default)) {
    default <- default
    # if last expression is not default, increase n_params again
    n_params <- n_params + 1
  } else if (length(dots[[n_params + 1]]) == 3) {
    default <- dots[[n_params + 1]][[3]]
  } else {
    default <- dots[[n_params + 1]][[2]]
  }

  # create default output vector
  out <- rep(default, times = len)

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
