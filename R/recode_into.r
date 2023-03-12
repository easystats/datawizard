#' @title Recode values from one or more variables into a new variable
#' @name recode_into
#'
#' @description
#' This functions recodes values from one or more variables into a new variable.
#' It is a convenient function to avoid nested [`ifelse()`] statements, which
#' is similar to `dplyr::case_when()`.
#'
#' @param ... A sequence of two-sided formulas, where the left hand side (LHS)
#' is a logical matching condition that determines which values match this case.
#' The LHS of this formula is also called "recode pattern" (e.g., in messages).
#' The right hand side (RHS) indicates the replacement value.
#' @param data Optional, name of a data frame. This can be used to avoid writing
#' the data name multiple times in `...`. See 'Examples'.
#' @param default Indicates the default value that is chosen when no match in
#' the formulas in `...` is found. If not provided, `NA` is used as default
#' value.
#' @param verbose Toggle warnings.
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

  all_recodes <- NULL
  all_same_length <- NULL
  # check recode values
  for (i in seq_len(n_params)) {
    # get type of all recode values
    if (is.null(data)) {
      type <- typeof(eval(dots[[i]][[3]]))
      len_matches <- length(eval(dots[[i]][[2]]))
    } else {
      type <- typeof(with(data, eval(dots[[i]][[3]])))
      len_matches <- length(with(data, eval(dots[[i]][[2]])))
    }
    all_recodes <- c(all_recodes, type)
    all_same_length <- c(all_same_length, len_matches)
  }
  # if we have mixed types, warn user
  if (!is.null(all_recodes) && !all(all_recodes == all_recodes[1])) {
    insight::format_error("Recoding not carried out. Not all recode values are of the same type.")
  }
  # all inputs of correct length?
  if (!is.null(all_same_length) && !all(all_same_length == all_same_length[1])) {
    insight::format_error(
      "The matching conditions return vectors of different length.",
      "Please check if all variables in your recode patterns are of the same length."
    )
  }


  # iterate all expressions
  for (i in seq_len(n_params)) {
    # grep index of observations with replacements and replacement value
    if (is.null(data)) {
      index <- eval(dots[[i]][[2]])
      value <- eval(dots[[i]][[3]])
    } else {
      index <- with(data, eval(dots[[i]][[2]]))
      value <- with(data, eval(dots[[i]][[3]]))
    }
    # overwriting values? do more recode-patterns match the same case?
    if (is.na(default)) {
      already_exists <- !is.na(out[index])
    } else {
      already_exists <- out[index] != default
    }
    if (any(already_exists) && verbose) {
      insight::format_warning(
        paste(
          "Several recode patterns apply to the same cases.",
          "Some of the already recoded cases will be overwritten with new values again",
          sprintf("(e.g. pattern %i overwrites the former recode of case %i).", i, which(already_exists)[1])
        ),
        "Please check if this is intentional!"
      )
    }
    out[index] <- value
  }

  out
}
