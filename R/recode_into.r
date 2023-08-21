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
#' @param overwrite Logical, if `TRUE` (default) and more than one recode pattern
#' apply to the same case, already recoded values will be overwritten by subsequent
#' recode patterns. If `FALSE`, former recoded cases will not be altered by later
#' recode patterns that would apply to those cases again. A warning message is
#' printed to alert such situations and to avoid unintentional recodings.
#' @param verbose Toggle warnings.
#'
#' @return A vector with recoded values.
#'
#' @examples
#' x <- 1:30
#' recode_into(
#'   x > 15 ~ "a",
#'   x > 10 & x <= 15 ~ "b",
#'   default = "c"
#' )
#'
#' x <- 1:10
#' # default behaviour: second recode pattern "x > 5" overwrites
#' # some of the formerly recoded cases from pattern "x >= 3 & x <= 7"
#' recode_into(
#'   x >= 3 & x <= 7 ~ 1,
#'   x > 5 ~ 2,
#'   default = 0,
#'   verbose = FALSE
#' )
#'
#' # setting "overwrite = FALSE" will not alter formerly recoded cases
#' recode_into(
#'   x >= 3 & x <= 7 ~ 1,
#'   x > 5 ~ 2,
#'   default = 0,
#'   overwrite = FALSE,
#'   verbose = FALSE
#' )
#'
#' set.seed(123)
#' d <- data.frame(
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
recode_into <- function(..., data = NULL, default = NA, overwrite = TRUE, verbose = TRUE) {
  dots <- list(...)

  # get length of vector, so we know the length of the output vector
  len <- if (is.null(data)) {
    length(.dynEval(dots[[1]][[2]], ifnotfound = NULL))
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
      type <- typeof(.dynEval(dots[[i]][[3]], ifnotfound = NULL))
      len_matches <- length(.dynEval(dots[[i]][[2]], ifnotfound = NULL))
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
      index <- .dynEval(dots[[i]][[2]], ifnotfound = NULL)
      value <- .dynEval(dots[[i]][[3]], ifnotfound = NULL)
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
    # save indices of overwritten cases
    overwritten_cases <- which(index)[already_exists]
    # tell user...
    if (any(already_exists) && verbose) {
      if (overwrite) {
        msg <- paste(
          "Several recode patterns apply to the same cases.",
          "Some of the already recoded cases will be overwritten with new values again",
          sprintf("(e.g. pattern %i overwrites the former recode of case %i).", i, overwritten_cases[1])
        )
      } else {
        msg <- paste(
          "Several recode patterns apply to the same cases.",
          "Some of the already recoded cases will not be altered by later recode patterns.",
          sprintf("(e.g. pattern %i also matches the former recode of case %i).", i, overwritten_cases[1])
        )
      }
      insight::format_warning(msg, "Please check if this is intentional!")
    }
    # if user doesn't want to overwrite, remove already recoded indices
    if (!overwrite) {
      index[overwritten_cases] <- FALSE
    }
    out[index] <- value
  }

  out
}
