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
#' @param preserve_na Logical, if `TRUE` and `default` is not `NA`, missing
#' values in the original variable will be set back to `NA` in the recoded
#' variable (unless overwritten by other recode patterns). If `FALSE`, missing
#' values in the original variable will be recoded to `default`. Setting
#' `preserve_na = TRUE` prevents unintentional overwriting of missing values
#' with `default`, which means that you won't find valid values where the
#' original data only had missing values. See 'Examples'.
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
#'
#' # handling of missing values
#' d <- data.frame(
#'   x = c(1, NA, 2, NA, 3, 4),
#'   y = c(1, 11, 3, NA, 5, 6)
#' )
#' # first NA in x is overwritten by valid value from y
#' # we have no known value for second NA in x and y,
#' # thus we get one NA in the result
#' recode_into(
#'   x <= 3 ~ 1,
#'   y > 5 ~ 2,
#'   data = d,
#'   default = 0,
#'   preserve_na = TRUE
#' )
#' # first NA in x is overwritten by valid value from y
#' # default value is used for second NA
#' recode_into(
#'   x <= 3 ~ 1,
#'   y > 5 ~ 2,
#'   data = d,
#'   default = 0,
#'   preserve_na = FALSE
#' )
#' @export
recode_into <- function(...,
                        data = NULL,
                        default = NA,
                        overwrite = TRUE,
                        preserve_na = FALSE,
                        verbose = TRUE) {
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
  new_values <- NULL
  # check recode values
  for (i in seq_len(n_params)) {
    # get type of all recode values
    if (is.null(data)) {
      value_type <- .dynEval(dots[[i]][[3]], ifnotfound = NULL)
      value_length <- .dynEval(dots[[i]][[2]], ifnotfound = NULL)
    } else {
      value_type <- with(data, eval(dots[[i]][[3]]))
      value_length <- with(data, eval(dots[[i]][[2]]))
    }
    # if we have "NA", we don't want to check the type. Else, you cannot use
    # "NA" for numeric recodes, but rather need to use "NA_real_", which is not
    # user-friendly
    if (is.na(value_type)) {
      type <- NULL
    } else {
      type <- typeof(value_type)
    }
    len_matches <- length(value_length)
    # save type and length of recode values
    all_recodes <- c(all_recodes, type)
    all_same_length <- c(all_same_length, len_matches)
    new_values <- c(new_values, value_type)
  }
  # if we have mixed types, warn user
  if (!is.null(all_recodes) && !all(all_recodes == all_recodes[1])) {
    wrong_type <- which(all_recodes != all_recodes[1])
    insight::format_error(
      paste(
        "Recoding not carried out. Not all recode values are of the same type.",
        sprintf(
          "For instance, the new value of the first pattern, `%s`, is of type `%s`. The new value of the %s recode pattern, `%s`, is of type `%s`.", # nolint
          insight::color_text(new_values[1], "cyan"),
          insight::color_text(all_recodes[1], "cyan"),
          .number_to_text(wrong_type[1]),
          insight::color_text(new_values[wrong_type[1]], "cyan"),
          insight::color_text(all_recodes[wrong_type[1]], "cyan")
        )
      )
    )
  }
  # all inputs of correct length?
  if (!is.null(all_same_length) && !all(all_same_length == all_same_length[1])) {
    wrong_length <- which(all_same_length != all_same_length[1])
    insight::format_error(
      "The matching conditions return vectors of different length.",
      paste(
        "Please check if all variables in your recode patterns are of the same length.",
        sprintf(
          "For instance, the first and the %s recode pattern return vectors of different length.",
          .number_to_text(wrong_length[1])
        )
      )
    )
  }

  # indicator to show message when replacing NA by default
  # needed to show message only once
  overwrite_NA_msg <- TRUE

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
    # remember missing values, so we can add back later
    missing_index <- is.na(index)
    # make sure index has no missing values. when we have missing values in
    # original expression, these are considered as "no match" and set to FALSE
    # we handle NA value later and thus want to remove them from "index" now
    index[is.na(index)] <- FALSE
    # overwriting values? do more recode-patterns match the same case?
    if (is.na(default)) {
      already_exists <- !is.na(out[index])
    } else {
      already_exists <- out[index] != default
    }
    # save indices of overwritten cases
    overwritten_cases <- which(index)[already_exists]
    # tell user...
    if (any(already_exists, na.rm = TRUE) && verbose) {
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
    # write new values into output vector
    out[index] <- value
    # set back missing values
    if (any(missing_index) && !is.na(default)) {
      if (preserve_na) {
        # but only where we still have default values
        # we don't want to overwrite already recoded values with NA
        out[missing_index & out == default] <- NA
      } else if (overwrite_NA_msg && verbose) {
        # don't show msg again
        overwrite_NA_msg <- FALSE
        insight::format_alert(
          "Missing values in original variable are overwritten by default value. If you want to preserve missing values, set `preserve_na = TRUE`." # nolint
        )
      }
    }
  }

  out
}

.number_to_text <- function(x) {
  if (is.null(x) || is.na(x)) {
    return("")
  }
  if (x == 1) {
    "first"
  } else if (x == 2) {
    "second"
  } else if (x == 3) {
    "third"
  } else if (x == 4) {
    "fourth"
  } else if (x == 5) {
    "fifth"
  } else if (x == 21) {
    "twenty-first"
  } else if (x == 22) {
    "twenty-second"
  } else if (x == 23) {
    "twenty-third"
  } else {
    paste0(x, "th")
  }
}
