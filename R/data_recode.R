#' @title Recode old values of variables into new values
#' @name data_recode
#'
#' @description
#' This functions recodes old values into new values and can be used to to
#' recode numeric or character vectors, or factors.
#'
#' @param x A data frame, numeric or character vector, or factor.
#' @param recodes A list of named vectors, which indicate the recode pairs.
#'   The _names_ of the list-elements (i.e. the left-hand side) represent the
#'   old values, while the values of the list-elements indicate the values.
#'   When recoding numeric vectors, element names have to be surrounded in
#'   backticks. For example, ``recodes = list(`1` = 0)`` would recode all
#'   `1` into `0` in a numeric vector. For factors, backticks are only required
#'   when the left-hand side contains multiple, comma separated values, e.g.
#'   ``recodes = list(a = "x", `b, c` = "y")``. See also 'Examples'.
#' @param ... not used.
#' @inheritParams find_columns
#' @inheritParams data_cut
#'
#' @return `x`, where old values are replaced by new values.
#'
#' @details
#' The `recodes` argument provides some shortcuts, in particular when recoding
#' numeric values.
#'
#' - Single values
#'
#'   Single values either need to be wrapped in backticks (in case of numeric
#'   values) or "as is" (for character or factor levels). Example:
#'   ``recodes=list(`1`=0,`2`=1)`` would recode 1 into 0, and 2 into 1.
#'   For factors or character vectors, an example is:
#'   `recodes=list(a="x",b="y")`.
#'
#' - Multiple values
#'
#'   Multiple values that should be recoded into a new value can be separated
#'   with comma. Example: ``recodes=list(`1,4`=1,`2,3`=2)`` would recode the
#'   values 1 and 4 into 1, and 2 and 3 into 2. For factors or character
#'   vectors, an example is: ``recodes=list(`a,b`="x",`c,d`="y")``.
#'
#' - Value range
#'
#'   Numeric value ranges can be defined using the `:`. Example:
#'   ``recodes=list(`1:3`=1,`4:6`=2)`` would recode all values from 1 to 3 into
#'   1, and 4 to 6 into 2.
#'
#' - `min` and `max`
#'
#'   placeholder to use the minimum or maximum value of the
#'   (numeric) variable. Useful, e.g., when recoding ranges of values.
#'   Example: ``recodes=list(`min:10`=1,`11:max`=2)``.
#'
#' - `else`
#'
#'   defines the default value for all values that have no match in the
#'   recode-pairs. Example: ``recodes=list(`1,2`=1,`3:4`=2,`else`=9)`` would
#'   recode values 1 and 2 into 1, 3 and 4 into 2, and all other values (no
#'   matter if missing or any numeric value other than 1 to 4) into 5.
#'
#' @examples
#' # numeric
#' set.seed(123)
#' x <- sample(c(1:4, NA), 15, TRUE)
#' table(x, useNA = "always")
#'
#' out <- data_recode(x, list(`1` = 0, `2:3` = 1, `4` = 2))
#' out
#' table(out, useNA = "always")
#'
#' out <- data_recode(x, list(`1` = 0, `2:3` = 1, `4` = 2, `NA` = 9))
#' out
#' table(out, useNA = "always")
#'
#' out <- data_recode(x, list(`1` = 0, `2:3` = 1, `else` = 77))
#' out
#' table(out, useNA = "always")
#'
#'
#' # factors (character vectors are similar)
#' set.seed(123)
#' x <- as.factor(sample(c("a", "b", "c"), 15, TRUE))
#' table(x)
#'
#' out <- data_recode(x, list(a = "x", `b, c` = "y"))
#' out
#' table(out)
#'
#' out <- data_recode(x, list(a = "x", b = "y", c = "z"))
#' out
#' table(out)
#'
#' out <- data_recode(x, list(`b, c` = "y", `else` = 77))
#' out
#' table(out)
#'
#'
#' # data frames
#' set.seed(123)
#' d <- data.frame(
#'   x = sample(c(1:4, NA), 12, TRUE),
#'   y = as.factor(sample(c("a", "b", "c"), 12, TRUE)),
#'   stringsAsFactors = FALSE
#' )
#'
#' data_recode(
#'   d,
#'   recodes = list(`1` = 0, `2:3` = 1, `4` = 2, a = "x", `b, c` = "y"),
#'   force = TRUE,
#'   append = TRUE
#' )
#' @export
data_recode <- function(x, ...) {
  UseMethod("data_recode")
}


#' @export
data_recode.default <- function(x, ...) {
  return(x)
}


#' @rdname data_recode
#' @export
data_recode.numeric <- function(x, recodes = NULL, verbose = TRUE, ...) {

  # save
  original_x <- x

  # no missings
  valid <- stats::na.omit(x)

  # stop if all NA
  if (!length(valid)) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("Variable contains only missing values. No recoding carried out."), call. = FALSE)
    }
    return(original_x)
  }

  # check for "else" token
  if ("else" %in% names(recodes)) {
    else_token <- recodes[["else"]]
    recodes["else"] <- NULL

    # set the default value for all values that have no match
    # (i.e. that should not be recoded)
    if (else_token == "copy") {
      x <- original_x
    } else {
      x <- rep(as.numeric(else_token), length = length(x))
    }
  }

  for (i in names(recodes)) {
    # name of list element is old value
    value_string <- i

    # replace placeholder
    value_string <- gsub("min", min(x, na.rm = TRUE), value_string)
    value_string <- gsub("max", max(x, na.rm = TRUE), value_string)

    # mimic vector
    if (!grepl("c(", value_string, fixed = TRUE)) {
      value_string <- paste0("c(", value_string, ")")
    }

    # parse old values, which are strings (names of element), but which should
    # contain values, like "1:10" or "1, 2, 3, 4". These should now be in the
    # format "c(1, 2, 3, 4)" or "c(1:10)", and it should be possible to parse
    # and evaluate these strings into a numeric vector
    old_values <- tryCatch(eval(parse(text = value_string)), error = function(e) NULL)

    if (!is.null(old_values)) {
      x[which(original_x %in% old_values)] <- as.numeric(recodes[[i]])
    }
  }

  # set back variable labels, remove value labels
  # (these are most likely not matching anymore)
  attr(x, "label") <- attr(original_x, "label", exact = TRUE)
  attr(x, "labels") <- NULL

  x
}


#' @export
data_recode.factor <- function(x, recodes = NULL, verbose = TRUE, ...) {

  # save
  original_x <- x

  # no missings
  valid <- stats::na.omit(x)

  # stop if all NA
  if (!length(valid)) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("Variable contains only missing values. No recoding carried out."), call. = FALSE)
    }
    return(original_x)
  }

  # as character, so recoding works
  x <- as.character(x)

  # check for "else" token
  if ("else" %in% names(recodes)) {
    else_token <- recodes[["else"]]
    recodes["else"] <- NULL

    # set the default value for all values that have no match
    # (i.e. that should not be recoded)
    if (else_token == "copy") {
      x <- as.character(original_x)
    } else {
      x <- rep(as.character(else_token), length = length(x))
    }
  }

  for (i in names(recodes)) {
    # name of list element is old value
    value_string <- paste(deparse(insight::trim_ws(unlist(strsplit(i, ",", fixed = TRUE)))), collapse = ",")

    # parse old values, which are strings (names of element), but which should
    # contain values, like "a" or "a, b, c". These should now be in the
    # format "c("a", "b", "c")" and it should be possible to parse
    # and evaluate these strings into a numeric vector
    old_values <- tryCatch(eval(parse(text = value_string)), error = function(e) NULL)

    if (!is.null(old_values)) {
      x[which(original_x %in% old_values)] <- recodes[[i]]
    }
  }

  # make sure we have correct new levels
  x <- droplevels(as.factor(x))

  # set back variable labels, remove value labels
  # (these are most likely not matching anymore)
  attr(x, "label") <- attr(original_x, "label", exact = TRUE)
  attr(x, "labels") <- NULL

  x
}


#' @export
data_recode.character <- function(x, recodes = NULL, verbose = TRUE, ...) {

  # save
  original_x <- x

  # no missings
  valid <- stats::na.omit(x)

  # stop if all NA
  if (!length(valid)) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("Variable contains only missing values. No recoding carried out."), call. = FALSE)
    }
    return(original_x)
  }

  # check for "else" token
  if ("else" %in% names(recodes)) {
    else_token <- recodes[["else"]]
    recodes["else"] <- NULL

    # set the default value for all values that have no match
    # (i.e. that should not be recoded)
    if (else_token == "copy") {
      x <- original_x
    } else {
      x <- rep(as.character(else_token), length = length(x))
    }
  }

  for (i in names(recodes)) {
    # name of list element is old value
    value_string <- paste(deparse(insight::trim_ws(unlist(strsplit(i, ",", fixed = TRUE)))), collapse = ",")

    # parse old values, which are strings (names of element), but which should
    # contain values, like "a" or "a, b, c". These should now be in the
    # format "c("a", "b", "c")" and it should be possible to parse
    # and evaluate these strings into a numeric vector
    old_values <- tryCatch(eval(parse(text = value_string)), error = function(e) NULL)

    if (!is.null(old_values)) {
      x[which(original_x %in% old_values)] <- recodes[[i]]
    }
  }

  # set back variable labels, remove value labels
  # (these are most likely not matching anymore)
  attr(x, "label") <- attr(original_x, "label", exact = TRUE)
  attr(x, "labels") <- NULL

  x
}


#' @rdname data_recode
#' @export
data_recode.data.frame <- function(x,
                                   recodes = NULL,
                                   force = FALSE,
                                   append = FALSE,
                                   select = NULL,
                                   exclude = NULL,
                                   ignore_case = FALSE,
                                   verbose = TRUE,
                                   ...) {
  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)

  # process arguments
  args <- .process_std_args(x, select, exclude, weights = NULL, append, append_suffix = "_r", force)

  # update processed arguments
  x <- args$x
  select <- args$select

  x[select] <- lapply(x[select], data_recode, recodes = recodes, verbose = verbose, ...)
  x
}

