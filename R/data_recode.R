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
#'   _new_ values, while the values of the list-elements indicate the original
#'   (old) values that should be replaced. When recoding numeric vectors,
#'   element names have to be surrounded in backticks. For example,
#'   ``recodes = list(`0` = 1)`` would recode all `1` into `0` in a numeric
#'   vector. See also 'Examples' and 'Details'.
#' @param ... not used.
#' @inheritParams find_columns
#' @inheritParams data_cut
#'
#' @return `x`, where old values are replaced by new values.
#'
#' @inherit data_rename seealso
#'
#' @details
#' This section describes the pattern of the `recodes` arguments, which also
#' provides some shortcuts, in particular when recoding numeric values.
#'
#' - Single values
#'
#'   Single values either need to be wrapped in backticks (in case of numeric
#'   values) or "as is" (for character or factor levels). Example:
#'   ``recodes=list(`0`=1,`1`=2)`` would recode 1 into 0, and 2 into 1.
#'   For factors or character vectors, an example is:
#'   `recodes=list(x="a",y="b")` (recode "a" into "x" and "b" into "y").
#'
#' - Multiple values
#'
#'   Multiple values that should be recoded into a new value can be separated
#'   with comma. Example: ``recodes=list(`1`=c(1,4),`2`=c(2,3))`` would recode the
#'   values 1 and 4 into 1, and 2 and 3 into 2. It is also possible to define  the
#'   old values as a character string, like:  ``recodes=list(`1`="1,4",`2`="2,3")``
#'   For factors or character vectors, an example is:
#'   ``recodes=list(x=c("a","b"),y=c("c","d"))``.
#'
#' - Value range
#'
#'   Numeric value ranges can be defined using the `:`. Example:
#'   ``recodes=list(`1`=1:3,`2`=4:6)`` would recode all values from 1 to 3 into
#'   1, and 4 to 6 into 2.
#'
#' - `min` and `max`
#'
#'   placeholder to use the minimum or maximum value of the
#'   (numeric) variable. Useful, e.g., when recoding ranges of values.
#'   Example: ``recodes=list(`1`="min:10",2="11:max")``.
#'
#' - `default` values
#'
#'   defines the default value for all values that have no match in the
#'   recode-pairs. Example: ``recodes=list(`1`=c(1,2),`2`=c(3,4),default=9)`` would
#'   recode values 1 and 2 into 1, 3 and 4 into 2, and all other values (no
#'   matter if missing or any numeric value other than 1 to 4) into 5.
#'
#' - Reversing and rescaling
#'
#'   See [data_reverse()] and [data_rescale()].
#'
#' @examples
#' # numeric
#' set.seed(123)
#' x <- sample(c(1:4, NA), 15, TRUE)
#' table(x, useNA = "always")
#'
#' out <- data_recode(x, list(`0` = 1, `1` = 2:3, `2` = 4))
#' out
#' table(out, useNA = "always")
#'
#' out <- data_recode(x, list(`0` = 1, `1` = 2:3, `2` = 4, `9` = NA))
#' out
#' table(out, useNA = "always")
#'
#' out <- data_recode(x, list(`0` = 1, `1` = 2:3, default = 77))
#' out
#' table(out, useNA = "always")
#'
#'
#' # factors (character vectors are similar)
#' set.seed(123)
#' x <- as.factor(sample(c("a", "b", "c"), 15, TRUE))
#' table(x)
#'
#' out <- data_recode(x, list(x = "a", y = c("b", "c")))
#' out
#' table(out)
#'
#' out <- data_recode(x, list(x = "a", y = "b", z = "c"))
#' out
#' table(out)
#'
#' out <- data_recode(x, list(y = "b,c", default = 77))
#' # same as
#' # data_recode(x, list(y = c("b", "c"), default = 77))
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
#'   recodes = list(`0` = 1, `1` = 2:3, `2` = 4, x = "a", y = c("b", "c")),
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

  # check for "default" token
  if ("default" %in% names(recodes)) {
    default_token <- recodes[["default"]]
    recodes["default"] <- NULL

    # set the default value for all values that have no match
    # (i.e. that should not be recoded)
    if (default_token == "copy") {
      x <- original_x
    } else {
      x <- rep(as.numeric(default_token), length = length(x))
    }
  }

  for (i in names(recodes)) {
    # name of list element is old value
    old_values <- recodes[[i]]

    if (is.character(old_values)) {
      # replace placeholder
      old_values <- gsub("min", min(x, na.rm = TRUE), old_values)
      old_values <- gsub("max", max(x, na.rm = TRUE), old_values)

      # mimic vector
      if (length(old_values) == 1 && !grepl("c(", old_values, fixed = TRUE)) {
        old_values <- paste0("c(", old_values, ")")
      }

      # parse old values, which can be strings, but which should contain values,
      # like "1:10" or "1, 2, 3, 4". These should now be in the format
      # "c(1, 2, 3, 4)" or "c(1:10)", and it should be possible to parse
      # and evaluate these strings into a numeric vector
      old_values <- tryCatch(eval(parse(text = old_values)), error = function(e) NULL)
    }

    if (!is.null(old_values) && (is.numeric(old_values) || is.na(old_values))) {
      x[which(original_x %in% old_values)] <- as.numeric(i)
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

  # check for "default" token
  if ("default" %in% names(recodes)) {
    default_token <- recodes[["default"]]
    recodes["default"] <- NULL

    # set the default value for all values that have no match
    # (i.e. that should not be recoded)
    if (default_token == "copy") {
      x <- as.character(original_x)
    } else {
      x <- rep(as.character(default_token), length = length(x))
    }
  }

  for (i in names(recodes)) {
    old_values <- as.character(recodes[[i]])
    # check input style: "a, b, c"
    if (length(old_values) == 1 && grepl(",", old_values, fixed = TRUE)) {
      # split and make character vector
      old_values <- insight::trim_ws(unlist(strsplit(old_values, ",", fixed = TRUE)))
    }
    # recode
    x[which(original_x %in% old_values)] <- as.character(i)
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

  # check for "default" token
  if ("default" %in% names(recodes)) {
    default_token <- recodes[["default"]]
    recodes["default"] <- NULL

    # set the default value for all values that have no match
    # (i.e. that should not be recoded)
    if (default_token == "copy") {
      x <- as.character(original_x)
    } else {
      x <- rep(as.character(default_token), length = length(x))
    }
  }

  for (i in names(recodes)) {
    old_values <- as.character(recodes[[i]])
    # check input style: "a, b, c"
    if (length(old_values) == 1 && grepl(",", old_values, fixed = TRUE)) {
      # split and make character vector
      old_values <- insight::trim_ws(unlist(strsplit(old_values, ",", fixed = TRUE)))
    }
    # recode
    x[which(original_x %in% old_values)] <- as.character(i)
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

