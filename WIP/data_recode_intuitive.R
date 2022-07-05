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
#'   _old_ values that should be replaced, while the values of the list-elements
#'   indicate the _new_ values . When recoding numeric vectors, element names
#'   have to be surrounded in backticks. For example, ``recodes=list(`1`=0)``
#'   would recode all `1` into `0` in a numeric vector. See also 'Examples' and
#'   'Details'.
#' @param default Defines the default value for all values that have no match
#'   in the recode-pairs. Note that, if `preserve_na=FALSE`, missing values
#'   (`NA`) are also captured by the `default` argument, and thus will also be
#'   recoded into the specified value. See 'Examples' and 'Details'.
#' @param preserve_na Logical, if `TRUE`, `NA` (missing values) are preserved.
#'   This overrides any other arguments, including `default`. Hence, if
#'   `preserve_na=TRUE`, `default` will no longer convert `NA` into the specified
#'   default value.
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
#'   ``recodes=list(`1`=0,`2`=1)`` would recode 1 into 0, and 2 into 1.
#'   For factors or character vectors, an example is:
#'   `recodes=list(a="x",b="y")` (recode "a" into "x" and "b" into "y").
#'
#' - Multiple values
#'
#'   Multiple values that should be recoded into a new value can be separated
#'   with comma. Example: ``recodes=list(`1,4`=1,`2,3`=2`` would recode the
#'   values 1 and 4 into 1, and 2 and 3 into 2. For factors or character vectors,
#'   an example is: ``recodes=list(`a,b`="x",`c,d`="y")``.
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
#' - `default` values
#'
#'   The `default` argument defines the default value for all values that have
#'   no match in the recode-pairs. For example,
#'   ``recodes=list(`1:2`=1,`3:4`=2), default=9`` would recode values 1 and 2
#'   into 1, 3 and 4 into 2, and all other values into 5. If `preserve_na` is
#'   set to `FALSE`, `NA` (missing values) will also be recoded into the
#'   specified default value.
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
#' out <- data_recode(x, list(`1` = 0, `2:3` = 1, `4` = 2))
#' out
#' table(out, useNA = "always")
#'
#' # to recode NA values, set preserve_na to FALSE
#' out <- data_recode(
#'   x,
#'   list(`1` = 0, `2:3` = 1, `4` = 2, `NA` = 9),
#'   preserve_na = FALSE
#' )
#' out
#' table(out, useNA = "always")
#'
#' # preserve na
#' out <- data_recode(x, list(`1` = 0, `2:3` = 1), default = 77)
#' out
#' table(out, useNA = "always")
#'
#' # recode na into default
#' out <- data_recode(
#'   x,
#'   list(`1` = 0, `2:3` = 1),
#'   default = 77,
#'   preserve_na = FALSE
#' )
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
#' out <- data_recode(x, list(`b, c` = "y"), default = 77)
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
data_recode.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    message(insight::format_message(paste0("Variables of class '", class(x)[1], "' can't be recoded and remain unchanged.")))
  }
  return(x)
}


#' @rdname data_recode
#' @export
data_recode.numeric <- function(x,
                                recodes = NULL,
                                default = NULL,
                                preserve_na = TRUE,
                                verbose = TRUE,
                                ...) {
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

  # make sure NAs are preserved after recoding
  missing_values <- NULL
  if (preserve_na) {
    missing_values <- is.na(x)
  }

  # check for "default" token
  if (!is.null(default)) {
    # set the default value for all values that have no match
    # (i.e. that should not be recoded)
    x <- rep(as.numeric(default), length = length(x))
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

    if (!is.null(old_values) && (is.numeric(old_values) || is.na(old_values))) {
      x[which(original_x %in% old_values)] <- as.numeric(recodes[[i]])
    }
  }

  # set back variable labels, remove value labels
  # (these are most likely not matching anymore)
  attr(x, "label") <- attr(original_x, "label", exact = TRUE)
  attr(x, "labels") <- NULL

  # set back missing values
  if (!is.null(missing_values)) {
    x[missing_values] <- NA
  }

  x
}


#' @export
data_recode.factor <- function(x,
                               recodes = NULL,
                               default = NULL,
                               preserve_na = TRUE,
                               verbose = TRUE,
                               ...) {
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

  # make sure NAs are preserved after recoding
  missing_values <- NULL
  if (preserve_na) {
    missing_values <- is.na(x)
  }

  # as character, so recoding works
  x <- as.character(x)

  # check for "default" token
  if (!is.null(default)) {
    # set the default value for all values that have no match
    # (i.e. that should not be recoded)
    x <- rep(as.character(default), length = length(x))
  }

  for (i in names(recodes)) {
    # name of list element is old value
    value_string <- paste(deparse(insight::trim_ws(unlist(strsplit(i, ",", fixed = TRUE)))), collapse = ",")

    # parse old values, which are strings (names of element), but which should
    # contain values, like "a" or "a, b, c". These should now be in the
    # format "c("a", "b", "c")" and it should be possible to parse
    # and evaluate these strings into a numeric vector
    old_values <- tryCatch(eval(parse(text = value_string)), error = function(e) NULL)

    # recode
    x[which(original_x %in% old_values)] <- recodes[[i]]
  }

  # set back missing values
  if (!is.null(missing_values)) {
    x[missing_values] <- NA_character_
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
data_recode.character <- function(x,
                                  recodes = NULL,
                                  default = NULL,
                                  preserve_na = TRUE,
                                  verbose = TRUE,
                                  ...) {
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

  # make sure NAs are preserved after recoding
  missing_values <- NULL
  if (preserve_na) {
    missing_values <- is.na(x)
  }

  # check for "default" token
  if (!is.null(default)) {
    # set the default value for all values that have no match
    # (i.e. that should not be recoded)
    x <- rep(as.character(default), length = length(x))
  }

  for (i in names(recodes)) {
    # name of list element is old value
    value_string <- paste(deparse(insight::trim_ws(unlist(strsplit(i, ",", fixed = TRUE)))), collapse = ",")

    # parse old values, which are strings (names of element), but which should
    # contain values, like "a" or "a, b, c". These should now be in the
    # format "c("a", "b", "c")" and it should be possible to parse
    # and evaluate these strings into a numeric vector
    old_values <- tryCatch(eval(parse(text = value_string)), error = function(e) NULL)

    # recode
    x[which(original_x %in% old_values)] <- recodes[[i]]
  }

  # set back variable labels, remove value labels
  # (these are most likely not matching anymore)
  attr(x, "label") <- attr(original_x, "label", exact = TRUE)
  attr(x, "labels") <- NULL

  # set back missing values
  if (!is.null(missing_values)) {
    x[missing_values] <- NA_character_
  }

  x
}


#' @rdname data_recode
#' @export
data_recode.data.frame <- function(x,
                                   recodes = NULL,
                                   default = NULL,
                                   preserve_na = TRUE,
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

  x[select] <- lapply(
    x[select],
    data_recode,
    recodes = recodes,
    default = default,
    preserve_na = preserve_na,
    verbose = verbose,
    ...
  )

  x
}
