#' @title Recode old values of variables into new values
#' @name recode_values
#'
#' @description
#' This functions recodes old values into new values and can be used to to
#' recode numeric or character vectors, or factors.
#'
#' @param x A data frame, numeric or character vector, or factor.
#' @param recode A list of named vectors, which indicate the recode pairs.
#'   The _names_ of the list-elements (i.e. the left-hand side) represent the
#'   _new_ values, while the values of the list-elements indicate the original
#'   (old) values that should be replaced. When recoding numeric vectors,
#'   element names have to be surrounded in backticks. For example,
#'   ``recode=list(`0`=1)`` would recode all `1` into `0` in a numeric
#'   vector. See also 'Examples' and 'Details'.
#' @param default Defines the default value for all values that have
#'   no match in the recode-pairs. Note that, if `preserve_na=FALSE`, missing
#'   values (`NA`) are also captured by the `default` argument, and thus will
#'   also be recoded into the specified value. See 'Examples' and 'Details'.
#' @param preserve_na Logical, if `TRUE`, `NA` (missing values) are preserved.
#'   This overrides any other arguments, including `default`. Hence, if
#'   `preserve_na=TRUE`, `default` will no longer convert `NA` into the specified
#'   default value.
#' @param ... not used.
#' @inheritParams find_columns
#' @inheritParams categorize
#'
#' @return `x`, where old values are replaced by new values.
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @inherit data_rename seealso
#'
#' @note You can use `options(data_recode_pattern = "old=new")` to switch the
#' behaviour of the `recode`-argument, i.e. recode-pairs are now following the
#' pattern `old values = new values`, e.g. if `getOption("data_recode_pattern")`
#' is set to `"old=new"`, then ``recode(`1`=0)`` would recode all 1 into 0.
#' The default for ``recode(`1`=0)`` is to recode all 0 into 1.
#'
#' @details
#' This section describes the pattern of the `recode` arguments, which also
#' provides some shortcuts, in particular when recoding numeric values.
#'
#' - Single values
#'
#'   Single values either need to be wrapped in backticks (in case of numeric
#'   values) or "as is" (for character or factor levels). Example:
#'   ``recode=list(`0`=1,`1`=2)`` would recode 1 into 0, and 2 into 1.
#'   For factors or character vectors, an example is:
#'   `recode=list(x="a",y="b")` (recode "a" into "x" and "b" into "y").
#'
#' - Multiple values
#'
#'   Multiple values that should be recoded into a new value can be separated
#'   with comma. Example: ``recode=list(`1`=c(1,4),`2`=c(2,3))`` would recode the
#'   values 1 and 4 into 1, and 2 and 3 into 2. It is also possible to define  the
#'   old values as a character string, like:  ``recode=list(`1`="1,4",`2`="2,3")``
#'   For factors or character vectors, an example is:
#'   ``recode=list(x=c("a","b"),y=c("c","d"))``.
#'
#' - Value range
#'
#'   Numeric value ranges can be defined using the `:`. Example:
#'   ``recode=list(`1`=1:3,`2`=4:6)`` would recode all values from 1 to 3 into
#'   1, and 4 to 6 into 2.
#'
#' - `min` and `max`
#'
#'   placeholder to use the minimum or maximum value of the
#'   (numeric) variable. Useful, e.g., when recoding ranges of values.
#'   Example: ``recode=list(`1`="min:10",`2`="11:max")``.
#'
#' - `default` values
#'
#'   The `default` argument defines the default value for all values that have
#'   no match in the recode-pairs. For example,
#'   ``recode=list(`1`=c(1,2),`2`=c(3,4)), default=9`` would
#'   recode values 1 and 2 into 1, 3 and 4 into 2, and all other values into 9.
#'   If `preserve_na` is set to `FALSE`, `NA` (missing values) will also be
#'   recoded into the specified default value.
#'
#' - Reversing and rescaling
#'
#'   See [reverse()] and [rescale()].
#'
#' @examples
#' # numeric ----------
#' set.seed(123)
#' x <- sample(c(1:4, NA), 15, TRUE)
#' table(x, useNA = "always")
#'
#' out <- recode_values(x, list(`0` = 1, `1` = 2:3, `2` = 4))
#' out
#' table(out, useNA = "always")
#'
#' # to recode NA values, set preserve_na to FALSE
#' out <- recode_values(
#'   x,
#'   list(`0` = 1, `1` = 2:3, `2` = 4, `9` = NA),
#'   preserve_na = FALSE
#' )
#' out
#' table(out, useNA = "always")
#'
#' # preserve na ----------
#' out <- recode_values(x, list(`0` = 1, `1` = 2:3), default = 77)
#' out
#' table(out, useNA = "always")
#'
#' # recode na into default ----------
#' out <- recode_values(
#'   x,
#'   list(`0` = 1, `1` = 2:3),
#'   default = 77,
#'   preserve_na = FALSE
#' )
#' out
#' table(out, useNA = "always")
#'
#'
#' # factors (character vectors are similar) ----------
#' set.seed(123)
#' x <- as.factor(sample(c("a", "b", "c"), 15, TRUE))
#' table(x)
#'
#' out <- recode_values(x, list(x = "a", y = c("b", "c")))
#' out
#' table(out)
#'
#' out <- recode_values(x, list(x = "a", y = "b", z = "c"))
#' out
#' table(out)
#'
#' out <- recode_values(x, list(y = "b,c"), default = 77)
#' # same as
#' # recode_values(x, list(y = c("b", "c")), default = 77)
#' out
#' table(out)
#'
#'
#' # data frames ----------
#' set.seed(123)
#' d <- data.frame(
#'   x = sample(c(1:4, NA), 12, TRUE),
#'   y = as.factor(sample(c("a", "b", "c"), 12, TRUE)),
#'   stringsAsFactors = FALSE
#' )
#'
#' recode_values(
#'   d,
#'   recode = list(`0` = 1, `1` = 2:3, `2` = 4, x = "a", y = c("b", "c")),
#'   append = TRUE
#' )
#'
#'
#' # switch recode pattern to "old=new" ----------
#' options(data_recode_pattern = "old=new")
#'
#' # numeric
#' set.seed(123)
#' x <- sample(c(1:4, NA), 15, TRUE)
#' table(x, useNA = "always")
#'
#' out <- recode_values(x, list(`1` = 0, `2:3` = 1, `4` = 2))
#' table(out, useNA = "always")
#'
#' # factors (character vectors are similar)
#' set.seed(123)
#' x <- as.factor(sample(c("a", "b", "c"), 15, TRUE))
#' table(x)
#'
#' out <- recode_values(x, list(a = "x", `b, c` = "y"))
#' table(out)
#'
#' # reset options
#' options(data_recode_pattern = NULL)
#' @export
recode_values <- function(x, ...) {
  UseMethod("recode_values")
}


#' @export
recode_values.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert(
      paste0("Variables of class `", class(x)[1], "` can't be recoded and remain unchanged.")
    )
  }
  return(x)
}


#' @rdname recode_values
#' @export
recode_values.numeric <- function(x,
                                  recode = NULL,
                                  default = NULL,
                                  preserve_na = TRUE,
                                  verbose = TRUE,
                                  ...) {
  # save
  original_x <- x

  # check arguments
  if (!.recode_args_ok(x, recode, verbose)) {
    return(x)
  }

  # recode-pattern option
  pattern <- getOption("data_recode_pattern")

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

  for (i in names(recode)) {
    # based on option-settings, the recode-argument can either follow the
    # pattern "new=old", or "old=new"

    if (identical(pattern, "old=new")) {
      # pattern: old = new, name of list element is old value
      old_values <- i
      new_values <- recode[[i]]
    } else {
      # pattern: new = old, name of list element is new value
      old_values <- recode[[i]]
      new_values <- i
    }

    if (is.character(old_values)) {
      # replace placeholder
      old_values <- gsub("min", min(x, na.rm = TRUE), old_values, fixed = TRUE)
      old_values <- gsub("max", max(x, na.rm = TRUE), old_values, fixed = TRUE)

      # mimic vector
      if (length(old_values) == 1 && !grepl("c(", old_values, fixed = TRUE)) {
        old_values <- paste0("c(", old_values, ")")
      }

      # parse old values, which are strings (names of element), but which should
      # contain values, like "1:10" or "1, 2, 3, 4". These should now be in the
      # format "c(1, 2, 3, 4)" or "c(1:10)", and it should be possible to parse
      # and evaluate these strings into a numeric vector
      old_values <- tryCatch(eval(parse(text = old_values)), error = function(e) NULL)
    }

    if (!is.null(old_values) && (is.numeric(old_values) || is.na(old_values))) {
      x[which(original_x %in% old_values)] <- as.numeric(new_values)
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
recode_values.factor <- function(x,
                                 recode = NULL,
                                 default = NULL,
                                 preserve_na = TRUE,
                                 verbose = TRUE,
                                 ...) {
  # save
  original_x <- x

  # check arguments
  if (!.recode_args_ok(x, recode, verbose)) {
    return(x)
  }

  # recode-pattern option
  pattern <- getOption("data_recode_pattern")

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

  for (i in names(recode)) {
    # based on option-settings, the recode-argument can either follow the
    # pattern "new=old", or "old=new"

    if (identical(pattern, "old=new")) {
      # pattern: old = new
      # name of list element is old value

      old_values <- paste(
        deparse(insight::trim_ws(unlist(strsplit(i, ",", fixed = TRUE), use.names = FALSE))),
        collapse = ","
      )

      # parse old values, which are strings (names of element), but which should
      # contain values, like "a" or "a, b, c". These should now be in the
      # format "c("a", "b", "c")" and it should be possible to parse
      # and evaluate these strings into a numeric vector
      old_values <- tryCatch(eval(parse(text = old_values)), error = function(e) NULL)

      # recode
      x[which(original_x %in% old_values)] <- recode[[i]]
    } else {
      # pattern: new = old
      # name of list element is new value

      old_values <- as.character(recode[[i]])
      # check input style: "a, b, c"
      if (length(old_values) == 1 && grepl(",", old_values, fixed = TRUE)) {
        # split and make character vector
        old_values <- insight::trim_ws(unlist(strsplit(old_values, ",", fixed = TRUE), use.names = FALSE))
      }
      # recode
      if (identical(i, "NA")) {
        x[which(original_x %in% old_values)] <- NA_character_
      } else {
        x[which(original_x %in% old_values)] <- as.character(i)
      }
    }
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
recode_values.character <- function(x,
                                    recode = NULL,
                                    default = NULL,
                                    preserve_na = TRUE,
                                    verbose = TRUE,
                                    ...) {
  # save
  original_x <- x

  # check arguments
  if (!.recode_args_ok(x, recode, verbose)) {
    return(x)
  }

  # recode-pattern option
  pattern <- getOption("data_recode_pattern")

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

  for (i in names(recode)) {
    # based on option-settings, the recode-argument can either follow the
    # pattern "new=old", or "old=new"

    if (identical(pattern, "old=new")) {
      # pattern: old = new
      # name of list element is old value

      # name of list element is old value
      value_string <- paste(
        deparse(insight::trim_ws(unlist(strsplit(i, ",", fixed = TRUE), use.names = FALSE))),
        collapse = ","
      )

      # parse old values, which are strings (names of element), but which should
      # contain values, like "a" or "a, b, c". These should now be in the
      # format "c("a", "b", "c")" and it should be possible to parse
      # and evaluate these strings into a numeric vector
      old_values <- tryCatch(eval(parse(text = value_string)), error = function(e) NULL)

      # recode
      x[which(original_x %in% old_values)] <- recode[[i]]
    } else {
      # pattern: new = old
      # name of list element is new value

      old_values <- as.character(recode[[i]])
      # check input style: "a, b, c"
      if (length(old_values) == 1 && grepl(",", old_values, fixed = TRUE)) {
        # split and make character vector
        old_values <- insight::trim_ws(unlist(strsplit(old_values, ",", fixed = TRUE), use.names = FALSE))
      }
      # recode
      if (identical(i, "NA")) {
        x[which(original_x %in% old_values)] <- NA_character_
      } else {
        x[which(original_x %in% old_values)] <- as.character(i)
      }
    }
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


#' @rdname recode_values
#' @export
recode_values.data.frame <- function(x,
                                     select = NULL,
                                     exclude = NULL,
                                     recode = NULL,
                                     default = NULL,
                                     preserve_na = TRUE,
                                     append = FALSE,
                                     ignore_case = FALSE,
                                     regex = FALSE,
                                     verbose = TRUE,
                                     ...) {
  # evaluate arguments
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  # process arguments
  args <- .process_std_args(x, select, exclude, weights = NULL, append, append_suffix = "_r", force = TRUE)

  # update processed arguments
  x <- args$x
  select <- args$select

  x[select] <- lapply(
    x[select],
    recode_values,
    recode = recode,
    default = default,
    preserve_na = preserve_na,
    verbose = verbose,
    ...
  )

  x
}


# utils --------------------------

.recode_args_ok <- function(x, recode, verbose) {
  ok <- TRUE
  # no missings
  valid <- stats::na.omit(x)

  # skip if all NA
  if (!length(valid)) {
    if (isTRUE(verbose)) {
      insight::format_warning("Variable contains only missing values. No recoding carried out.")
    }
    ok <- FALSE
  }

  # warn if not a list
  if (!is.list(recode) || is.null(names(recode))) {
    if (isTRUE(verbose)) {
      insight::format_warning("`recode` needs to be a (named) list. No recoding carried out.")
    }
    ok <- FALSE
  }

  ok
}


## TODO Deprecate and remove alias later

#' @rdname recode_values
#' @export
change_code <- recode_values
