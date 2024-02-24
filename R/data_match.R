#' Return filtered or sliced data frame, or row indices
#'
#' Return a filtered (or sliced) data frame or row indices of a data frame that
#' match a specific condition. `data_filter()` works like `data_match()`, but works
#' with logical expressions or row indices of a data frame to specify matching
#' conditions.
#'
#' @param x A data frame.
#' @param to A data frame matching the specified conditions. Note that if
#'   `match` is a value other than `"and"`, the original row order might be
#'   changed. See 'Details'.
#' @param match String, indicating with which logical operation matching
#'   conditions should be combined. Can be `"and"` (or `"&"`), `"or"` (or `"|"`)
#'   or `"not"` (or `"!"`).
#' @param return_indices Logical, if `FALSE`, return the vector of rows that
#'   can be used to filter the original data frame. If `FALSE` (default),
#'   returns directly the filtered data frame instead of the row indices.
#' @param drop_na Logical, if `TRUE`, missing values (`NA`s) are removed before
#'   filtering the data. This is the default behaviour, however, sometimes when
#'   row indices are requested (i.e. `return_indices=TRUE`), it might be useful
#'   to preserve `NA` values, so returned row indices match the row indices of
#'   the original data frame.
#' @param ... A sequence of logical expressions indicating which rows to keep,
#'   or a numeric vector indicating the row indices of rows to keep. Can also be
#'   a string representation of a logical expression (e.g. `"x > 4"`), a
#'   character vector (e.g. `c("x > 4", "y == 2")`) or a variable that contains
#'   the string representation of a logical expression. These might be useful
#'   when used in packages to avoid defining undefined global variables.
#'
#' @return A filtered data frame, or the row indices that match the specified
#' configuration.
#'
#' @details For `data_match()`, if `match` is either `"or"` or `"not"`, the
#' original row order from `x` might be changed. If preserving row order is
#' required, use `data_filter()` instead.
#'
#' ```
#' # mimics subset() behaviour, preserving original row order
#' head(data_filter(mtcars[c("mpg", "vs", "am")], vs == 0 | am == 1))
#' #>                    mpg vs am
#' #> Mazda RX4         21.0  0  1
#' #> Mazda RX4 Wag     21.0  0  1
#' #> Datsun 710        22.8  1  1
#' #> Hornet Sportabout 18.7  0  0
#' #> Duster 360        14.3  0  0
#' #> Merc 450SE        16.4  0  0
#'
#' # re-sorting rows
#' head(data_match(mtcars[c("mpg", "vs", "am")],
#'                 data.frame(vs = 0, am = 1),
#'                 match = "or"))
#' #>                    mpg vs am
#' #> Mazda RX4         21.0  0  1
#' #> Mazda RX4 Wag     21.0  0  1
#' #> Hornet Sportabout 18.7  0  0
#' #> Duster 360        14.3  0  0
#' #> Merc 450SE        16.4  0  0
#' #> Merc 450SL        17.3  0  0
#' ```
#'
#' While `data_match()` works with data frames to match conditions against,
#' `data_filter()` is basically a wrapper around `subset(subset = <filter>)`.
#' However, unlike `subset()`, it preserves label attributes and is useful when
#' working with labelled data.
#'
#' @examples
#' data_match(mtcars, data.frame(vs = 0, am = 1))
#' data_match(mtcars, data.frame(vs = 0, am = c(0, 1)))
#'
#' # observations where "vs" is NOT 0 AND "am" is NOT 1
#' data_match(mtcars, data.frame(vs = 0, am = 1), match = "not")
#' # equivalent to
#' data_filter(mtcars, vs != 0 & am != 1)
#'
#' # observations where EITHER "vs" is 0 OR "am" is 1
#' data_match(mtcars, data.frame(vs = 0, am = 1), match = "or")
#' # equivalent to
#' data_filter(mtcars, vs == 0 | am == 1)
#'
#' # slice data frame by row indices
#' data_filter(mtcars, 5:10)
#'
#' # Define a custom function containing data_filter()
#' my_filter <- function(data, variable) {
#'   data_filter(data, variable)
#' }
#' my_filter(mtcars, "cyl == 6")
#'
#' # Pass complete filter-condition as string.
#' my_filter <- function(data, condition) {
#'   data_filter(data, condition)
#' }
#' my_filter(mtcars, "am != 0")
#'
#' # string can also be used directly as argument
#' data_filter(mtcars, "am != 0")
#'
#' # or as variable
#' fl <- "am != 0"
#' data_filter(mtcars, fl)
#' @inherit data_rename seealso
#' @export
data_match <- function(x, to, match = "and", return_indices = FALSE, drop_na = TRUE, ...) {
  if (!is.data.frame(to)) {
    to <- as.data.frame(to)
  }
  original_x <- x

  # evaluate
  match <- match.arg(tolower(match), c("and", "&", "&&", "or", "|", "||", "!", "not"))
  match <- switch(match,
    `&` = ,
    `&&` = ,
    and = "and",
    `!` = ,
    not = "not",
    "or"
  )

  # validation check
  shared_columns <- intersect(colnames(x), colnames(to))
  if (is.null(shared_columns) || length(shared_columns) == 0L) {
    insight::format_error(
      "None of the columns from the data frame with matching conditions were found in `x`."
    )
  }

  # only select common columns
  x <- x[shared_columns]

  # prepare
  if (identical(match, "or")) {
    idx <- vector("numeric", length = 0L)
  } else {
    # remove missings before matching
    if (isTRUE(drop_na)) {
      x <- x[stats::complete.cases(x), , drop = FALSE]
    }
    idx <- seq_len(nrow(x))
  }

  # Find matching rows
  for (col in names(to)) {
    values <- x[[col]]
    if (match == "or") {
      idx <- union(idx, which(values %in% to[[col]]))
    } else if (match == "not") {
      idx <- idx[!values[idx] %in% to[[col]]]
    } else {
      idx <- idx[values[idx] %in% to[[col]]]
    }
  }

  # prepare output
  if (isFALSE(return_indices)) {
    out <- original_x[idx, , drop = FALSE]
    # restore value and variable labels
    for (i in colnames(out)) {
      attr(out[[i]], "label") <- attr(original_x[[i]], "label", exact = TRUE)
      attr(out[[i]], "labels") <- attr(original_x[[i]], "labels", exact = TRUE)
    }
  } else {
    out <- idx
  }

  # add back custom attributes
  out <- .replace_attrs(out, attributes(original_x))
  out
}



#' @rdname data_match
#' @export
data_filter <- function(x, ...) {
  UseMethod("data_filter")
}

#' @export
data_filter.data.frame <- function(x, ...) {
  out <- x
  dots <- match.call(expand.dots = FALSE)[["..."]]

  if (any(nzchar(names(dots), keepNA = TRUE))) {
    insight::format_error(
      "Filtering did not work. Please check if you need `==` (instead of `=`) for comparison."
    )
  }

  # turn character vector (like `c("mpg <= 20", "cyl == 6")`) into symbols
  if (length(dots) == 1) {
    character_vector <- .dynEval(dots[[1]], ifnotfound = NULL)
    if (is.character(character_vector) && length(character_vector) > 1) {
      dots <- lapply(character_vector, str2lang)
    }
  }

  # Check syntax of the filter. Must be done *before* calling subset()
  # (cf easystats/datawizard#237)
  for (.fcondition in dots) {
    .check_filter_syntax(insight::safe_deparse(.fcondition))
  }

  for (i in seq_along(dots)) {
    # only proceed when result is still valid
    if (!is.null(out)) {
      symbol <- dots[[i]]
      # evaluate, we may have a variable with filter expression
      eval_symbol <- .dynEval(symbol, ifnotfound = NULL)
      # validation check: is variable named like a function?
      if (is.function(eval_symbol)) {
        eval_symbol <- .dynGet(symbol, ifnotfound = NULL)
      }
      eval_symbol_numeric <- NULL
      if (!is.null(eval_symbol)) {
        # when possible to evaluate, do we have a numeric vector provided
        # as string? (e.g. `"5:10"`) - then try to coerce to numeric
        eval_symbol_numeric <- tryCatch(eval(parse(text = eval_symbol)), error = function(e) NULL)
      }

      # here we go when we have a filter expression, and no numeric vector to slice
      if (is.null(eval_symbol) || (!is.numeric(eval_symbol) && !is.numeric(eval_symbol_numeric))) {
        # could be evaluated? Then filter expression is a string and we need
        # to convert into symbol
        if (is.character(eval_symbol)) {
          symbol <- str2lang(eval_symbol)
        }
        # filter data
        out <- tryCatch(
          subset(out, subset = eval(symbol, envir = new.env())),
          warning = function(e) e,
          error = function(e) e
        )
      } else if (is.numeric(eval_symbol)) {
        # if symbol could be evaluated and is numeric, slice
        out <- tryCatch(out[eval_symbol, , drop = FALSE], error = function(e) NULL)
      } else if (is.numeric(eval_symbol_numeric)) {
        # if symbol could be evaluated, was string and could be converted to numeric, slice
        out <- tryCatch(out[eval_symbol_numeric, , drop = FALSE], error = function(e) NULL)
      }

      if (inherits(out, "simpleError")) {
        error_msg <- out$message[1]
        # try to find out which variable was the cause for the error
        if (grepl("object '(.*)' not found", error_msg)) {
          error_var <- gsub("object '(.*)' not found", "\\1", error_msg)
          # some syntax errors do not relate to misspelled variables...
          if (!error_var %in% colnames(x)) {
            insight::format_error(
              paste0("Variable \"", error_var, "\" was not found in the dataset."),
              .misspelled_string(colnames(x), error_var, "Possibly misspelled?")
            )
          }
        }
        out <- NULL
      }
    }
  }

  if (is.null(out)) {
    insight::format_error(
      "Filtering did not work. Please check the syntax of your conditions."
    )
  }

  # restore value and variable labels
  for (i in colnames(out)) {
    attr(out[[i]], "label") <- attr(x[[i]], "label", exact = TRUE)
    attr(out[[i]], "labels") <- attr(x[[i]], "labels", exact = TRUE)
  }

  # add back custom attributes
  out <- .replace_attrs(out, attributes(x))
  out
}


#' @export
data_filter.grouped_df <- function(x, ...) {
  grps <- attr(x, "groups", exact = TRUE)
  grps <- grps[[".rows"]]

  dots <- match.call(expand.dots = FALSE)[["..."]]
  out <- lapply(grps, function(grp) {
    arguments <- list(x[grp, ])
    arguments <- c(arguments, dots)
    do.call("data_filter.data.frame", arguments)
  })

  out <- do.call(rbind, out)

  if (!insight::object_has_rownames(x)) {
    rownames(out) <- NULL
  }

  out
}


# helper -------------------

.check_filter_syntax <- function(.fcondition) {
  # NOTE: We cannot check for `=` when "filter" is not a character vector
  # because the function will then fail in general. I.e.,
  # "data_filter(mtcars, filter = mpg > 10 & cyl = 4)" will not start
  # running this function and never reaches the first code line,
  # but immediately stops...
  tmp <- gsub("==", "", .fcondition, fixed = TRUE)
  tmp <- gsub("<=", "", tmp, fixed = TRUE)
  tmp <- gsub(">=", "", tmp, fixed = TRUE)
  tmp <- gsub("!=", "", tmp, fixed = TRUE)

  # We want to check whether user used a "=" in the filter syntax. This
  # typically indicates that the comparison "==" is probably wrong by using
  # a "=" instead of `"=="`. However, if a function was provided, we indeed
  # may have "=", e.g. if the pattern was
  # `data_filter(out, grep("pattern", x = value))`. We thus first check if we
  # can identify a function call, and only continue checking for wrong syntax
  # when we have not identified a function.

  if (!is.function(try(get(gsub("^(.*?)\\((.*)", "\\1", tmp)), silent = TRUE))) {
    # Give more informative message to users
    # about possible misspelled comparisons / logical conditions
    # check if "=" instead of "==" was used?
    if (any(grepl("=", tmp, fixed = TRUE))) {
      insight::format_error(
        "Filtering did not work. Please check if you need `==` (instead of `=`) for comparison."
      )
    }
    # check if "&&" etc instead of "&" was used?
    logical_operator <- NULL
    if (any(grepl("&&", .fcondition, fixed = TRUE))) {
      logical_operator <- "&&"
    }
    if (any(grepl("||", .fcondition, fixed = TRUE))) {
      logical_operator <- "||"
    }
    if (!is.null(logical_operator)) {
      insight::format_error(
        paste0(
          "Filtering did not work. Please check if you need `",
          substr(logical_operator, 0, 1),
          "` (instead of `", logical_operator, "`) as logical operator."
        )
      )
    }
  }
}
