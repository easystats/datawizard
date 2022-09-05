# styler: off
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
#' @param filter A logical expression indicating which rows to keep, or a numeric
#'   vector indicating the row indices of rows to keep. Can also be a string
#'   representation of a logical expression. e.g. `filter = "x > 4"`. This might
#'   be useful when used in packages to avoid defining undefined global variables.
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
#' @param ... Not used.
#'
#' @return A filtered data frame, or the row indices that match the specified configuration.
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
#' # Define a custom function containing data_filter() and pass variable names
#' # to it using curly brackets
#' my_filter <- function(data, variable) {
#'   data_filter(data, {variable} <= 20)
#' }
#' my_filter(mtcars, "mpg")
#'
#' # Pass complete filter-condition as string
#' my_filter <- function(data, condition) {
#'   data_filter(data, {condition})
#' }
#' my_filter(mtcars, "am != 0")
#'
#' # string can also be used directly as argument
#' data_filter(mtcars, "am != 0")
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
    "&" = ,
    "&&" = ,
    "and" = "and",
    "!" = ,
    "not" = "not",
    "or"
  )

  # sanity check
  shared_columns <- intersect(colnames(x), colnames(to))
  if (is.null(shared_columns) || length(shared_columns) == 0) {
    stop(insight::format_message(
      "None of the columns from the data frame with matching conditions were found in `x`."
    ), call. = FALSE)
  }

  # only select common columns
  x <- x[shared_columns]

  # prepare
  if (identical(match, "or")) {
    idx <- c()
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

  out
}



#' @rdname data_match
#' @export
data_filter <- function(x, filter, ...) {
  condition <- substitute(filter)

  # condition can be a numeric vector, to slice rows by indices,
  # or a logical condition to filter observations. first, we check
  # for numeric vector. the logical condition can also be passed
  # as character vector, which allows to use data_filer() from inside
  # other function w/o the need to define "globalVariables".

  # numeric vector to slice data frame?
  rows <- try(eval(condition, envir = parent.frame()), silent = TRUE)
  if (is.numeric(rows)) {
    out <- x[rows, , drop = FALSE]
  } else {
    if (!is.character(condition)) {
      condition <- insight::safe_deparse(condition)
    }
    # Check syntax of the filter. Must be done *before* calling subset()
    # (cf easystats/datawizard#237)
    .check_filter_syntax(condition)

    has_curley <- grepl("{", condition, fixed = TRUE)

    if (has_curley) {
      condition <- gsub("\\{ ", "\\{", condition)
      condition <- gsub(" \\}", "\\}", condition)

      curley_vars <- regmatches(condition, gregexpr("[^{\\}]+(?=\\})", condition, perl = TRUE))
      curley_vars <- unlist(curley_vars)

      for (i in curley_vars) {
        token <- get(i, envir = parent.frame())
        condition <- gsub(paste0("{", i, "}"), token, condition, fixed = TRUE)
      }
    }

    out <- tryCatch(
      subset(x, subset = eval(parse(text = condition), envir = new.env())),
      warning = function(e) NULL,
      error = function(e) NULL
    )
    if (is.null(out)) {
      stop(insight::format_message(
        "Filtering did not work. Please check the syntax of your `filter` argument."
      ), call. = FALSE)
    }
  }
  # restore value and variable labels
  for (i in colnames(out)) {
    attr(out[[i]], "label") <- attr(x[[i]], "label", exact = TRUE)
    attr(out[[i]], "labels") <- attr(x[[i]], "labels", exact = TRUE)
  }
  out
}


# helper -------------------

.check_filter_syntax <- function(condition) {
  # NOTE: We cannot check for `=` when "filter" is not a character vector
  # because the function will then fail in general. I.e.,
  # "data_filter(mtcars, filter = mpg > 10 & cyl = 4)" will not start
  # running this function and never reaches the first code line,
  # but immediately stops...
  tmp <- gsub("==", "", condition, fixed = TRUE)
  tmp <- gsub("<=", "", tmp, fixed = TRUE)
  tmp <- gsub(">=", "", tmp, fixed = TRUE)
  tmp <- gsub("!=", "", tmp, fixed = TRUE)

  # Give more informative message to users
  # about possible misspelled comparisons / logical conditions
  # check if "=" instead of "==" was used?
  if (any(grepl("=", tmp, fixed = TRUE))) {
    stop(insight::format_message(
      "Filtering did not work. Please check if you need `==` (instead of `=`) for comparison."
    ), call. = FALSE)
  }
  # check if "&&" etc instead of "&" was used?
  logical_operator <- NULL
  if (any(grepl("&&", condition, fixed = TRUE))) {
    logical_operator <- "&&"
  }
  if (any(grepl("||", condition, fixed = TRUE))) {
    logical_operator <- "||"
  }
  if (!is.null(logical_operator)) {
    stop(insight::format_message(
      paste0(
        "Filtering did not work. Please check if you need `",
        substr(logical_operator, 0, 1),
        "` (instead of `", logical_operator, "`) as logical operator."
      )
    ), call. = FALSE)
  }
}
# styler: on