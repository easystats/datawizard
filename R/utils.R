#' @keywords internal
.get_model_info <- function(model, model_info = NULL, ...) {
  if (is.null(model_info)) model_info <- insight::model_info(model)

  model_info
}

#' Print a message saying that an argument is deprecated and that the user
#' should use its replacement instead.
#'
#' @param arg Argument that is deprecated
#' @param replacement Argument that replaces the deprecated argument
#' @keywords internal
.is_deprecated <- function(arg, replacement) {
  insight::format_warning(
    paste0("Argument `", arg, "` is deprecated. Please use `", replacement, "` instead.")
  )
}

#' `NULL` coalescing operator
#'
#' @keywords internal
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' Try to convert object to a dataframe
#'
#' @keywords internal
#' @noRd
.coerce_to_dataframe <- function(data) {
  if (!is.data.frame(data)) {
    data <- tryCatch(
      as.data.frame(data, stringsAsFactors = FALSE),
      error = function(e) {
        insight::format_error(
          "`data` must be a data frame, or an object that can be coerced to a data frame."
        )
      }
    )
  }
  data
}


#' Fuzzy grep, matches pattern that are close, but not identical
#' Example:
#' colnames(iris)
#' p <- sprintf("(%s){~%i}", "Spela", 2)
#' grep(pattern = p, x = colnames(iris), ignore.case = FALSE)
#' @keywords internal
#' @noRd

.fuzzy_grep <- function(x, pattern, precision = NULL) {
  if (is.null(precision)) {
    precision <- round(nchar(pattern) / 3)
  }
  if (precision > nchar(pattern)) {
    return(NULL)
  }
  p <- sprintf("(%s){~%i}", pattern, precision)
  grep(pattern = p, x = x, ignore.case = FALSE)
}


#' create a message string to tell user about matches that could possibly
#' be the string they were looking for
#'
#' @keywords internal
#' @noRd

.misspelled_string <- function(source, searchterm, default_message = NULL) {
  if (is.null(searchterm) || length(searchterm) < 1) {
    return(default_message)
  }
  # used for many matches
  more_found <- ""
  # init default
  msg <- ""
  # guess the misspelled string
  possible_strings <- unlist(lapply(searchterm, function(s) {
    source[.fuzzy_grep(source, s)] # nolint
  }), use.names = FALSE)
  if (length(possible_strings)) {
    msg <- "Did you mean "
    if (length(possible_strings) > 1) {
      # make sure we don't print dozens of alternatives for larger data frames
      if (length(possible_strings) > 5) {
        more_found <- sprintf(
          " We even found %i more possible matches, not shown here.",
          length(possible_strings) - 5
        )
        possible_strings <- possible_strings[1:5]
      }
      msg <- paste0(msg, "one of ", text_concatenate(possible_strings, enclose = "\"", last = " or "))
    } else {
      msg <- paste0(msg, "\"", possible_strings, "\"")
    }
    msg <- paste0(msg, "?", more_found)
  } else {
    msg <- default_message
  }
  # no double white space
  insight::trim_ws(msg)
}

#' Check that a vector is sorted
#' @noRd

.is_sorted <- Negate(is.unsorted)


#' Replace only custom attributes
#'
#' Using "attributes(out) <- attributes(data)" or similar doesn't work so well
#' for big datasets because it takes some time to attribute the row names.
#'
#' This function gives only custom attributes to the new dataset.
#' @noRd

.replace_attrs <- function(data, custom_attr) {
  for (nm in setdiff(names(custom_attr), names(attributes(data.frame())))) {
    attr(data, which = nm) <- custom_attr[[nm]]
  }
  return(data)
}


.is_date <- function(x) {
  inherits(x, "Date")
}


#' Taken from https://github.com/coolbutuseless/gluestick [licence: MIT]
#' Same functionality as `{glue}`
#'
#' @noRd

.gluestick <- function(fmt, src = parent.frame(), open = "{", close = "}", eval = TRUE) {
  nchar_open <- nchar(open)
  nchar_close <- nchar(close)

  # Sanity checks
  stopifnot(exprs = {
    is.character(fmt)
    length(fmt) == 1L
    is.character(open)
    length(open) == 1L
    nchar_open > 0L
    is.character(close)
    length(close) == 1
    nchar_close > 0
  })

  # Brute force the open/close characters into a regular expression for
  # extracting the expressions from the format string
  open <- gsub("(.)", "\\\\\\1", open) # Escape everything!!
  close <- gsub("(.)", "\\\\\\1", close) # Escape everything!!
  re <- paste0(open, ".*?", close)

  # Extract the delimited expressions
  matches <- gregexpr(re, fmt)
  exprs <- regmatches(fmt, matches)[[1]]

  # Remove the delimiters
  exprs <- substr(exprs, nchar_open + 1L, nchar(exprs) - nchar_close)

  # create a valid sprintf fmt string.
  #  - replace all "{expr}" strings with "%s"
  #  - escape any '%' so sprintf() doesn't try and use them for formatting
  #    but only if the '%' is NOT followed by an 's'
  #
  # gluestick() doesn't deal with any pathological cases
  fmt_sprintf <- gsub(re, "%s", fmt)
  fmt_sprintf <- gsub("%(?!s)", "%%", fmt_sprintf, perl = TRUE)

  # Evaluate
  if (eval) {
    args <- lapply(exprs, function(expr) {
      eval(parse(text = expr), envir = src)
    })
  } else {
    args <- unname(mget(exprs, envir = as.environment(src)))
  }

  # Create the string(s)
  do.call(sprintf, c(list(fmt_sprintf), args))
}
