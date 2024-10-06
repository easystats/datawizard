#' Convenient text formatting functionalities
#'
#' Convenience functions to manipulate and format text.
#'
#' @param text,text2 A character string.
#' @param width Positive integer giving the target column width for wrapping
#' lines in the output. Can be "auto", in which case it will select 90\% of the
#' default width.
#' @inheritParams data_rename
#' @param sep Separator.
#' @param last Last separator.
#' @param n The number of characters to find.
#' @param enclose Character that will be used to wrap elements of `text`, so
#'   these can be, e.g., enclosed with quotes or backticks. If `NULL` (default),
#'   text elements will not be enclosed.
#' @param ... Other arguments to be passed to or from other functions.
#'
#' @return A character string.
#'
#' @examples
#' # Add full stop if missing
#' text_fullstop(c("something", "something else."))
#'
#' # Find last characters
#' text_lastchar(c("ABC", "DEF"), n = 2)
#'
#' # Smart concatenation
#' text_concatenate(c("First", "Second", "Last"))
#' text_concatenate(c("First", "Second", "Last"), last = " or ", enclose = "`")
#'
#' # Remove parts of string
#' text_remove(c("one!", "two", "three!"), "!")
#'
#' # Wrap text
#' long_text <- paste(rep("abc ", 100), collapse = "")
#' cat(text_wrap(long_text, width = 50))
#'
#' # Paste with optional separator
#' text_paste(c("A", "", "B"), c("42", "42", "42"))
#' @export
text_format <- function(text, sep = ", ", last = " and ", width = NULL, enclose = NULL, ...) {
  text_wrap(text_concatenate(text, sep = sep, last = last, enclose = enclose), width = width)
}

#' @rdname text_format
#' @export
text_fullstop <- function(text) {
  text[!text_lastchar(text) %in% c(".", ":", ",", ";", "!", "?")] <- paste0(text[text_lastchar(text) != "."], ".")
  text
}


#' @rdname text_format
#' @export
text_lastchar <- function(text, n = 1) {
  vapply(text, function(xx) {
    substr(xx, (nchar(xx) - n + 1), nchar(xx))
  }, FUN.VALUE = character(1L))
}


#' @rdname text_format
#' @export
text_concatenate <- function(text, sep = ", ", last = " and ", enclose = NULL) {
  if (length(text) == 1 && !nzchar(text, keepNA = TRUE)) {
    return(text)
  }
  text <- text[text != ""] # nolint
  if (length(text) && !is.null(enclose) && length(enclose) == 1 && nzchar(enclose, keepNA = TRUE)) {
    text <- paste0(enclose, text, enclose)
  }
  if (length(text) == 1) {
    s <- text
  } else {
    s <- paste(text[1:(length(text) - 1)], collapse = sep)
    s <- paste(c(s, text[length(text)]), collapse = last)
  }
  s
}


#' @rdname text_format
#' @export
text_paste <- function(text, text2 = NULL, sep = ", ", enclose = NULL, ...) {
  if (!is.null(text2)) {
    if (!is.null(enclose) && length(enclose) == 1 && nzchar(enclose, keepNA = TRUE)) {
      text <- vapply(text, function(i) {
        if (i != "") {
          i <- paste0(enclose, i, enclose)
        }
        i
      }, character(1L))
      text2 <- vapply(text2, function(i) {
        if (i != "") {
          i <- paste0(enclose, i, enclose)
        }
        i
      }, character(1L))
    }
    paste0(text, ifelse(text == "" | text2 == "", "", sep), text2) # nolint
  }
}



#' @rdname text_format
#' @export
text_remove <- function(text, pattern = "", ...) {
  gsub(pattern, "", text, ...)
}


#' @rdname text_format
#' @export
text_wrap <- function(text, width = NULL, ...) {
  width <- width %||% getOption("width")

  text <- strsplit(text, "\n", fixed = TRUE)
  text <- unlist(text, use.names = FALSE)

  wrapped <- ""

  for (s in text) {
    if (nchar(s) > width) {
      leading_spaces <- nchar(s) - nchar(insight::trim_ws(s))
      s <- strwrap(s, width = width)
      s <- paste(s, collapse = "\n")
      s <- paste0(strrep(" ", leading_spaces), s)
    }
    wrapped <- paste0(wrapped, s, "\n")
  }

  wrapped
}
