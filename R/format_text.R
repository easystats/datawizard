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
format_text <- function(text, sep = ", ", last = " and ", width = NULL, enclose = NULL, ...) {
  text_wrap(text_concatenate(text, sep = sep, last = last, enclose = enclose), width = width)
}


#' @rdname format_text
#' @export
text_fullstop <- function(text) {
  text[!text_lastchar(text) %in% c(".", ":", ",", ";", "!", "?")] <- paste0(text[text_lastchar(text) != "."], ".")
  text
}


#' @rdname format_text
#' @export
text_lastchar <- function(text, n = 1) {
  sapply(text, function(xx) {
    substr(xx, (nchar(xx) - n + 1), nchar(xx))
  })
}


#' @rdname format_text
#' @export
text_concatenate <- function(text, sep = ", ", last = " and ", enclose = NULL) {
  text <- text[text != ""]
  if (length(text) && !is.null(enclose) && length(enclose) == 1 && nchar(enclose) > 0) {
    text <- paste0(enclose, text, enclose)
  }
  if (length(text) == 1) {
    s <- text
  } else {
    s <- paste0(utils::head(text, -1), collapse = sep)
    s <- paste0(c(s, utils::tail(text, 1)), collapse = last)
  }
  s
}


#' @rdname format_text
#' @export
text_paste <- function(text, text2 = NULL, sep = ", ", enclose = NULL, ...) {
  if (!is.null(text2)) {
    if (!is.null(enclose) && length(enclose) == 1 && nchar(enclose) > 0) {
      text <- sapply(text, function(i) {
        if (i != "") {
          i <- paste0(enclose, i, enclose)
        }
        i
      })
      text2 <- sapply(text2, function(i) {
        if (i != "") {
          i <- paste0(enclose, i, enclose)
        }
        i
      })
    }
    paste0(text, ifelse(text == "" | text2 == "", "", sep), text2)
  }
}



#' @rdname format_text
#' @export
text_remove <- function(text, pattern = "", ...) {
  gsub(pattern, "", text, ...)
}


#' @rdname format_text
#' @export
text_wrap <- function(text, width = NULL, ...) {
  width <- width %||% getOption("width")

  text <- strsplit(text, "\n", perl = TRUE)
  text <- unlist(text)

  wrapped <- ""

  for (s in text) {
    if (nchar(s) > width) {
      leading_spaces <- nchar(s) - nchar(trimws(s))
      s <- strwrap(s, width = width)
      s <- paste0(s, collapse = "\n")
      s <- paste0(paste0(rep(" ", leading_spaces), collapse = ""), s)
    }
    wrapped <- paste0(wrapped, s, "\n")
  }

  wrapped
}
