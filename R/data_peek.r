#' @title Peek at values and type of variables in a data frame
#' @name data_peek
#'
#' @description This function creates a table a data frame, showing all
#' column names, variable types and the first values (as many as fit into
#' the screen).
#'
#' @param x A data frame.
#' @param width Maximum width of line length to display. If `NULL`, width will
#' be determined using `options()$width`.
#' @param n Number of variables to display. If `NULL`, all variables will be
#' shown.
#' @param ... not used.
#' @inheritParams find_columns
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @return fds
#'
#' @examples
#' data(efc)
#' data_peek(efc)
#' @export
data_peek <- function(x, ...) {
  UseMethod("data_peek")
}


#' @rdname data_peek
#' @export
data_peek.data.frame <- function(x,
                                 select = NULL,
                                 exclude = NULL,
                                 ignore_case = FALSE,
                                 regex = FALSE,
                                 width = NULL,
                                 n = NULL,
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
  out <- do.call(rbind, lapply(select, function(i) {
    .data_peek(x, i, width, verbose = verbose, ...)
  }))

  # shorten?
  if (!is.null(n)) {
    out <- out[1:n, ]
  }

  class(out) <- c("dw_data_peek", class(out))
  attr(out, "n_rows") <- n
  attr(out, "max_width") <- ifelse(is.null(width), 0.9 * options()$width, width)

  out
}


# methods -----------------

#' @export
print.dw_data_peek <- function(x, n = NULL, ...) {
  x <- format(x, n = n, ...)
  cat(insight::export_table(x, align = "lll", ...))
}

#' @export
print_md.dw_data_peek <- function(x, n = NULL, ...) {
  x <- format(x, n = n, ...)
  insight::export_table(x, align = "lll", format = "markdown", ...)
}

#' @export
print_html.dw_data_peek <- function(x, n = NULL, ...) {
  x <- format(x, n = n, ...)
  insight::export_table(x, align = "lll", format = "html", ...)
}

#' @export
format.dw_data_peek <- function(x, n = NULL, ...) {
  width_col1 <- max(nchar(x$Variable))
  width_col2 <- max(nchar(x$Type))
  max_width <- attributes(x)$max_width
  if (is.null(max_width)) {
    max_width <- 0.9 * options()$width
  }
  width_col3 <- max_width - (width_col1 + width_col2 + 10) # 10 = separator chars in table

  # shorten value-string
  x$Values <- substr(x$Values, 0, width_col3)
  # make sure we have a clear truncation, at last "comma"
  x$Values <- gsub("(.+)(,.+)$", "\\1", x$Values)
  # add "..."
  x$Values <- paste0(x$Values, ", ...")

  # row-limit?
  if (is.null(n)) {
    n <- attributes(x)$n_rows
  }
  if (!is.null(n)) {
    x <- x[1:n, ]
  }

  x
}


# helper -----------------

.data_peek <- function(x, variable, width = NULL, verbose = TRUE, ...) {
  v_name <- variable
  v_type <- .variable_type(x[[variable]])

  max_width <- ifelse(is.null(width), 0.9 * options()$width, width)
  v_values <- paste0(x[[variable]][1:max_width], collapse = ", ")

  data.frame(Variable = v_name, Type = v_type, Values = v_values, stringsAsFactors = FALSE)
}
