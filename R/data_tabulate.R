#' @title Create frequency tables of variables
#' @name data_tabulate
#'
#' @description This function creates frequency tables of variables, including
#' the number of levels/values as well as the distribution of raw, valid and
#' cumulative percentages.
#'
#' @param x A (grouped) data frame, a vector or factor.
#' @param drop_levels Logical, if `TRUE`, factor levels that do not occur in
#' the data are included in the table (with frequency of zero), else unused
#' factor levels are dropped from the frequency table.
#' @param name Optional character string, which includes the name that is used
#' for printing.
#' @param collapse Logical, if `TRUE` collapses multiple tables into one larger
#' table for printing. This affects only printing, not the returned object.
#' @param ... not used.
#' @inheritParams find_columns
#'
#' @return A data frame, or a list of data frames, with one frequency table
#' as data frame per variable.
#'
#' @examplesIf requireNamespace("poorman")
#' data(efc)
#'
#' # vector/factor
#' data_tabulate(efc$c172code)
#'
#' # data frame
#' data_tabulate(efc, c("e42dep", "c172code"))
#'
#' # grouped data frame
#' suppressPackageStartupMessages(library(poorman, quietly = TRUE))
#' efc %>%
#'   group_by(c172code) %>%
#'   data_tabulate("e16sex")
#'
#' # collapse tables
#' efc %>%
#'   group_by(c172code) %>%
#'   data_tabulate("e16sex", collapse = TRUE)
#'
#' # for larger N's (> 100000), a big mark is automatically added
#' set.seed(123)
#' x <- sample(1:3, 1e6, TRUE)
#' data_tabulate(x, name = "Large Number")
#'
#' # to remove the big mark, use "print(..., big_mark = "")"
#' print(data_tabulate(x), big_mark = "")
#' @export
data_tabulate <- function(x, ...) {
  UseMethod("data_tabulate")
}


#' @rdname data_tabulate
#' @export
data_tabulate.default <- function(x, drop_levels = FALSE, name = NULL, verbose = TRUE, ...) {
  # save label attribute, before it gets lost...
  var_label <- attr(x, "label", exact = TRUE)

  # save and fix variable name, check for grouping variable
  obj_name <- tryCatch(insight::safe_deparse(substitute(x)), error = function(e) NULL)
  if (identical(obj_name, "x[[i]]")) {
    obj_name <- name
  }
  group_variable <- list(...)$group_variable

  # check whether levels not present in data should be shown or not
  if (is.factor(x) && isTRUE(drop_levels)) {
    x <- droplevels(x)
  }

  # frequency table
  freq_table <- tryCatch(table(addNA(x)), error = function(e) NULL)

  if (is.null(freq_table)) {
    insight::format_warning(paste0("Can't compute frequency tables for objects of class `", class(x)[1], "`."))
    return(NULL)
  }

  # create data frame with freq table and cumulative percentages etc.
  out <- data_rename(data.frame(freq_table, stringsAsFactors = FALSE),
    replacement = c("Value", "N")
  )

  out$`Raw %` <- 100 * out$N / sum(out$N)
  out$`Valid %` <- c(100 * out$N[-nrow(out)] / sum(out$N[-nrow(out)]), NA)
  out$`Cumulative %` <- cumsum(out$`Valid %`)

  # add information about variable/group names
  if (!is.null(obj_name)) {
    if (is.null(group_variable)) {
      var_info <- data.frame(Variable = obj_name, stringsAsFactors = FALSE)
    } else {
      var_info <- data.frame(
        Variable = obj_name,
        Group = toString(lapply(colnames(group_variable), function(i) {
          sprintf("%s (%s)", i, group_variable[[i]])
        })),
        stringsAsFactors = FALSE
      )
    }
    out <- cbind(var_info, out)
  }

  # save information
  attr(out, "type") <- .variable_type(x)
  attr(out, "varname") <- name
  attr(out, "label") <- var_label
  attr(out, "object") <- obj_name
  attr(out, "group_variable") <- group_variable
  attr(out, "duplicate_varnames") <- duplicated(out$Variable)

  attr(out, "total_n") <- sum(out$N, na.rm = TRUE)
  attr(out, "valid_n") <- sum(out$N[-length(out$N)], na.rm = TRUE)

  class(out) <- c("dw_data_tabulate", "data.frame")

  out
}


#' @rdname data_tabulate
#' @export
data_tabulate.data.frame <- function(x,
                                     select = NULL,
                                     exclude = NULL,
                                     ignore_case = FALSE,
                                     regex = FALSE,
                                     collapse = FALSE,
                                     drop_levels = FALSE,
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
  out <- lapply(select, function(i) {
    data_tabulate(x[[i]], drop_levels = drop_levels, name = i, verbose = verbose, ...)
  })

  class(out) <- c("dw_data_tabulates", "list")
  attr(out, "collapse") <- isTRUE(collapse)

  out
}


#' @export
data_tabulate.grouped_df <- function(x,
                                     select = NULL,
                                     exclude = NULL,
                                     ignore_case = FALSE,
                                     regex = FALSE,
                                     verbose = TRUE,
                                     collapse = FALSE,
                                     drop_levels = FALSE,
                                     ...) {
  # works only for dplyr >= 0.8.0
  grps <- attr(x, "groups", exact = TRUE)
  group_variables <- data_remove(grps, ".rows")
  grps <- grps[[".rows"]]

  # evaluate arguments
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  x <- as.data.frame(x)
  out <- list()
  for (i in seq_along(grps)) {
    rows <- grps[[i]]
    # save information about grouping factors
    if (!is.null(group_variables)) {
      group_variable <- group_variables[i, , drop = FALSE]
    } else {
      group_variable <- NULL
    }
    out <- c(out, data_tabulate(
      data_filter(x, rows),
      select = select,
      exclude = exclude,
      ignore_case = ignore_case,
      verbose = verbose,
      drop_levels = drop_levels,
      group_variable = group_variable,
      ...
    ))
  }
  class(out) <- c("dw_data_tabulates", "list")
  attr(out, "collapse") <- isTRUE(collapse)

  out
}




# methods --------------------

#' @importFrom insight print_html
#' @export
insight::print_html


#' @importFrom insight print_md
#' @export
insight::print_md


#' @export
format.dw_data_tabulate <- function(x, format = "text", big_mark = NULL, ...) {
  # convert to character manually, else, for large numbers,
  # format_table() returns scientific notation
  x <- as.data.frame(x)
  x$N <- as.character(x$N)

  # format data frame
  ftab <- insight::format_table(x, ...)
  ftab[] <- lapply(ftab, function(i) {
    i[i == ""] <- ifelse(identical(format, "text"), "<NA>", "(NA)")
    i
  })
  ftab$N <- gsub("\\.00$", "", ftab$N)

  # insert big marks?
  ftab$N <- .add_commas_in_numbers(ftab$N, big_mark)

  ftab
}

.add_commas_in_numbers <- function(x, big_mark = NULL) {
  if (is.null(big_mark) && any(nchar(x) > 5)) {
    big_mark <- ","
  }
  if (!is.null(big_mark)) {
    x <- prettyNum(x, big.mark = big_mark)
  }

  x
}



#' @export
print.dw_data_tabulate <- function(x, big_mark = NULL, ...) {
  a <- attributes(x)

  # "table" header with variable label/name, and type
  cat(.table_header(x, "text"))

  # grouped data? if yes, add information on grouping factor
  if (!is.null(a$group_variable)) {
    group_title <- paste0("Grouped by ", toString(lapply(colnames(a$group_variable), function(i) {
      sprintf("%s (%s)", i, a$group_variable[[i]])
    })))
    cat(insight::print_color(group_title, "blue"))
    cat("\n")
  }

  a$total_n <- .add_commas_in_numbers(a$total_n, big_mark)
  a$valid_n <- .add_commas_in_numbers(a$valid_n, big_mark)

  # summary of total and valid N (we may add mean/sd as well?)
  summary_line <- sprintf("# total N=%s valid N=%s\n\n", a$total_n, a$valid_n)
  cat(insight::print_color(summary_line, "blue"))

  # remove information that goes into the header/footer
  x$Variable <- NULL
  x$Group <- NULL

  # print table
  cat(insight::export_table(
    format(x, big_mark = big_mark, ...),
    cross = "+",
    missing = "<NA>"
  ))
  invisible(x)
}


#' @export
print_html.dw_data_tabulate <- function(x, big_mark = NULL, ...) {
  a <- attributes(x)

  # "table" header with variable label/name, and type
  caption <- .table_header(x, "html")

  # summary of total and valid N (we may add mean/sd as well?)
  footer <- sprintf("total N=%i valid N=%i\n\n", a$total_n, a$valid_n)

  # remove information that goes into the header/footer
  x$Variable <- NULL
  x$Group <- NULL

  # print table
  insight::export_table(
    format(x, format = "html", big_mark = big_mark, ...),
    title = caption,
    footer = footer,
    missing = "(NA)",
    format = "html"
  )
}


#' @export
print_md.dw_data_tabulate <- function(x, big_mark = NULL, ...) {
  a <- attributes(x)

  # "table" header with variable label/name, and type
  caption <- .table_header(x, "markdown")

  # summary of total and valid N (we may add mean/sd as well?)
  footer <- sprintf("total N=%i valid N=%i\n\n", a$total_n, a$valid_n)

  # remove information that goes into the header/footer
  x$Variable <- NULL
  x$Group <- NULL

  # print table
  insight::export_table(
    format(x, format = "markdown", big_mark = big_mark, ...),
    title = caption,
    footer = footer,
    missing = "(NA)",
    format = "markdown"
  )
}


#' @export
print.dw_data_tabulates <- function(x, big_mark = NULL, ...) {
  a <- attributes(x)
  if (!isTRUE(a$collapse) || length(x) == 1) {
    for (i in seq_along(x)) {
      print(x[[i]], big_mark = big_mark, ...)
      if (i < length(x)) cat("\n")
    }
  } else {
    x <- lapply(x, function(i) {
      attr <- attributes(i)
      i <- format(i, format = "text", big_mark = big_mark, ...)
      i$Variable[attr$duplicate_varnames] <- ""
      if (!is.null(i$Group)) i$Group[attr$duplicate_varnames] <- ""
      i[nrow(i) + 1, ] <- ""
      i
    })

    out <- do.call(rbind, x)
    cat(insight::print_color("# Frequency Table\n\n", "blue"))

    # print table
    cat(insight::export_table(
      out,
      missing = "<NA>",
      cross = "+",
      empty_line = "-"
    ))
  }
}


#' @export
print_html.dw_data_tabulates <- function(x, big_mark = NULL, ...) {
  if (length(x) == 1) {
    print_html(x[[1]], big_mark = big_mark, ...)
  } else {
    x <- lapply(x, function(i) {
      attr <- attributes(i)
      i <- format(i, format = "html", big_mark = big_mark, ...)
      i$Variable[attr$duplicate_varnames] <- ""
      i
    })

    out <- do.call(rbind, x)

    # print table
    insight::export_table(
      out,
      missing = "<NA>",
      caption = "Frequency Table",
      format = "html",
      group_by = "Group"
    )
  }
}


#' @export
print_md.dw_data_tabulates <- function(x, big_mark = NULL, ...) {
  if (length(x) == 1) {
    print_md(x[[1]], big_mark = big_mark, ...)
  } else {
    x <- lapply(x, function(i) {
      attr <- attributes(i)
      i <- format(i, format = "markdown", big_mark = big_mark, ...)
      i$Variable[attr$duplicate_varnames] <- ""
      if (!is.null(i$Group)) i$Group[attr$duplicate_varnames] <- ""
      i[nrow(i) + 1, ] <- ""
      i
    })

    out <- do.call(rbind, x)

    # print table
    insight::export_table(
      out,
      missing = "(NA)",
      empty_line = "-",
      format = "markdown",
      title = "Frequency Table"
    )
  }
}




# tools --------------------

.table_header <- function(x, format = "text") {
  a <- attributes(x)

  # assemble name, based on what information is available
  name <- NULL
  # fix object name
  if (identical(a$object, "x[[i]]")) {
    a$object <- NULL
  }
  if (!is.null(a$label)) {
    name <- a$label
    if (!is.null(a$varname)) {
      name <- paste0(name, " (", a$varname, ")")
    } else if (!is.null(a$object)) {
      name <- paste0(name, " (", a$object, ")")
    }
  } else if (!is.null(a$varname)) {
    name <- a$varname
    if (!is.null(a$object)) {
      name <- paste0(name, " (", a$object, ")")
    }
  }

  if (is.null(name) && !is.null(a$object)) {
    name <- a$object
  }

  # "table" header with variable label/name, and type
  if (identical(format, "text")) {
    out <- paste(
      insight::color_text(name, "red"),
      insight::color_text(sprintf("<%s>\n", a$type), "blue")
    )
  } else {
    out <- paste0(name, " (", a$type, ")")
  }

  out
}


.variable_type <- function(x) {
  if (is.ordered(x)) {
    vt <- "ord"
  } else if (is.factor(x)) {
    vt <- "fct"
  } else if (class(x)[1] == "Date") {
    vt <- "date"
  } else {
    vt <- switch(typeof(x),
      logical = "lgl",
      integer = "int",
      double = "dbl",
      character = "chr",
      complex = "cpl",
      closure = "fn",
      environment = "env",
      typeof(x)
    )
  }

  switch(vt,
    "ord" = "ordinal",
    "fct" = "categorical",
    "dbl" = "numeric",
    "int" = "integer",
    "chr" = "character",
    "lbl" = "labelled",
    "cpl" = "complex",
    "lgl" = "logical",
    vt
  )
}
