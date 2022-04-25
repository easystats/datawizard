#' @title Create frequency tables of variables
#' @name data_table
#'
#' @description This function creates frequency tables of data frame, including
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
#' @examples
#' data(efc)
#'
#' # vector/factor
#' data_table(efc$c172code)
#'
#' # data frame
#' data_table(efc, c("e42dep", "c172code"))
#'
#' # grouped data frame
#' if (require("poorman")) {
#'   efc %>%
#'     group_by(c172code) %>%
#'     data_table("e16sex")
#' }
#' @export
data_table <- function(x, ...) {
  UseMethod("data_table")
}


#' @rdname data_table
#' @export
data_table.default <- function(x, drop_levels = FALSE, name = NULL, verbose = TRUE, ...) {
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
    warning(paste0("Can't compute frequency tables for objects of class '", class(x)[1], "'."), call. = FALSE)
    return(NULL)
  }

  # create data frame with freq table and cumulative percentages etc.
  out <- data_rename(data.frame(freq_table, stringsAsFactors = FALSE),
                     replacement = c("Value", "N"))

  out$`Raw %` <- 100 * out$N / sum(out$N)
  out$`Valid %` <- c(100 * out$N[-nrow(out)] / sum(out$N[-nrow(out)]), NA)
  out$`Cumulative %` <- cumsum(out$`Valid %`)

  # add information about variable/group names
  if (!is.null(obj_name)) {
    if (is.null(group_variable)) {
      var_info <- data.frame(Variable = obj_name, stringsAsFactors = FALSE)
    } else {
      var_info <- data.frame(Variable = obj_name,
                             Group = paste0(lapply(colnames(group_variable), function(i) {
                               sprintf("%s (%s)", i, group_variable[[i]])
                             }), collapse = ", "),
                             stringsAsFactors = FALSE)
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

  class(out) <- c("dw_data_table", "data.frame")

  out
}


#' @rdname data_table
#' @export
data_table.data.frame <- function(x,
                                  select = NULL,
                                  exclude = NULL,
                                  ignore_case = FALSE,
                                  verbose = TRUE,
                                  collapse = FALSE,
                                  drop_levels = FALSE,
                                  ...) {
  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)
  out <- lapply(select, function(i) {
    data_table(x[[i]], drop_levels = drop_levels, name = i, verbose = verbose, ...)
  })

  class(out) <- c("dw_data_tables", "list")
  attr(out, "collapse") <- isTRUE(collapse)

  out
}


#' @export
data_table.grouped_df <- function(x,
                                  select = NULL,
                                  exclude = NULL,
                                  ignore_case = FALSE,
                                  verbose = TRUE,
                                  collapse = FALSE,
                                  drop_levels = FALSE,
                                  ...) {
  # dplyr < 0.8.0 returns attribute "indices"
  grps <- attr(x, "groups", exact = TRUE)
  group_variables <- NULL

  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)

  # dplyr < 0.8.0?
  if (is.null(grps)) {
    grps <- attr(x, "indices", exact = TRUE)
    grps <- lapply(grps, function(x) x + 1)
  } else {
    group_variables <- data_remove(grps, ".rows")
    grps <- grps[[".rows"]]
  }

  x <- as.data.frame(x)
  out <- list()
  for (i in 1:length(grps)) {
    rows <- grps[[i]]
    # save information about grouping factors
    if (!is.null(group_variables)) {
      group_variable <- group_variables[i, , drop = FALSE]
    } else {
      group_variable <- NULL
    }
    out <- c(out, data_table(
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
  class(out) <- c("dw_data_tables", "list")
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
format.dw_data_table <- function(x, format = "text", ...) {
  # format data frame
  ftab <- insight::format_table(as.data.frame(x))
  ftab[] <- lapply(ftab, function(i) {
    i[i == ""] <- ifelse(identical(format, "text"), "<NA>", "(NA)")
    i
  })
  ftab$N <- gsub("\\.00$", "", ftab$N)
  ftab
}


#' @export
print.dw_data_table <- function(x, ...) {
  a <- attributes(x)

  # "table" header with variable label/name, and type
  cat(.table_header(x, "text"))

  # grouped data? if yes, add information on grouping factor
  if (!is.null(a$group_variable)) {
    group_title <- paste0("Grouped by ", paste0(lapply(colnames(a$group_variable), function(i) {
      sprintf("%s (%s)", i, a$group_variable[[i]])
    }), collapse = ", "))
    cat(insight::print_color(group_title, "blue"))
    cat("\n")
  }

  # summary of total and valid N (we may add mean/sd as well?)
  summary_line <- sprintf("# total N=%g valid N=%g\n\n", a$total_n, a$valid_n)
  cat(insight::print_color(summary_line, "blue"))

  # remove information that goes into the header/footer
  x$Variable <- NULL
  x$Group <- NULL

  # print table
  cat(insight::export_table(
    format(x),
    cross = "+",
    missing = "<NA>"
  ))
  invisible(x)
}


#' @export
print_html.dw_data_table <- function(x, ...) {
  a <- attributes(x)

  # "table" header with variable label/name, and type
  caption <- .table_header(x, "html")

  # summary of total and valid N (we may add mean/sd as well?)
  footer <- sprintf("total N=%g valid N=%g\n\n", a$total_n, a$valid_n)

  # remove information that goes into the header/footer
  x$Variable <- NULL
  x$Group <- NULL

  # print table
  insight::export_table(
    format(x, format = "html"),
    title = caption,
    footer = footer,
    missing = "(NA)",
    format = "html"
  )
}


#' @export
print_md.dw_data_table <- function(x, ...) {
  a <- attributes(x)

  # "table" header with variable label/name, and type
  caption <- .table_header(x, "markdown")

  # summary of total and valid N (we may add mean/sd as well?)
  footer <- sprintf("total N=%g valid N=%g\n\n", a$total_n, a$valid_n)

  # remove information that goes into the header/footer
  x$Variable <- NULL
  x$Group <- NULL

  # print table
  insight::export_table(
    format(x, format = "markdown"),
    title = caption,
    footer = footer,
    missing = "(NA)",
    format = "markdown"
  )
}


#' @export
print.dw_data_tables <- function(x, ...) {
  a <- attributes(x)
  if (!isTRUE(a$collapse) || length(x) == 1) {
    for (i in 1:length(x)) {
      print(x[[i]])
      if (i < length(x)) cat("\n")
    }
  } else {
    x <- lapply(x, function(i) {
      attr <- attributes(i)
      i <- format(i, format = "text")
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
print_html.dw_data_tables <- function(x, ...) {
  if (length(x) == 1) {
    print_html(x[[1]])
  } else {
    x <- lapply(x, function(i) {
      attr <- attributes(i)
      i <- format(i, format = "html")
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
print_md.dw_data_tables <- function(x, ...) {
  if (length(x) == 1) {
    print_md(x[[1]])
  } else {
    x <- lapply(x, function(i) {
      attr <- attributes(i)
      i <- format(i, format = "markdown")
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
    out <- paste(insight::color_text(name, "red"),
                 insight::color_text(sprintf("<%s>\n", a$type), "blue"))
  } else {
    out <- paste0(name, " (", a$type, ")")
  }

  out
}


.variable_type <- function(x) {
  if (is.ordered(x))
    vt <- "ord"
  else if (is.factor(x))
    vt <- "fct"
  else if (class(x)[1] == "Date")
    vt <- "date"
  else {
    vt <- switch(typeof(x), logical = "lgl", integer = "int",
                 double = "dbl", character = "chr", complex = "cpl",
                 closure = "fn", environment = "env", typeof(x))
  }

  switch(vt, "ord" = "ordinal", "fct" = "categorical", "dbl" = "numeric",
         "int" = "integer", "chr" = "character", "lbl" = "labelled",
         "cpl" = "complex", vt)
}
