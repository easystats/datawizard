#' @title Create frequency tables of variables
#' @name data_table
#'
#' @description This function creates frequency tables of data frame, including
#' the number of levels/values as well as the distribution of raw, valid and
#' cumulative percentages.
#'
#' @param x A (grouped) data frame, a vector or factor.
#' @param name Optional character string, which includes the name that is used
#' for printing.
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
data_table.default <- function(x, name = NULL, verbose = TRUE, ...) {
  freq_table <- tryCatch(table(addNA(x)), error = function(e) NULL)

  if (is.null(freq_table)) {
    warning(paste0("Can't compute frequency tables for objects of class '", class(x)[1], "'."), call. = FALSE)
    return(NULL)
  }

  out <- data_rename(data.frame(freq_table, stringsAsFactors = FALSE),
                     replacement = c("Value", "N"))

  out$`Raw %` <- 100 * out$N / sum(out$N)
  out$`Valid %` <- c(100 * out$N[-nrow(out)] / sum(out$N[-nrow(out)]), NA)
  out$`Cumulative %` <- cumsum(out$`Valid %`)

  # save information
  attr(out, "type") <- .variable_type(x)
  attr(out, "varname") <- name
  attr(out, "label") <- attr(x, "label", exact = TRUE)
  attr(out, "object") <- tryCatch(insight::safe_deparse(substitute(x)), error = function(e) NULL)
  attr(out, "group_variable") <- list(...)$group_variable

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
                                  ...) {
  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)
  out <- lapply(select, function(i) {
    data_table(x[[i]], name = i, verbose = verbose, ...)
  })

  class(out) <- c("dw_data_tables", "list")
  out
}


#' @export
data_table.grouped_df <- function(x,
                                  select = NULL,
                                  exclude = NULL,
                                  ignore_case = FALSE,
                                  verbose = TRUE,
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
      x[rows, , drop = FALSE],
      select = select,
      exclude = exclude,
      ignore_case = ignore_case,
      verbose = verbose,
      group_variable = group_variable,
      ...
    ))
  }
  class(out) <- c("dw_data_tables", "list")
  out
}




# methods --------------------

#' @export
print.dw_data_table <- function(x, ...) {
  a <- attributes(x)

  # format data frame
  ftab <- insight::format_table(as.data.frame(x))
  ftab[] <- lapply(ftab, function(i) {
    i[i == ""] <- "<NA>"
    i
  })
  ftab$N <- gsub("\\.00$", "", ftab$N)

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
  cat(insight::print_color(name, "red"))
  cat(insight::print_color(sprintf(" <%s>\n", a$type), "blue"))

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

  # print table
  cat(insight::export_table(ftab, cross = "+", missing = "<NA>"))
  invisible(x)
}


#' @export
print.dw_data_tables <- function(x, ...) {
  for (i in 1:length(x)) {
    print(x[[i]])
    if (i < length(x)) cat("\n")
  }
}




# tools --------------------

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
