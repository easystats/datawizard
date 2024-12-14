#' @title Create frequency and crosstables of variables
#' @name data_tabulate
#'
#' @description This function creates frequency or crosstables of variables,
#' including the number of levels/values as well as the distribution of raw,
#' valid and cumulative percentages. For crosstables, row, column  and cell
#' percentages can be calculated.
#'
#' @param x A (grouped) data frame, a vector or factor.
#' @param by Optional vector or factor. If supplied, a crosstable is created.
#' If `x` is a data frame, `by` can also be a character string indicating the
#' name of a variable in `x`.
#' @param drop_levels Logical, if `FALSE`, factor levels that do not occur in
#' the data are included in the table (with frequency of zero), else unused
#' factor levels are dropped from the frequency table.
#' @param name Optional character string, which includes the name that is used
#' for printing.
#' @param remove_na Logical, if `FALSE`, missing values are included in the
#' frequency or crosstable, else missing values are omitted.
#' @param collapse Logical, if `TRUE` collapses multiple tables into one larger
#' table for printing. This affects only printing, not the returned object.
#' @param weights Optional numeric vector of weights. Must be of the same length
#' as `x`. If `weights` is supplied, weighted frequencies are calculated.
#' @param proportions Optional character string, indicating the type of
#' percentages to be calculated. Only applies to crosstables, i.e. when `by` is
#' not `NULL`. Can be `"row"` (row percentages), `"column"` (column percentages)
#' or `"full"` (to calculate relative frequencies for the full table).
#' @param ... not used.
#' @inheritParams extract_column_names
#'
#' @details
#' There is an `as.data.frame()` method, to return the frequency tables as a
#' data frame. The structure of the returned object is a nested data frame,
#' where the first column contains name of the variable for which frequencies
#' were calculated, and the second column is a list column that contains the
#' frequency tables as data frame. See 'Examples'.
#'
#' @section Crosstables:
#' If `by` is supplied, a crosstable is created. The crosstable includes `<NA>`
#' (missing) values by default. The first column indicates values of `x`, the
#' first row indicates values of `by` (including missing values). The last row
#' and column contain the total frequencies for each row and column, respectively.
#' Setting `remove_na = FALSE` will omit missing values from the crosstable.
#' Setting `proportions` to `"row"` or `"column"` will add row or column
#' percentages. Setting `proportions` to `"full"` will add relative frequencies
#' for the full table.
#'
#' @note
#' There are `print_html()` and `print_md()` methods available for printing
#' frequency or crosstables in HTML and markdown format, e.g.
#' `print_html(data_tabulate(x))`. The `print()` method for text outputs passes
#' arguments in `...` to [`insight::export_table()`].
#'
#' @return A data frame, or a list of data frames, with one frequency table
#' as data frame per variable.
#'
#' @examplesIf requireNamespace("poorman")
#' # frequency tables -------
#' # ------------------------
#' data(efc)
#'
#' # vector/factor
#' data_tabulate(efc$c172code)
#'
#' # drop missing values
#' data_tabulate(efc$c172code, remove_na = TRUE)
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
#'
#' # weighted frequencies
#' set.seed(123)
#' efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
#' data_tabulate(efc$e42dep, weights = efc$weights)
#'
#' # crosstables ------
#' # ------------------
#'
#' # add some missing values
#' set.seed(123)
#' efc$e16sex[sample.int(nrow(efc), 5)] <- NA
#'
#' data_tabulate(efc, "c172code", by = "e16sex")
#'
#' # add row and column percentages
#' data_tabulate(efc, "c172code", by = "e16sex", proportions = "row")
#' data_tabulate(efc, "c172code", by = "e16sex", proportions = "column")
#'
#' # omit missing values
#' data_tabulate(
#'   efc$c172code,
#'   by = efc$e16sex,
#'   proportions = "column",
#'   remove_na = TRUE
#' )
#'
#' # round percentages
#' out <- data_tabulate(efc, "c172code", by = "e16sex", proportions = "column")
#' print(out, digits = 0)
#'
#' # coerce to data frames
#' result <- data_tabulate(efc, "c172code", by = "e16sex")
#' as.data.frame(result)
#' as.data.frame(result)$table
#' as.data.frame(result, add_total = TRUE)$table
#' @export
data_tabulate <- function(x, ...) {
  UseMethod("data_tabulate")
}


#' @rdname data_tabulate
#' @export
data_tabulate.default <- function(x,
                                  by = NULL,
                                  drop_levels = FALSE,
                                  weights = NULL,
                                  remove_na = FALSE,
                                  proportions = NULL,
                                  name = NULL,
                                  verbose = TRUE,
                                  ...) {
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

  # validate "weights"
  weights <- .validate_table_weights(weights, x, weights_expression = insight::safe_deparse(substitute(weights)))

  # we go into another function for crosstables here...
  if (!is.null(by)) {
    by <- .validate_by(by, x)
    return(.crosstable(
      x,
      by = by,
      weights = weights,
      remove_na = remove_na,
      proportions = proportions,
      obj_name = obj_name,
      group_variable = group_variable
    ))
  }

  # frequency table
  if (is.null(weights)) {
    if (remove_na) {
      # we have a `.default` and a `.data.frame` method for `data_tabulate()`.
      # since this is the default, `x` can be an object which cannot be used
      # with `table()`, that's why we add `tryCatch()` here. Below we give an
      # informative error message for non-supported objects.
      freq_table <- tryCatch(table(x), error = function(e) NULL)
    } else {
      freq_table <- tryCatch(table(addNA(x)), error = function(e) NULL)
    }
  } else if (remove_na) {
    # weighted frequency table, excluding NA
    freq_table <- tryCatch(
      stats::xtabs(
        weights ~ x,
        data = data.frame(weights = weights, x = x),
        na.action = stats::na.omit,
        addNA = FALSE
      ),
      error = function(e) NULL
    )
  } else {
    # weighted frequency table, including NA
    freq_table <- tryCatch(
      stats::xtabs(
        weights ~ x,
        data = data.frame(weights = weights, x = addNA(x)),
        na.action = stats::na.pass,
        addNA = TRUE
      ),
      error = function(e) NULL
    )
  }

  if (is.null(freq_table)) {
    insight::format_warning(paste0("Can't compute frequency tables for objects of class `", class(x)[1], "`."))
    return(NULL)
  }

  # create data frame with freq table and cumulative percentages etc.
  out <- data_rename(data.frame(freq_table, stringsAsFactors = FALSE),
    replacement = c("Value", "N")
  )

  # we want to round N for weighted frequencies
  if (!is.null(weights)) {
    out$N <- round(out$N)
  }

  out$`Raw %` <- 100 * out$N / sum(out$N)
  # if we have missing values, we add a row with NA
  if (remove_na) {
    out$`Valid %` <- 100 * out$N / sum(out$N)
    valid_n <- sum(out$N, na.rm = TRUE)
  } else {
    out$`Valid %` <- c(100 * out$N[-nrow(out)] / sum(out$N[-nrow(out)]), NA)
    valid_n <- sum(out$N[-length(out$N)], na.rm = TRUE)
  }
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
  attr(out, "weights") <- weights

  attr(out, "total_n") <- sum(out$N, na.rm = TRUE)
  attr(out, "valid_n") <- valid_n

  class(out) <- c("datawizard_table", "data.frame")

  out
}


#' @rdname data_tabulate
#' @export
data_tabulate.data.frame <- function(x,
                                     select = NULL,
                                     exclude = NULL,
                                     ignore_case = FALSE,
                                     regex = FALSE,
                                     by = NULL,
                                     drop_levels = FALSE,
                                     weights = NULL,
                                     remove_na = FALSE,
                                     proportions = NULL,
                                     collapse = FALSE,
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

  # validate "by"
  by <- .validate_by(by, x)
  # validate "weights"
  weights <- .validate_table_weights(weights, x)

  out <- lapply(select, function(i) {
    data_tabulate(
      x[[i]],
      by = by,
      proportions = proportions,
      drop_levels = drop_levels,
      weights = weights,
      remove_na = remove_na,
      name = i,
      verbose = verbose,
      ...
    )
  })

  if (is.null(by)) {
    class(out) <- c("datawizard_tables", "list")
  } else {
    class(out) <- c("datawizard_crosstabs", "list")
  }
  attr(out, "collapse") <- isTRUE(collapse)
  attr(out, "is_weighted") <- !is.null(weights)

  out
}


#' @export
data_tabulate.grouped_df <- function(x,
                                     select = NULL,
                                     exclude = NULL,
                                     ignore_case = FALSE,
                                     regex = FALSE,
                                     by = NULL,
                                     proportions = NULL,
                                     drop_levels = FALSE,
                                     weights = NULL,
                                     remove_na = FALSE,
                                     collapse = FALSE,
                                     verbose = TRUE,
                                     ...) {
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
    if (is.null(group_variables)) {
      group_variable <- NULL
    } else {
      group_variable <- group_variables[i, , drop = FALSE]
    }
    out <- c(out, data_tabulate(
      data_filter(x, rows),
      select = select,
      exclude = exclude,
      ignore_case = ignore_case,
      verbose = verbose,
      drop_levels = drop_levels,
      weights = weights,
      remove_na = remove_na,
      by = by,
      proportions = proportions,
      group_variable = group_variable,
      ...
    ))
  }
  if (is.null(by)) {
    class(out) <- c("datawizard_tables", "list")
  } else {
    class(out) <- c("datawizard_crosstabs", "list")
  }
  attr(out, "collapse") <- isTRUE(collapse)
  attr(out, "is_weighted") <- !is.null(weights)

  out
}


# methods --------------------

#' @importFrom insight print_html
#' @export
insight::print_html


#' @importFrom insight print_md
#' @export
insight::print_md


#' @rdname data_tabulate
#' @param add_total For crosstables (i.e. when `by` is not `NULL`), a row and
#' column with the total N values are added to the data frame. `add_total` has
#' no effect in `as.data.frame()` for simple frequency tables.
#' @inheritParams base::as.data.frame
#' @export
as.data.frame.datawizard_tables <- function(x,
                                            row.names = NULL,
                                            optional = FALSE,
                                            ...,
                                            stringsAsFactors = FALSE,
                                            add_total = FALSE) {
  # extract variables of frequencies
  selected_vars <- unlist(lapply(x, function(i) attributes(i)$varname))
  # coerce to data frame, remove rownames
  data_frames <- lapply(x, function(i) {
    # the `format()` methods for objects returned by `data_tabulate()` call
    # `as.data.frame()` - we have to pay attention to avoid infinite iterations
    # here. At the moment, this is no problem, as objects we have at this stage
    # are of class "datawizard_table" or "datawizard_crosstab", while this
    # `as.data.frame()` method is only called for "datawizard_tables" (the plural)
    # form). Else, we would need to modify the class attribute here,
    # e.g. class(i) <- "data.frame"
    if (add_total) {
      # to add the total column and row, we simply can call `format()`
      out <- as.data.frame(format(i))
      for (cols in 2:ncol(out)) {
        # since "format()" returns a character matrix, we want to convert
        # the columns to numeric. We have to exclude the first column, as the
        # first column is character, due to the added "Total" value.
        out[[cols]] <- as.numeric(out[[cols]])
      }
      # after formatting, we have a "separator" row for nicer printing.
      # this should also be removed
      out <- remove_empty_rows(out)
    } else {
      out <- as.data.frame(i)
    }
    rownames(out) <- NULL
    out
  })
  # create nested data frame
  result <- data.frame(
    var = selected_vars,
    table = I(data_frames),
    stringsAsFactors = stringsAsFactors
  )
  # consider additional arguments
  rownames(result) <- row.names
  result
}

#' @export
as.data.frame.datawizard_crosstabs <- as.data.frame.datawizard_tables


#' @export
format.datawizard_table <- function(x, format = "text", big_mark = NULL, ...) {
  # convert to character manually, else, for large numbers,
  # format_table() returns scientific notation
  x <- as.data.frame(x)
  x$N <- as.character(x$N)

  # format data frame
  ftab <- insight::format_table(x, ...)
  ftab[] <- lapply(ftab, function(i) {
    i[i == ""] <- ifelse(identical(format, "text"), "<NA>", "(NA)") # nolint
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
print.datawizard_table <- function(x, big_mark = NULL, ...) {
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
  summary_line <- sprintf(
    "# total N=%s valid N=%s%s\n\n",
    a$total_n,
    a$valid_n,
    ifelse(is.null(a$weights), "", " (weighted)")
  )
  cat(insight::print_color(summary_line, "blue"))

  # remove information that goes into the header/footer
  x$Variable <- NULL
  x$Group <- NULL

  # print table
  cat(insight::export_table(
    format(x, big_mark = big_mark, ...),
    cross = "+",
    missing = "<NA>",
    ...
  ))
  invisible(x)
}


#' @export
print_html.datawizard_table <- function(x, big_mark = NULL, ...) {
  a <- attributes(x)

  # "table" header with variable label/name, and type
  caption <- .table_header(x, "html")

  # summary of total and valid N (we may add mean/sd as well?)
  footer <- sprintf(
    "total N=%i valid N=%i%s",
    a$total_n,
    a$valid_n,
    ifelse(is.null(a$weights), "", " (weighted)")
  )

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
print_md.datawizard_table <- function(x, big_mark = NULL, ...) {
  a <- attributes(x)

  # "table" header with variable label/name, and type
  caption <- .table_header(x, "markdown")

  # summary of total and valid N (we may add mean/sd as well?)
  footer <- sprintf(
    "total N=%i valid N=%i%s\n\n",
    a$total_n,
    a$valid_n,
    ifelse(is.null(a$weights), "", " (weighted)")
  )

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
print.datawizard_tables <- function(x, big_mark = NULL, ...) {
  # check if we have weights
  is_weighted <- isTRUE(attributes(x)$is_weighted)

  a <- attributes(x)
  if (!isTRUE(a$collapse) || length(x) == 1) {
    for (i in seq_along(x)) {
      print(x[[i]], big_mark = big_mark, ...)
      if (i < length(x)) cat("\n")
    }
  } else {
    x <- lapply(x, function(i) {
      i_attr <- attributes(i)
      i <- format(i, format = "text", big_mark = big_mark, ...)
      i$Variable[i_attr$duplicate_varnames] <- ""
      if (!is.null(i$Group)) i$Group[i_attr$duplicate_varnames] <- ""
      i[nrow(i) + 1, ] <- ""
      i
    })

    out <- do.call(rbind, x)
    if (is_weighted) {
      cat(insight::print_color("# Frequency Table (weighted)\n\n", "blue"))
    } else {
      cat(insight::print_color("# Frequency Table\n\n", "blue"))
    }

    # print table
    cat(insight::export_table(
      out,
      missing = "<NA>",
      cross = "+",
      empty_line = "-",
      ...
    ))
  }
}


#' @export
print_html.datawizard_tables <- function(x, big_mark = NULL, ...) {
  # check if we have weights
  is_weighted <- isTRUE(attributes(x)$is_weighted)

  if (length(x) == 1) {
    print_html(x[[1]], big_mark = big_mark, ...)
  } else {
    x <- lapply(x, function(i) {
      i_attr <- attributes(i)
      i <- format(i, format = "html", big_mark = big_mark, ...)
      i$Variable[i_attr$duplicate_varnames] <- ""
      i
    })

    out <- do.call(rbind, x)

    # print table
    insight::export_table(
      out,
      missing = "<NA>",
      caption = ifelse(is_weighted, "Frequency Table (weighted)", "Frequency Table"),
      format = "html",
      group_by = "Group"
    )
  }
}


#' @export
print_md.datawizard_tables <- function(x, big_mark = NULL, ...) {
  # check if we have weights
  is_weighted <- isTRUE(attributes(x)$is_weighted)

  if (length(x) == 1) {
    print_md(x[[1]], big_mark = big_mark, ...)
  } else {
    x <- lapply(x, function(i) {
      i_attr <- attributes(i)
      i <- format(i, format = "markdown", big_mark = big_mark, ...)
      i$Variable[i_attr$duplicate_varnames] <- ""
      if (!is.null(i$Group)) i$Group[i_attr$duplicate_varnames] <- ""
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
      title = ifelse(is_weighted, "Frequency Table (weighted)", "Frequency Table")
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
    ord = "ordinal",
    fct = "categorical",
    dbl = "numeric",
    int = "integer",
    chr = "character",
    lbl = "labelled",
    cpl = "complex",
    lgl = "logical",
    vt
  )
}
