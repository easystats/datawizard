# helper to compute crosstables --------------

.crosstable <- function(x,
                        by,
                        weights = NULL,
                        remove_na = FALSE,
                        proportions = NULL,
                        obj_name = NULL,
                        group_variable = NULL) {
  if (!is.null(proportions)) {
    proportions <- match.arg(proportions, c("row", "column", "full"))
  }
  # frequency table
  if (is.null(weights)) {
    # we have a `.default` and a `.data.frame` method for `data_tabulate()`.
    # since this is the default, `x` can be an object which cannot be used
    # with `table()`, that's why we add `tryCatch()` here. Below we give an
    # informative error message for non-supported objects.
    if (remove_na) {
      x_table <- tryCatch(table(x, by), error = function(e) NULL)
    } else {
      x_table <- tryCatch(table(addNA(x), addNA(by)), error = function(e) NULL)
    }
  } else if (remove_na) {
    # weighted frequency table, excluding NA
    x_table <- tryCatch(
      stats::xtabs(
        weights ~ x + by,
        data = data.frame(weights = weights, x = x, by = by),
        na.action = stats::na.omit,
        addNA = FALSE
      ),
      error = function(e) NULL
    )
  } else {
    # weighted frequency table, including NA
    x_table <- tryCatch(
      stats::xtabs(
        weights ~ x + by,
        data = data.frame(weights = weights, x = addNA(x), by = addNA(by)),
        na.action = stats::na.pass,
        addNA = TRUE
      ),
      error = function(e) NULL
    )
  }

  if (is.null(x_table)) {
    insight::format_warning(paste0("Can't compute cross tables for objects of class `", class(x)[1], "`."))
    return(NULL)
  }

  out <- as.data.frame(stats::ftable(x_table))
  colnames(out) <- c("Value", "by", "N")
  total_n <- sum(out$N, na.rm = TRUE)

  # we want to round N for weighted frequencies
  if (!is.null(weights)) {
    out$N <- round(out$N)
    total_n <- round(total_n)
  }

  out <- data_to_wide(out, values_from = "N", names_from = "by")

  # use variable name as column name
  if (!is.null(obj_name)) {
    colnames(out)[1] <- obj_name
  }

  # for grouped data frames, add info about grouping variables
  if (!is.null(group_variable)) {
    var_info <- toString(lapply(colnames(group_variable), function(i) {
      sprintf("%s (%s)", i, group_variable[[i]])
    }))
    out <- cbind(out[1], data.frame(Group = var_info, stringsAsFactors = FALSE), out[-1])
  }

  attr(out, "total_n") <- total_n
  attr(out, "weights") <- weights
  attr(out, "proportions") <- proportions
  attr(out, "varname") <- obj_name
  attr(out, "grouped_df") <- !is.null(group_variable)
  attr(out, "prop_table") <- .prop_table(out)

  class(out) <- c("datawizard_crosstab", "data.frame")

  out
}

# Helper function to calculate a table of proportions from a frequency table
.prop_table <- function(x) {
  # Extract the "proportions" attribute, which determines the type of calculation (row, column, or full)
  props <- attributes(x)$proportions
  out <- NULL

  # Proceed only if the "proportions" attribute is set
  if (!is.null(props)) {
    # Identify numeric columns, as proportions are only calculated for these
    numeric_columns <- vapply(x, is.numeric, logical(1))
    # Get the total count from the attributes, used for "full" proportions
    total_n <- attributes(x)$total_n

    # Use a switch to perform the calculation based on the "props" value
    out <- switch(
      props,
      # Calculate row-wise proportions
      row = lapply(seq_len(nrow(x)), function(i) {
        # Sum of the current row's numeric values
        row_sum <- sum(x[i, numeric_columns], na.rm = TRUE)
        # Avoid division by zero; if row sum is 0, return a row of zeros
        if (row_sum == 0) {
          tmp <- as.data.frame(as.list(rep(0, sum(numeric_columns))))
          # for later rbind, we need identical column names
          colnames(tmp) <- colnames(x)[numeric_columns]
          tmp
        } else {
          x[i, numeric_columns] / row_sum
        }
      }),
      # Calculate column-wise proportions
      column = lapply(seq_len(ncol(x))[numeric_columns], function(i) {
        # Sum of the current column's values
        col_sum <- sum(x[, i], na.rm = TRUE)
        # Avoid division by zero; if column sum is 0, return a vector of zeros
        if (col_sum == 0) {
          rep(0, nrow(x))
        } else {
          x[, i] / col_sum
        }
      }),
      # Calculate proportions relative to the total count of the entire table
      full = lapply(seq_len(ncol(x))[numeric_columns], function(i) {
        # Avoid division by zero; if total is 0, return a vector of zeros
        if (total_n == 0) {
          rep(0, nrow(x))
        } else {
          x[, i] / total_n
        }
      })
    )
  }

  # If a proportion table was calculated, format it into a data frame
  if (!is.null(out)) {
    # The output of the switch is a list. We need to bind it into a data frame.
    # For row proportions, we bind rows. For column/full, we bind columns.
    out <- switch(
      props,
      row = as.data.frame(do.call(rbind, out)),
      as.data.frame(do.call(cbind, out))
    )
    # Set the column names of the new proportion table
    colnames(out) <- colnames(x)[numeric_columns]

    # Check if the dimensions are consistent before setting row names
    if (nrow(out) == nrow(x)) {
      # If the first column of the original data is not numeric, it's likely a
      # label column. Use these labels as row names in the output for better
      # readability. This is useful for identifying rows, especially when NAs
      # are present.
      if (isFALSE(numeric_columns[1])) {
        r_names <- x[[1]]
        r_names <- as.character(r_names)
        # Replace NA in labels with the string "NA", else we cannot set rownames
        r_names[is.na(r_names)] <- "NA"
        rownames(out) <- r_names
      } else {
        # Otherwise, just use the original row names
        rownames(out) <- rownames(x)
      }
    }
  }

  out
}


# methods ---------------------

#' @export
format.datawizard_crosstab <- function(x,
                                       format = "text",
                                       digits = 1,
                                       big_mark = NULL,
                                       include_total_row = TRUE,
                                       ...) {
  # convert to character manually, else, for large numbers,
  # format_table() returns scientific notation
  x <- as.data.frame(x)

  # find numeric columns, only for these we need row/column sums
  numeric_columns <- which(vapply(x, is.numeric, logical(1)))

  # compute total N for rows and columns
  total_n <- attributes(x)$total_n
  total_column <- rowSums(x[numeric_columns], na.rm = TRUE)
  total_row <- c(colSums(x[numeric_columns], na.rm = TRUE), total_n)

  # proportions?
  props <- attributes(x)$proportions
  prop_table <- attributes(x)$prop_table

  if (!is.null(props) && !is.null(prop_table)) {
    for (i in seq_len(ncol(prop_table))) {
      x[, numeric_columns[i]] <- paste(
        format(x[, numeric_columns[i]]),
        format(
          ifelse(
            prop_table[, i] == 0,
            "(0.0%)",
            sprintf("(%.*f%%)", digits, 100 * prop_table[, i])
          ),
          justify = "right"
        )
      )
    }
  }

  x[] <- lapply(x, as.character)

  # format data frame
  ftab <- insight::format_table(x, ...)
  # replace empty cells with NA
  ftab[] <- lapply(ftab, function(i) {
    i[i == ""] <- ifelse(identical(format, "text"), "<NA>", "(NA)") # nolint
    i
  })
  # Remove ".00" from numbers
  ftab$Total <- gsub("\\.00$", "", as.character(total_column))

  # add final total row to each sub-table. For multiple, collapsed table
  # (i.e. when length of `by` > 1), we don't want multiple total rows in the
  # table, so we would set include_total_row = FALSE for objects of class
  # `datawizard_crosstabs` (note plural s!)
  if (include_total_row) {
    # for text format, insert "empty row" before last total row
    if (identical(format, "text") || identical(format, "markdown")) {
      empty_row <- as.data.frame(t(data.frame(
        rep("", ncol(ftab)),
        c("Total", as.character(total_row)),
        stringsAsFactors = FALSE
      )))
    } else {
      empty_row <- as.data.frame(t(data.frame(
        c("Total", as.character(total_row)),
        stringsAsFactors = FALSE
      )))
    }
    colnames(empty_row) <- colnames(ftab)
    ftab <- rbind(ftab, empty_row)
    ftab[nrow(ftab), ] <- gsub("\\.00$", "", ftab[nrow(ftab), ])
  }

  # insert big marks?
  ftab$Total <- .add_commas_in_numbers(ftab$Total, big_mark)
  ftab[nrow(ftab), ] <- .add_commas_in_numbers(ftab[nrow(ftab), ], big_mark)

  # also format NA column name
  colnames(ftab)[colnames(ftab) == "NA"] <- ifelse(
    identical(format, "text"),
    "<NA>",
    "(NA)"
  )

  ftab
}


# print, datawizard_crosstab ---------------------


#' @export
print.datawizard_crosstab <- function(x, big_mark = NULL, ...) {
  .print_text_table(x, big_mark, format = "text", ...)
  invisible(x)
}


#' @export
print_md.datawizard_crosstab <- function(x, big_mark = NULL, ...) {
  .print_text_table(x, big_mark, format = "markdown", ...)
}


#' @export
print_html.datawizard_crosstab <- function(x, big_mark = NULL, ...) {
  .print_text_table(x, big_mark, format = "html", ...)
}


.print_text_table <- function(x, big_mark = NULL, format = "text", ...) {
  # grouped data? if yes, add information on grouping factor
  if (is.null(x[["Group"]])) {
    caption <- NULL
  } else {
    caption <- paste0("Grouped by ", x[["Group"]][1])
    x$Group <- NULL
  }

  # this function is used by all four supported format, markdown, text, html
  # and tt (tinytable). For tt, we sometimes have format "html" and backend = "tt",
  # so we need to check for this special case
  backend <- switch(format,
    html = ,
    tt = .check_format_backend(...),
    format
  )
  # prepare table arguments
  fun_args <- list(
    format(x, big_mark = big_mark, format = format, ...),
    caption = caption,
    format = backend
  )
  if (!format %in% c("html", "tt")) {
    fun_args$cross <- "+"
    fun_args$empty_line <- "-"
  }
  if (format == "text") {
    fun_args$missing <- "<NA>"
  } else {
    fun_args$missing <- "(NA)"
  }
  out <- do.call(insight::export_table, c(fun_args, list(...)))

  # print table
  if (identical(format, "text")) {
    cat(out)
  } else {
    out
  }
}


# print, datawizard_crosstabs ---------------------


#' @export
print.datawizard_crosstabs <- function(x, big_mark = NULL, ...) {
  .print_text_tables(x, big_mark, format = "text", ...)
  invisible(x)
}


#' @export
print_md.datawizard_crosstabs <- function(x, big_mark = NULL, ...) {
  .print_text_tables(x, big_mark, format = "markdown", ...)
}


#' @export
print_html.datawizard_crosstabs <- function(x, big_mark = NULL, ...) {
  .print_text_tables(x, big_mark, format = "html", ...)
}


.print_text_tables <- function(x, big_mark = NULL, format = "text", ...) {
  if (length(x) == 1) {
    .print_text_table(x[[1]], big_mark = big_mark, format = format, ...)
  } else {
    x <- lapply(x, function(i) {
      # grouped data? if yes, add information on grouping factor
      if (!is.null(i[["Group"]])) {
        i$groups <- paste0("Grouped by ", i[["Group"]][1])
        i$Group <- NULL
      }
      # if we don't have the gt-grouping variable "groups" yet, we use it now
      # for grouping. Else, we use a new column named "Variable", to avoid
      # overwriting the groups-variable from grouped data frames
      if (is.null(i$groups) && identical(format, "html")) {
        grp_variable <- "groups"
      } else {
        grp_variable <- "Variable"
      }
      # first variable differs for each data frame, so we harmonize it here
      i[[grp_variable]] <- colnames(i)[1]
      colnames(i)[1] <- "Value"
      # move column to first position
      i <- data_relocate(i, select = grp_variable, before = 1)
      # format data frame
      format(i, format = format, big_mark = big_mark, include_total_row = FALSE, ...)
    })
    # now bind, but we need to check for equal number of columns
    if (all(lengths(x) == max(length(x)))) {
      out <- do.call(rbind, x)
    } else {
      # if not all tables have identical columns, we can use "data_merge()",
      # which safely row-binds all data frames. However, the column order can be
      # messed up, so we save column order here and restore it later
      col_order <- colnames(x[[which.max(lengths(x))]])
      out <- data_merge(x, join = "bind")[col_order]
    }

    # split tables for grouped data frames
    if (!is.null(out$groups)) {
      out <- split(out, out$groups)
      out <- lapply(out, function(subtable) {
        # for text and markdown, if we split tables, we remove the "groups"
        # variable. we need to keep it for HTML tables.
        if (!identical(format, "html")) {
          attr(subtable, "table_caption") <- c(unique(subtable$groups), "blue")
          subtable$groups <- NULL
        }
        # remove duplicated names
        for (grpvars in c("Variable", "Group")) {
          if (!is.null(subtable[[grpvars]])) {
            subtable[[grpvars]][duplicated(subtable[[grpvars]])] <- ""
          }
        }
        subtable
      })
      # no splitting of grouped data frames into list for HTML format,
      # because splitting is done by the `by` argument later
      if (identical(format, "html")) {
        out <- do.call(rbind, out)
      }
    }

    # prepare table arguments
    fun_args <- list(
      out,
      format = format
    )
    if (format != "html") {
      fun_args$cross <- "+"
      fun_args$empty_line <- "-"
    } else {
      fun_args$by <- "groups"
    }
    if (format == "text") {
      fun_args$missing <- "<NA>"
    } else {
      fun_args$missing <- "(NA)"
    }
    out <- do.call(insight::export_table, c(fun_args, list(...)))

    # print table
    if (identical(format, "text")) {
      cat(out)
    } else {
      out
    }
  }
}


# helper ---------------------


.validate_by <- function(by, x) {
  if (!is.null(by)) {
    if (is.character(by)) {
      # If "by" is a character string, must be of length 1
      if (length(by) > 1) {
        insight::format_error(
          "If `by` is a string indicating a variable name, `by` must be of length 1.",
          "You may use `data_group()` to group by multiple variables, then call `data_tabulate()`."
        )
      }
      # if "by" is a character, "x" must be a data frame
      if (!is.data.frame(x)) {
        insight::format_error("If `by` is a string indicating a variable name, `x` must be a data frame.")
      }
      # is "by" a column in "x"?
      if (!by %in% colnames(x)) {
        insight::format_error(sprintf(
          "The variable specified in `by` was not found in `x`. %s",
          .misspelled_string(names(x), by, "Possibly misspelled?")
        ))
      }
      by <- x[[by]]
    }
    # is "by" of same length as "x"?
    if (is.data.frame(x) && length(by) != nrow(x)) {
      insight::format_error("Length of `by` must be equal to number of rows in `x`.") # nolint
    }
    if (!is.data.frame(x) && length(by) != length(x)) {
      insight::format_error("Length of `by` must be equal to length of `x`.") # nolint
    }
    if (!is.factor(by)) {
      # coerce "by" to factor, including labels
      by <- to_factor(by, labels_to_levels = TRUE, verbose = FALSE)
    }
  }

  by
}


.validate_table_weights <- function(weights, x, weights_expression = NULL) {
  # exception: for vectors, if weighting variable not found, "weights" is NULL.
  # to check this, we further need to check whether a weights expression was
  # provided, e.g. "weights = iris$not_found" - all this is only relevant when
  # weights is NULL
  if (is.null(weights)) {
    # possibly misspelled weights-variables for default-method ----------------
    # -------------------------------------------------------------------------

    # do we have any value for weights_expression?
    if (!is.null(weights_expression) &&
      # due to deparse() and substitute, NULL becomes "NULL" - we need to check for this
      !identical(weights_expression, "NULL") &&
      # we should only run into this problem, when a variable from a data frame
      # is used in the data_tabulate() method for vectors - thus, we need to check
      # whether the weights_expression contains a "$" - `iris$not_found` is "NULL"
      # we need this check, because the default-method of data_tabulate() is called
      # from the data.frame method, where `weights = weights`, and then,
      # deparse(substitute(weights)) is "weights" (not "NULL" or "iris$not_found"),
      # leading to an error when actually all is OK (if "weights" is NULL)
      # Example:
      #> efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
      # Here, efc$wweight is NULL
      #> data_tabulate(efc$c172code, weights = efc$wweight)
      # Here, wweight errors anyway, because object "wweight" is not found
      #> data_tabulate(efc$c172code, weights = wweight)
      grepl("$", weights_expression, fixed = TRUE)) {
      insight::format_error("The variable specified in `weights` was not found. Possibly misspelled?")
    }
  } else {
    # possibly misspecified weights-variables for data.frame-method -----------
    # -------------------------------------------------------------------------

    if (is.character(weights)) {
      # If "weights" is a character string, must be of length 1
      if (length(weights) > 1) {
        insight::format_error(
          "If `weights` is a string indicating a variable name, `weights` must be of length 1."
        )
      }
      # if "weights" is a character, "x" must be a data frame
      if (!is.data.frame(x)) {
        insight::format_error("If `weights` is a string indicating a variable name, `x` must be a data frame.") # nolint
      }
      # is "by" a column in "x"?
      if (!weights %in% colnames(x)) {
        insight::format_error(sprintf(
          "The variable specified in `weights` was not found in `x`. %s",
          .misspelled_string(names(x), weights, "Possibly misspelled?")
        ))
      }
      weights <- x[[weights]]
    }
    # is "by" of same length as "x"?
    if (is.data.frame(x) && length(weights) != nrow(x)) {
      insight::format_error("Length of `weights` must be equal to number of rows in `x`.") # nolint
    }
    if (!is.data.frame(x) && length(weights) != length(x)) {
      insight::format_error("Length of `weights` must be equal to length of `x`.") # nolint
    }
  }

  weights
}
