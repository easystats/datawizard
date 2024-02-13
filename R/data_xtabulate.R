# helper to compute crosstables --------------

.crosstable <- function(x,
                        by,
                        weights = NULL,
                        include_na = TRUE,
                        proportions = NULL,
                        obj_name = NULL,
                        group_variable = NULL) {
  if (!is.null(proportions)) {
    proportions <- match.arg(proportions, c("row", "column", "cell"))
  }
  # frequency table
  if (is.null(weights)) {
    if (include_na) {
      x_table <- tryCatch(table(addNA(x), addNA(by)), error = function(e) NULL)
    } else {
      x_table <- tryCatch(table(x, by), error = function(e) NULL)
    }
  } else {
    # weighted frequency table
    if (include_na) {
      x_table <- tryCatch(
        stats::xtabs(
          weights ~ x + by,
          data = data.frame(weights = weights, x = addNA(x), by = addNA(by)),
          na.action = stats::na.pass,
          addNA = TRUE
        ),
        error = function(e) NULL
      )
    } else {
      x_table <- tryCatch(
        stats::xtabs(
          weights ~ x + by,
          data = data.frame(weights = weights, x = x, by = by),
          na.action = stats::na.omit,
          addNA = FALSE
        ),
        error = function(e) NULL
      )
    }
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

  class(out) <- c("dw_data_xtabulate", "data.frame")

  out
}


# methods ---------------------


#' @export
format.dw_data_xtabulate <- function(x, format = "text", digits = 1, big_mark = NULL, ...) {
  # convert to character manually, else, for large numbers,
  # format_table() returns scientific notation
  x <- as.data.frame(x)

  # remove group variable
  x$Group <- NULL

  # compute total N for rows and colummns
  total_n <- attributes(x)$total_n
  total_column <- rowSums(x[, -1], na.rm = TRUE)
  total_row <- c(colSums(x[, -1], na.rm = TRUE), total_n)

  # proportions?
  props <- attributes(x)$proportions

  if (!is.null(props)) {
    # we copy x to tmp, because when we create strings with "sprintf()", the
    # variable is coerced to character, and in subsequent iterations of the loop,
    # mathemathical operations are not possible anymore
    tmp <- x
    if (identical(props, "row")) {
      for (i in seq_len(nrow(x))) {
        tmp[i, -1] <- sprintf(
          "%s (%.*f%%)",
          x[i, -1],
          digits,
          100 * x[i, -1] / sum(x[i, -1], na.rm = TRUE)
        )
      }
    } else if (identical(props, "column")) {
      for (i in seq_len(ncol(x))[-1]) {
        tmp[, i] <- sprintf(
          "%s (%.*f%%)",
          x[, i],
          digits,
          100 * x[, i] / sum(x[, i], na.rm = TRUE)
        )
      }
    } else if (identical(props, "cell")) {
      for (i in seq_len(ncol(x))[-1]) {
        tmp[, i] <- sprintf(
          "%s (%.*f%%)",
          x[, i],
          digits,
          100 * x[, i] / total_n
        )
      }
    }
    # copy back final result
    x <- tmp
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
  # for text format, insert "empty row" before last total row
  if (identical(format, "text")) {
    sub <- as.data.frame(t(data.frame(
      rep("", ncol(ftab)),
      c("Total", as.character(total_row)),
      stringsAsFactors = FALSE
    )))
  } else {
    sub <- as.data.frame(t(data.frame(
      c("Total", as.character(total_row)),
      stringsAsFactors = FALSE
    )))
  }
  colnames(sub) <- colnames(ftab)
  ftab <- rbind(ftab, sub)
  ftab[nrow(ftab), ] <- gsub("\\.00$", "", ftab[nrow(ftab), ])

  # insert big marks?
  ftab$Total <- .add_commas_in_numbers(ftab$Total, big_mark)
  ftab[nrow(ftab), ] <- .add_commas_in_numbers(ftab[nrow(ftab), ], big_mark)

  ftab
}


#' @export
print.dw_data_xtabulate <- function(x, big_mark = NULL, ...) {
  # grouped data? if yes, add information on grouping factor
  if (is.null(x[["Group"]])) {
    caption <- NULL
  } else {
    caption <- paste0("Grouped by ", x[["Group"]][1])
    x$Group <- NULL
  }

  # print table
  cat(insight::export_table(
    format(x, big_mark = big_mark, ...),
    cross = "+",
    missing = "<NA>",
    caption = caption,
    empty_line = "-"
  ))
  invisible(x)
}


#' @export
print_html.dw_data_xtabulate <- function(x, big_mark = NULL, ...) {
  # grouped data? if yes, add information on grouping factor
  if (!is.null(x[["Group"]])) {
    x$groups <- paste0("Grouped by ", x[["Group"]][1])
    x$Group <- NULL
  }

  # print table
  insight::export_table(
    format(x, big_mark = big_mark, format = "html", ...),
    missing = "(NA)",
    format = "html",
    group_by = "groups"
  )
}


#' @export
print.dw_data_xtabulates <- function(x, big_mark = NULL, ...) {
  for (i in seq_along(x)) {
    print(x[[i]], big_mark = big_mark, ...)
    cat("\n")
  }
  invisible(x)
}


#' @export
print_html.dw_data_xtabulates <- function(x, big_mark = NULL, ...) {
  if (length(x) == 1) {
    print_html(x[[1]], big_mark = big_mark, ...)
  } else {
    x <- lapply(x, function(i) {
      # grouped data? if yes, add information on grouping factor
      if (!is.null(i[["Group"]])) {
        i$groups <- paste0("Grouped by ", i[["Group"]][1])
        i$Group <- NULL
      }
      format(i, format = "html", big_mark = big_mark, ...)
    })

    out <- do.call(rbind, x)

    # print table
    insight::export_table(
      out,
      missing = "(NA)",
      format = "html",
      group_by = "groups"
    )
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


.validate_tableweights <- function(weights, x) {
  if (!is.null(weights)) {
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
