#' Generate a codebook of a data frame.
#'
#' `data_codebook()` does...
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @param variable_label_width Length of variable labels. Longer labels will be
#' wrapped at `variable_label_width` chars. If `NULL`, longer labels will not
#' be split into multiple lines.
#' @param value_label_width Length of value labels. Longer labels will be
#' shortened, where the remaining part is truncated.
#' @param max_values Number of maximum values that should be displayed. Can be
#' used to avoid too many rows when variables have lots of unique values.
#' @inheritParams standardize.data.frame
#' @inheritParams find_columns
#'
#' @return A data frame.
#'
#' @examples
#' data(iris)
#' data_codebook(iris, select = starts_with("Sepal"))
#'
#' data(efc)
#' data_codebook(efc)
#' @export
data_codebook <- function(data,
                          select = NULL,
                          exclude = NULL,
                          variable_label_width = NULL,
                          value_label_width = NULL,
                          max_values = 10,
                          ignore_case = FALSE,
                          regex = FALSE,
                          verbose = TRUE,
                          ...) {
  data_name <- insight::safe_deparse_symbol(substitute(data))

  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    data,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  # check for emtpy columns, and remove
  empty <- empty_columns(data[select])
  if (length(empty)) {
    if (verbose) {
      insight::format_warning(
        sprintf("Following %i columns were empty and have been removed:", length(empty)),
        text_concatenate(names(empty))
      )
      select <- select[-empty]
    }
  }

  # needed for % NA
  rows <- nrow(data)
  max_values <- max_values + 1

  out <- lapply(seq_along(select), function(id) {

    # variable
    x <- data[[select[id]]]
    x_na <- is.na(x)

    # inital data frame for codebook
    d <- data.frame(
      ID = which(colnames(data) == select[id]),
      Name = select[id],
      Type = .variable_type(x),
      Missings = sprintf("%g (%.1f%%)", sum(x_na), 100 * (sum(x_na) / rows)),
      stringsAsFactors = FALSE,
      row.names = NULL,
      check.names = FALSE
    )

    # check if there are variable labels
    varlab <- attr(x, "label", exact = TRUE)
    if (!is.null(varlab) && length(varlab)) {
      variable_label <- varlab
      # if variable labels are too long, split into multiple elements
      if (!is.null(variable_label_width) && nchar(variable_label) > variable_label_width) {
        variable_label <- insight::trim_ws(unlist(strsplit(
          text_wrap(variable_label, width = variable_label_width),
          "\n",
          fixed = TRUE
        )))
      }
    } else {
      variable_label <- NA
    }

    # we may need to remove duplicated value range elements
    flag_range <- FALSE

    # save value labels
    vallab <- attr(x, "labels", exact = TRUE)

    # coerce to factor, for tabulate
    if (!is.numeric(x) && !is.factor(x)) {
      x <- as.factor(x)
    }

    # get unique values, to remove non labelled data
    unique_values <- unique(x)
    unique_values <- unique_values[!is.na(unique_values)]

    # handle labelled data - check if there are value labels or factor levels,
    # and extract values and N
    if (!is.null(vallab) && length(vallab)) {
      # if not all values are labelled, fill in value labels
      if (!all(unique_values %in% vallab)) {
        new_vals <- setdiff(unique_values, vallab)
        vallab <- c(vallab, stats::setNames(new_vals, new_vals))
      }
      # if not all value labels are present in the data, remove unused value labels
      if (!all(vallab %in% unique_values)) {
        not_needed <- setdiff(vallab, unique_values)
        vallab <- vallab[-not_needed]
      }
      # we now should have the same length of value labels and labelled values
      # which should also match the numberof unique values in the vector.
      # "tabulate" creates frequency tables by sorting by values/levels, so
      # we need to make sure that labels are also in sorted order.
      value_labels <- names(vallab)[order(unname(vallab))]
      values <- sort(unname(vallab))
      frq <- tabulate(x)

    # handle factors
    } else if (is.factor(x)) {
      values <- levels(x)
      value_labels <- NA
      frq <- tabulate(x)

    # handle numerics
    } else {
      value_labels <- NA
      r <- range(x, na.rm = TRUE)
      values <- sprintf("[%g, %g]", r[1], r[2])
      frq <- sum(!x_na)
      flag_range <- length(variable_label) > 1
    }

    # tabulate fills 0 for non-existend values, remove those
    frq <- frq[frq != 0]

    # make sure we have not too long rows, e.g. for variables that
    # have dozens of unique values
    if (length(value_labels) > max_values) {
      value_labels <- value_labels[1:max_values]
      value_labels[max_values] <- "(...)"
    }
    if (length(frq) > max_values) {
      frq <- frq[1:max_values]
      frq[max_values] <- NA
    }
    if (length(values) > max_values) {
      values <- values[1:max_values]
      values[max_values] <- "(...)"
    }

    # make sure length recycling doesn't fail, e.g. if we have split
    # variable_label into two lines (i.e. vector of length 2), but we have
    # 7 values in "frq", creating the data frame will fail. In this case,
    # we have to make sure that recycling shorter vectors works.
    if (length(variable_label) > 1 && !flag_range) {
      variable_label <- variable_label[seq_along(frq)]
    }

    # shorten value labels
    if (!is.null(value_label_width)) {
      value_labels <- insight::format_string(value_labels, length = value_label_width)
    }

    # add values, value labels and frequencies to data frame
    d <- cbind(d, data.frame(variable_label, values, value_labels, frq, stringsAsFactors = FALSE))

    # which columns need to be checked for duplicates?
    duplicates <- c("ID", "Name", "Type", "Missings", "variable_label")
    if (isTRUE(flag_range)) {
      # when we have numeric variables with value range as values, and when
      # these variables had long variable labels that have been wrapped,
      # the range value is duplicated (due to recycling), so we need to fix
      # this here.
      duplicates <- c(duplicates, c("values", "frq"))
    }

    # clear duplicates due to recycling
    for (i in duplicates) {
      d[[i]][duplicated(d[[i]])] <- ""
    }

    # remove empty rows
    d <- remove_empty_rows(d)

    # add empty row at the end, as separator
    d[nrow(d) + 1, ] <- rep("", ncol(d))
    d
  })

  out <- do.call(rbind, out)

  # rename
  pattern <- c("variable_label", "values", "value_labels", "frq")
  replacement <- c("Label", "Values", "Value Labels", "N")
  for (i in seq_along(pattern)) {
    names(out) <- replace(names(out), names(out) == pattern[i], replacement[i])
  }

  # remove all empty columns
  out <- remove_empty_columns(out)

  # reorder
  column_order <- c("ID", "Name", "Label", "Type", "Missings", "Values", "Value Labels", "N")
  out <- out[union(intersect(column_order, names(out)), names(out))]

  attr(out, "data_name") <- data_name
  attr(out, "total_n") <- nrow(data)
  class(out) <- c("data_cookbook", "data.frame")

  out
}


# methods ----------------------


#' @export
print.data_cookbook <- function(x, ...) {
  caption <- c(sprintf(
    "%s (total N=%i)",
    attributes(x)$data_name,
    attributes(x)$total_n
  ), "blue")
  cat(insight::export_table(x, title = caption, empty_line = "-", cross = "+"))
}

#' @export
print_html.data_cookbook <- function(x, ...) {
  caption <- sprintf(
    "%s (total N=%i)",
    attributes(x)$data_name,
    attributes(x)$total_n
  )
  attr(x, "table_caption") <- caption
  insight::export_table(x, title = caption, format = "html")
}

#' @export
print_md.data_cookbook <- function(x, ...) {
  caption <- sprintf(
    "%s (total N=%i)",
    attributes(x)$data_name,
    attributes(x)$total_n
  )
  attr(x, "table_caption") <- caption
  insight::export_table(x, title = caption, format = "markdown")
}
