#' Generate a codebook of a data frame.
#'
#' `data_codebook()` generates codebooks from data frames, i.e. overviews
#' of all variables and some more information about each variable (like
#' labels, values or value range, frequencies, amount of missing values).
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @param variable_label_width Length of variable labels. Longer labels will be
#' wrapped at `variable_label_width` chars. If `NULL`, longer labels will not
#' be split into multiple lines. Only applies to _labelled data_.
#' @param value_label_width Length of value labels. Longer labels will be
#' shortened, where the remaining part is truncated. Only applies to
#' _labelled data_ or factor levels.
#' @param range_at Indicates how many unique values in a numeric vector are
#' needed in order to print a range for that variable instead of a frequency
#' table for all numeric values. Can be useful if the data contains numeric
#' variables with only a few unique values and where full frequency tables
#' instead of value ranges should be displayed.
#' @param max_values Number of maximum values that should be displayed. Can be
#' used to avoid too many rows when variables have lots of unique values.
#' @param font_size For HTML tables, the font size.
#' @param line_padding For HTML tables, the distance (in pixel) between lines.
#' @param row_color For HTML tables, the fill color for odd rows.
#' @inheritParams standardize.data.frame
#' @inheritParams find_columns
#'
#' @return A formatted data frame, summarizing the content of the data frame.
#' Returned columns include the column index of the variables in the original
#' data frame (`ID`), column name, variable label (if data is labelled), type
#' of variable, number of missing values, unique values (or value range),
#' value labels (for labelled data), and a frequency table (N for each value).
#' Most columns are formatted as character vectors.
#'
#' @note There are methods to `print()` the data frame in a nicer output, as
#' well methods for printing in markdown or HTML format (`print_md()` and
#' `print_html()`).
#'
#' @examples
#' data(iris)
#' data_codebook(iris, select = starts_with("Sepal"))
#'
#' data(efc)
#' data_codebook(efc)
#'
#' # shorten labels
#' data_codebook(efc, variable_label_width = 20, value_label_width = 15)
#'
#' # automatic range for numerics at more than 5 unique values
#' data(mtcars)
#' data_codebook(mtcars, select = starts_with("c"))
#'
#' # force all values to be displayed
#' data_codebook(mtcars, select = starts_with("c"), range_at = 100)
#' @export
data_codebook <- function(data,
                          select = NULL,
                          exclude = NULL,
                          variable_label_width = NULL,
                          value_label_width = NULL,
                          max_values = 10,
                          range_at = 6,
                          ignore_case = FALSE,
                          regex = FALSE,
                          verbose = TRUE,
                          ...) {
  data_name <- insight::safe_deparse(substitute(data))

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
    }
    select <- select[-empty]
  }

  # needed for % NA
  rows <- nrow(data)
  max_values <- max_values + 1

  out <- lapply(seq_along(select), function(id) {
    # variable
    x <- data[[select[id]]]
    x_na <- is.na(x)
    x_inf <- is.infinite(x)

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
        ), use.names = FALSE))
      }
    } else {
      variable_label <- NA
    }

    # we may need to remove duplicated value range elements
    flag_range <- FALSE

    # save value labels
    vallab <- attr(x, "labels", exact = TRUE)

    # do we have labelled NA values? If so, include labelled NAs in count table
    # we do this by converting NA values into character strings
    if (anyNA(vallab) && insight::check_if_installed("haven", quietly = TRUE)) {
      # get na-tags, i.e. the value labels for the different NA values
      na_labels <- haven::na_tag(vallab)
      # replace NA in labels with NA tags
      vallab[!is.na(na_labels)] <- stats::setNames(
        paste0("NA(", na_labels[!is.na(na_labels)], ")"),
        names(vallab[!is.na(na_labels)])
      )
      # replace tagged NAs in variable with their values, tagged as NA(value)
      na_values <- haven::na_tag(x)
      # need to convert, we still have haven-class, which cannot coerce
      x <- as.character(x)
      x[!is.na(na_values)] <- paste0("NA(", na_values[!is.na(na_values)], ")")
      # update information on NA - we still might have non-labelled (regular) NA
      x_na <- is.na(x)
    }

    # remove NA and Inf, for tabulate(). as.factor() will convert NaN
    # to a factor level "NaN", which we don't want here (same for Inf),
    # because tabulate() will then return frequencies for that level, too
    x <- x[!(x_na | x_inf)]

    # get unique values, to remove non labelled data
    unique_values <- unique(x)

    # coerce to factor, for tabulate(). We will coerce numerics to factor later
    # which is required because tabulate() doesn't return frequencies for values
    # lower than 1
    if (!is.numeric(x) && !is.factor(x)) {
      x <- as.factor(x)
    }

    # for ranges, we don't want the N% value, so use this to flag range-values
    is_range <- FALSE

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
        # match not needed values in vallab vector - values from labels
        # may not be in sorted order (e.g. 1, 2, 3, -9), or may be character
        # vectors in case of tagged NAs, so we have to make sure we know which
        # values can be removed from vallab
        not_needed <- stats::na.omit(match(not_needed, vallab))
        vallab <- vallab[-not_needed]
      }
      # we now should have the same length of value labels and labelled values
      # which should also match the numberof unique values in the vector.
      # "tabulate" creates frequency tables by sorting by values/levels, so
      # we need to make sure that labels are also in sorted order.
      value_labels <- names(vallab)[order(unname(vallab))]
      values <- sort(unname(vallab))
      frq <- tabulate(as.factor(x))

      # handle factors
    } else if (is.factor(x)) {
      values <- levels(x)
      value_labels <- NA
      frq <- tabulate(x)

      # handle numerics
    } else {
      value_labels <- NA
      # only range for too many unique values
      if (length(unique_values) >= range_at) {
        r <- range(x, na.rm = TRUE)
        values <- sprintf("[%g, %g]", round(r[1], 2), round(r[2], 2))
        frq <- sum(!x_na)
        flag_range <- length(variable_label) > 1
        is_range <- TRUE
        # if we have few values, we can print whole freq. table
      } else {
        values <- sort(unique_values)
        frq <- tabulate(as.factor(x))
      }
    }

    # tabulate fills 0 for non-existing values, remove those
    frq <- frq[frq != 0]

    # add Inf values?
    if (any(x_inf) && length(frq) <= max_values) {
      values <- c(values, Inf)
      if (!is.na(value_labels)) {
        value_labels <- c(value_labels, "infinite")
      }
      frq <- c(frq, sum(x_inf))
      # Inf are added as value, so don't flag range any more,
      # since we now have proportions for the range and the inf values.
      is_range <- FALSE
    }

    # add proportions, but not for ranges, since these are always 100%
    if (is_range) {
      proportions <- ""
    } else {
      proportions <- sprintf("%.1f%%", round(100 * (frq / sum(frq)), 1))
    }

    # make sure we have not too long rows, e.g. for variables that
    # have dozens of unique values
    if (length(value_labels) > max_values) {
      value_labels <- value_labels[1:max_values]
      value_labels[max_values] <- "(...)"
    }
    if (length(frq) > max_values) {
      frq <- frq[1:max_values]
      proportions <- proportions[1:max_values]
      frq[max_values] <- NA
      proportions[max_values] <- NA
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
    d <- cbind(d, data.frame(
      variable_label,
      values,
      value_labels,
      frq,
      proportions,
      stringsAsFactors = FALSE
    ))

    # which columns need to be checked for duplicates?
    duplicates <- c("ID", "Name", "Type", "Missings", "variable_label")
    if (isTRUE(flag_range)) {
      # when we have numeric variables with value range as values, and when
      # these variables had long variable labels that have been wrapped,
      # the range value is duplicated (due to recycling), so we need to fix
      # this here.
      duplicates <- c(duplicates, c("values", "frq", "proportions"))
    }

    # clear duplicates due to recycling
    for (i in duplicates) {
      d[[i]][duplicated(d[[i]])] <- ""
    }

    # remove empty rows
    d <- remove_empty_rows(d)

    # add empty row at the end, as separator
    d[nrow(d) + 1, ] <- rep("", ncol(d))

    # add row ID
    d$.row_id <- id
    d
  })

  out <- do.call(rbind, out)

  # rename
  pattern <- c("variable_label", "values", "value_labels", "frq", "proportions")
  replacement <- c("Label", "Values", "Value Labels", "N", "Prop")
  for (i in seq_along(pattern)) {
    names(out) <- replace(names(out), names(out) == pattern[i], replacement[i])
  }

  # remove all empty columns
  out <- remove_empty_columns(out)

  # reorder
  column_order <- c(
    "ID", "Name", "Label", "Type", "Missings", "Values",
    "Value Labels", "N", "Prop", ".row_id"
  )
  out <- out[union(intersect(column_order, names(out)), names(out))]

  attr(out, "data_name") <- data_name
  attr(out, "n_rows") <- nrow(data)
  attr(out, "n_cols") <- ncol(data)
  attr(out, "n_shown") <- length(select)
  class(out) <- c("data_codebook", "data.frame")

  out
}


# methods ----------------------


#' @export
format.data_codebook <- function(x, format = "text", ...) {
  # use [["N"]] to avoid partial matching
  if (any(stats::na.omit(nchar(x[["N"]]) > 5))) {
    x[["N"]] <- insight::trim_ws(prettyNum(x[["N"]], big.mark = ","))
    x[["N"]][x[["N"]] == "NA" | is.na(x[["N"]])] <- ""
  }
  # merge N and %
  if (!is.null(x$Prop)) {
    x$Prop[x$Prop == "NA" | is.na(x$Prop)] <- ""
    # align only for text format
    if (identical(format, "text")) {
      x$Prop[x$Prop != ""] <- format(x$Prop[x$Prop != ""], justify = "right")
    }
    x[["N"]][x$Prop != ""] <- sprintf(
      "%s (%s)",
      as.character(x[["N"]][x$Prop != ""]),
      x$Prop[x$Prop != ""]
    )
    x$Prop <- NULL
  }
  x
}


#' @export
print.data_codebook <- function(x, ...) {
  caption <- c(.get_codebook_caption(x), "blue")
  x$.row_id <- NULL
  cat(
    insight::export_table(format(x),
      title = caption,
      empty_line = "-",
      cross = "+",
      align = .get_codebook_align(x)
    )
  )
}


#' @rdname data_codebook
#' @export
print_html.data_codebook <- function(x,
                                     font_size = "100%",
                                     line_padding = 3,
                                     row_color = "#eeeeee",
                                     ...) {
  insight::check_if_installed("gt")
  caption <- .get_codebook_caption(x)
  attr(x, "table_caption") <- caption
  # since we have each value at its own row, the HTML table contains
  # horizontal borders for each cell/row. We want to remove those borders
  # from rows that actually belong to one variable
  separator_lines <- which(duplicated(x$.row_id) & x$N == "")
  # remove separator lines, as we don't need these for HTML tables
  x <- x[-separator_lines, ]
  # check row IDs, and find odd rows
  odd_rows <- (x$.row_id %% 2 == 1)
  x$.row_id <- NULL
  # create basic table
  out <- insight::export_table(
    format(x, format = "html"),
    title = caption,
    format = "html",
    align = .get_codebook_align(x)
  )
  # no border for rows which are not separator lines
  out <- gt::tab_style(
    out,
    style = list(gt::cell_borders(sides = "top", style = "hidden")),
    locations = gt::cells_body(rows = which(x$ID == ""))
  )
  # highlight odd rows
  if (!is.null(row_color)) {
    out <- gt::tab_style(
      out,
      style = list(gt::cell_fill(color = row_color)),
      locations = gt::cells_body(rows = odd_rows)
    )
  }
  # set up additonal HTML options
  gt::tab_options(out,
    table.font.size = font_size,
    data_row.padding = gt::px(line_padding)
  )
}


#' @export
print_md.data_codebook <- function(x, ...) {
  caption <- .get_codebook_caption(x)
  x$.row_id <- NULL
  attr(x, "table_caption") <- caption
  insight::export_table(format(x, format = "markdown"),
    title = caption,
    align = .get_codebook_align(x),
    format = "markdown"
  )
}


# helper ---------

.get_codebook_caption <- function(x) {
  n_rows <- as.character(attributes(x)$n_rows)
  if (nchar(n_rows) > 5) {
    n_rows <- prettyNum(n_rows, big.mark = ",")
  }
  sprintf(
    "%s (%s rows and %i variables, %i shown)",
    attributes(x)$data_name,
    n_rows,
    attributes(x)$n_cols,
    attributes(x)$n_shown
  )
}

.get_codebook_align <- function(x) {
  # need to remove this one
  x$Prop <- NULL
  align <- c(
    "ID" = "l", "Name" = "l", "Label" = "l", "Type" = "l", "Missings" = "r",
    "Values" = "r", "Value Labels" = "l", "N" = "r"
  )
  align <- align[colnames(x)]
  paste0(unname(align), collapse = "")
}
