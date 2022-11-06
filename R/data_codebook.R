#' Generate a codebook of a data frame.
#'
#' `data_codebook()` does...
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @param label_width Length of variable labels. Longer labels will be wrapped
#' at `label_width` chars. If `NULL`, longer labels will not be split into
#' multiple lines.
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
                          label_width = NULL,
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

  out <- lapply(seq_along(select), function(id) {

    # variable
    x <- data[[select[id]]]
    x_na <- is.na(x)

    # inital data frame for codebook
    d <- data.frame(
      ID = id,
      Name = select[id],
      missings = sprintf("%g (%.1f%%)", sum(x_na), sum(x_na) / length(x_na)),
      stringsAsFactors = FALSE,
      row.names = NULL,
      check.names = FALSE
    )

    # check if there are variable labels
    if (!is.null(attr(x, "label", exact = TRUE))) {
      variable_label <- attr(x, "label", exact = TRUE)
      if (!is.null(label_width) && nchar(variable_label) > label_width) {
        variable_label <- insight::trim_ws(unlist(strsplit(
          text_wrap(variable_label, width = label_width),
          "\n",
          fixed = TRUE
        )))
      }
    } else {
      variable_label <- NA
    }

    # we may need to remove duplicated value range elements
    flag_range <- FALSE

    # check if there are value labels or factor levels, and extract values and N
    if (!is.null(attr(x, "labels", exact = TRUE))) {
      value_labels <- names(attr(x, "labels", exact = TRUE))
      values <- attr(x, "labels", exact = TRUE)
      frq <- as.vector(table(x))
    } else if (is.factor(x)) {
      values <- levels(x)
      value_labels <- NA
      frq <- as.vector(table(x))
    } else {
      value_labels <- NA
      if (is.numeric(x)) {
        r <- range(x, na.rm = TRUE)
        values <- sprintf("[%g, %g]", r[1], r[2])
        frq <- sum(!x_na)
        flag_range <- length(variable_label) > 1
      } else {
        values <- stats::na.omit(unique(x))
        frq <- as.vector(table(x))
      }
    }

    # add values, value labels and frequencies to data frame
    d <- cbind(d, data.frame(variable_label, values, value_labels, frq, stringsAsFactors = FALSE))

    # which columns need to be checked for duplicates?
    duplicates <- c("ID", "Name", "missings", "variable_label")
    if (isTRUE(flag_range)) {
      # when we have numeric variables with value range as values, and when
      # these variables had long variable labels that have been wrapped,
      # the range value is duplicated, so we need to fix this here.
      duplicates <- c(duplicates, c("values", "frq"))
    }

    # clear duplicates due to recycling
    for (i in duplicates) {
      d[[i]][duplicated(d[[i]])] <- ""
    }

    # add empty row
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
