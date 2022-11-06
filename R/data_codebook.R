#' Generate a codebook of a data frame.
#'
#' `data_codebook()` does...
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
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
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    data,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

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
      row.names = NULL
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
      } else {
        values <- stats::na.omit(unique(x))
        frq <- as.vector(table(x))
      }
    }

    # add values, value labels and frequencies to data frame
    d <- cbind(d, data.frame(variable_label, values, value_labels, frq, stringsAsFactors = FALSE))

    # clear duplicates due to recycling
    for (i in c("ID", "Name", "missings", "variable_label")) {
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

  class(out) <- c("data_cookbook", "data.frame")
  out
}


# methods ----------------------


#' @export
print.data_cookbook <- function(x, ...) {
  cat(insight::export_table(x, empty_line = "-", cross = "+"))
}

#' @export
print_html.data_cookbook <- function(x, ...) {
  insight::export_table(x, format = "html")
}

#' @export
print_md.data_cookbook <- function(x, ...) {
  insight::export_table(x, empty_line = "-", format = "markdown")
}
