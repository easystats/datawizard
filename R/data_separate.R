#' @title Separate single variable into multiple variables
#' @name data_separate
#'
#' @description
#' Separates a single variable into multiple new variables.
#'
#' @param data A data frame.
#' @param new_columns The names of the new columns, as character vector.
#' @param separator Separator between columns. Can be a character vector, which
#' is then treated as regular expression, or a numeric vector that indicates at
#' which positions the string values will be split.
#' @param append Logical, if `FALSE` (default), removes original columns that
#' were united. If `TRUE`, all columns are preserved and the new column is
#' appended to the data frame.
#' @param guess_columns ...
#' @param merge_multiple ...
#' @param ... Currently not used.
#' @inheritParams find_columns
#'
#' @return `data`, with a newly created variable.
#'
#' @examples
#' d <- data.frame(
#'   x = c("1.a.6", "2.b.7", "3.c.8"),
#'   stringsAsFactors = FALSE
#' )
#' d
#' data_separate(d, new_columns = c("a", "b", "c")
#' @export
data_separate <- function(data,
                          select = NULL,
                          new_columns = NULL,
                          separator = "[^[:alnum:]]+",
                          guess_columns = "mode",
                          merge_multiple = FALSE,
                          exclude = NULL,
                          append = FALSE,
                          ignore_case = FALSE,
                          verbose = TRUE,
                          regex = FALSE,
                          ...) {
  # in case user did not provide names of new columns, we can try
  # to guess number of columns per variable
  guess_columns <- match.arg(guess_columns, choices = c("min", "max", "mode"))

  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    data,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  if (is.null(select)) {
    insight::format_error("No columnas found to separate. Please check your `select` argument.")
  }

  # do we have known number of columns?
  if (is.null(new_columns)) {
    n_columns <- NULL
    new_column_names <- NULL
  } else {
    n_columns <- length(new_columns)
    new_column_names <- new_columns
  }

  # iterate columns that should be split
  for (sep_column in select) {

    # make sure we have a character that we can split
    x <- data[[sep_column]]
    if (!is.character(x)) {
      x <- as.character(x)
    }

    # separate column into multiple strings
    if (is.numeric(separator)) {
      maxlen <- max(nchar(x))
      starts <- c(0, cumsum(separator) + 1)
      ends <- c(cumsum(separator), maxlen)
      separated_columns <- lapply(seq_along(starts), function(i) {
        substr(x, starts[i], ends[i])
      })
    } else {
      separated_columns <- strsplit(x, separator)
    }

    # how many new columns do we need?
    if (is.null(n_columns)) {
      # lengths of all split strings
      l <- lengths(separated_columns)
      # define number of new columns, based on user-choice
      n_cols <- switch(
        guess_columns,
        "min" = min(l, na.rm = TRUE),
        "max" = max(l, na.rm = TRUE),
        "mode" = distribution_mode(l),
      )
    } else {
      # else, if we know number of columns, use that number
      n_cols <- n_columns
    }

    # bind separated columns into data frame and set column names
    out <- as.data.frame(do.call(rbind, separated_columns))
    colnames(out) <- new_columns
  }

  if (isTRUE(append)) {
    data <- cbind(data, out)
  }

  # fin
  data
}
