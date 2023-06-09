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
#' @param fill ...
#' @param extra ...
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
                          fill = "right",
                          extra = "drop_right",
                          exclude = NULL,
                          append = FALSE,
                          ignore_case = FALSE,
                          verbose = TRUE,
                          regex = FALSE,
                          ...) {
  # in case user did not provide names of new columns, we can try
  # to guess number of columns per variable
  guess_columns <- match.arg(guess_columns, choices = c("min", "max", "mode"))

  # make sure we have valid options for fill and extra
  fill <- match.arg(fill, choices = c("left", "right", "value_left", "value_right"))
  extra <- match.arg(extra, choices = c("drop_left", "drop_right", "merge_left", "merge_right"))

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
  } else {
    n_columns <- length(new_columns)
  }

  # iterate columns that should be split
  split_data <- lapply(select, function(sep_column) {

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
      # but without NA values
      l <- l[!vapply(l, function(i) all(is.na(i)), TRUE)]
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

    # main task here - fill or drop values for all columns
    separated_columns <- .fix_separated_columns(separated_columns, fill, extra, n_cols)

    # bind separated columns into data frame and set column names
    out <- as.data.frame(do.call(rbind, separated_columns))

    # if no column names provided, use standard names
    if (is.null(new_columns)) {
      new_column_names <- paste0("split_", seq_along(out))
    } else {
      new_column_names <- new_columns
    }

    # check if column names should be recycled
    if (ncol(out) != length(new_column_names)) {
      # if column names can't be recycled, error
      if (ncol(out) %% length(new_column_names) != 0) {
        insight::format_error(
          "Number of provided column names does not match number of newly created columns.",
          "Cannot recycle column names."
        )
      }
      # recycle names, avoid duplicates
      new_column_names <- make.unique(rep(new_column_names, times = ncol(out) / new_column_names))
    }

    colnames(out) <- new_column_names
    out
  })

  # final preparation, bind or merge columns, make unique columm names
  if (isTRUE(merge_multiple) && length(split_data) > 1) {
    # we merge all splitted columns, which are currently saved as list
    # of data frames, together into one data frame
    for (i in 2:length(split_data)) {
      for (j in seq_len(split_data[[1]])) {
        split_data[[1]][[j]] <- paste(split_data[[1]][[j]], split_data[[i]][[j]])
      }
    }
    split_data <- split_data[[1]]
  } else {
    # bind all columns
    split_data <- do.call(cbind, split_data)
    colnames(split_data) <- make.unique(colnames(split_data))
  }

  if (isTRUE(append)) {
    data <- cbind(data, split_data)
  } else {
    data <- split_data
  }

  # fin
  data
}


#' @keywords internal
.fix_separated_columns <- function(separated_columns, fill, extra, n_cols) {
  separated_columns <- lapply(separated_columns, function(i) {
    # determine number of values in separated column
    n_values <- length(i)
    if (all(is.na(i))) {
      # we have NA values - so fill everything with NA
      out <- rep(NA_character_, times = n_cols)
    } else if (n_values > n_cols) {
      # we have more values than required - drop extra columns
      if (extra == "drop_left") {
        out <- i[(n_values - n_cols + 1):n_values]
      } else if (extra == "drop_right") {
        out <- i[1:n_cols]
      } else if (extra == "merge_left") {
        out <- paste(i[1:(n_values - n_cols + 1)], collapse = " ")
        out <- c(out, i[n_cols:n_values])
      } else {
        out <- i[1:(n_cols - 1)]
        out <- c(out, paste(i[n_cols:n_values], collapse = " "))
      }
    } else if (n_values < n_cols) {
      # we have fewer values than required - fill columns
      if (fill == "left") {
        out <- c(rep(NA_character_, times = n_cols - n_values), i)
      } else if (fill == "right") {
        out <- c(i, rep(NA_character_, times = n_cols - n_values))
      } else if (fill == "value_left") {
        out <- c(rep(i[1], times = n_cols - n_values), i)
      } else if (fill == "value_right") {
        out <- c(i, rep(i[length(i)], times = n_cols - n_values))
      }
    } else {
      out <- i
    }
    out
  })
}
