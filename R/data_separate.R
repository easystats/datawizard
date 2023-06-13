#' @title Separate single variable into multiple variables
#' @name data_separate
#'
#' @description
#' Separates a single variable into multiple new variables.
#'
#' @param data A data frame.
#' @param new_columns The names of the new columns, as character vector. If
#' more than one variable was selected (in `select`), the new names are prefixed
#' with the name of the original column. `new_columns` can also be a list of
#' (named) character vectors when multiple variables should be separated. See
#' 'Examples'.
#' @param separator Separator between columns. Can be a character vector, which
#' is then treated as regular expression, or a numeric vector that indicates at
#' which positions the string values will be split.
#' @param append Logical, if `FALSE` (default), removes original columns that
#' were separated. If `TRUE`, all columns are preserved and the new columns are
#' appended to the data frame.
#' @param guess_columns If `new_columns` is not given, the required number of
#' new columns is guessed based on the results of value splitting. For example,
#' if a variable is split into three new columns, this will be considered as
#' the required number of new columns, and columns are named `"split_1"`,
#' `"split_2"` and `"split_3"`. When values from a variable are split into
#' different amount of new columns, the `guess_column` can be either `"mode"`
#' (number of new columns is based on the most common number of splits), `"min"`
#' or `"max"` to use the minimum resp. maximum number of possible splits as
#' required number of columns.
#' @param fill How to deal with values that return fewer new columns after
#' splitting? Can be `"left"` (fill missing columns from the left with `NA`),
#' `"right"` (fill missing columns from the right with `NA`) or `"value_left"`
#' or `"value_right"` to fill missing columns from left or right with the
#' left-most or right-most values.
#' @param extra How to deal with values that return too many new columns after
#' splitting? Can be `"drop_left"` or `"drop_right"` to drop the left-most or
#' right-most values, or `"merge_left"` or `"merge_right"` to merge the left-
#' or right-most value together, and keeping all remaining values as is.
#' @param merge_multiple Logical, if `TRUE` and more than one variable is selected
#' for separating, new columns can be merged. Value pairs of all split variables
#' are merged.
#' @param merge_separator Separator string when `merge_multiple = TRUE`. Defines
#' the string that is used to merge values together.
#' @param convert_na Logical, if `TRUE`, character `"NA"` values are converted
#' into real `NA` values.
#' @param ... Currently not used.
#' @inheritParams find_columns
#'
#' @seealso [`data_unite()`]
#'
#' @return A data frame with the newly created variable(s), or - when `append = TRUE` -
#' `data` including new variables.
#'
#' @examples
#' # simple case
#' d <- data.frame(
#'   x = c("1.a.6", "2.b.7", "3.c.8"),
#'   stringsAsFactors = FALSE
#' )
#' d
#' data_separate(d, new_columns = c("a", "b", "c"))
#'
#' # guess number of columns
#' d <- data.frame(
#'   x = c("1.a.6", NA, "2.b.6.7", "3.c", "x.y.z"),
#'   stringsAsFactors = FALSE
#' )
#' d
#' data_separate(d, guess_columns = "mode")
#'
#' data_separate(d, guess_columns = "max")
#'
#' # drop left-most column
#' data_separate(d, guess_columns = "mode", extra = "drop_left")
#'
#' # merge right-most column
#' data_separate(d, guess_columns = "mode", extra = "merge_right")
#'
#' # fill columns with fewer values with left-most values
#' data_separate(d, guess_columns = "mode", fill = "value_left")
#'
#' # fill and merge
#' data_separate(
#'   d,
#'   guess_columns = "mode",
#'   fill = "value_left",
#'   extra = "merge_right"
#' )
#'
#' # multiple columns to split
#' d <- data.frame(
#'   x = c("1.a.6", "2.b.7", "3.c.8"),
#'   y = c("x.y.z", "10.11.12", "m.n.o"),
#'   stringsAsFactors = FALSE
#' )
#' d
#' # split two columns, default column names
#' data_separate(d, guess_columns = "mode")
#'
#' # split into new named columns, repeating column names
#' data_separate(d, new_columns = c("a", "b", "c"))
#'
#' # split selected variable new columns
#' data_separate(d, select = "y", new_columns = c("a", "b", "c"))
#'
#' # merge multiple split columns
#' data_separate(
#'   d,
#'   new_columns = c("a", "b", "c"),
#'   merge_multiple = TRUE
#' )
#'
#' # merge multiple split columns
#' data_separate(
#'   d,
#'   new_columns = c("a", "b", "c"),
#'   merge_multiple = TRUE,
#'   merge_separator = "-"
#' )
#'
#' # separate multiple columns, give proper column names
#' d_sep <- data.frame(
#'   x = c("1.a.6", "2.b.7.d", "3.c.8", "5.j"),
#'   y = c("m.n.99.22", "77.f.g.34", "44.9", NA),
#'   stringsAsFactors = FALSE
#' )
#'
#' data_separate(
#'   d_sep,
#'   select = c("x", "y"),
#'   new_columns = list(
#'     x = c("A", "B", "C"), # separate "x" into three columns
#'     y = c("EE", "FF", "GG", "HH") # separate "y" into four columns
#'   ),
#'   verbose = FALSE
#' )
#' @export
data_separate <- function(data,
                          select = NULL,
                          new_columns = NULL,
                          separator = "[^[:alnum:]]+",
                          guess_columns = NULL,
                          merge_multiple = FALSE,
                          merge_separator = "",
                          fill = "right",
                          extra = "drop_right",
                          convert_na = TRUE,
                          exclude = NULL,
                          append = FALSE,
                          ignore_case = FALSE,
                          verbose = TRUE,
                          regex = FALSE,
                          ...) {
  # we need at least one explicit choice for either `new_columns` or `guess_columns`
  if (is.null(new_columns) && is.null(guess_columns)) {
    insight::format_error("Cannot separate values. Either `new_columns` or `guess_columns` must be provided.")
  }
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

  # make new_columns as list, this works with single and multiple columns
  if (!is.null(new_columns) && !is.list(new_columns)) {
    new_columns <- rep(list(new_columns), times = length(select))
    # if we have multiple columns that were separated, we avoid duplicated
    # column names of created variables by appending name of original column
    # however, we don't have duplicated column names when we merge them together
    # so don't create new column names when "merge_multiple" is FALSE.
    make_unique_colnames <- length(select) > 1 && !merge_multiple
  } else {
    # we don't want to create own unique column names when user explicitly
    # provided column names as a list, i.e. column names for each separated
    # variable
    make_unique_colnames <- FALSE
  }

  # make sure list of new column names is named
  if (!is.null(new_columns) && is.null(names(new_columns))) {
    names(new_columns) <- select
  }

  # iterate columns that should be split
  split_data <- lapply(select, function(sep_column) {
    # do we have known number of columns?
    if (is.null(new_columns)) {
      n_columns <- NULL
    } else {
      n_columns <- length(new_columns[[sep_column]])
    }

    # make sure we have a character that we can split
    x <- data[[sep_column]]
    if (!is.character(x)) {
      x <- as.character(x)
    }

    # separate column into multiple strings
    if (is.numeric(separator)) {
      maxlen <- max(nchar(x), na.rm = TRUE)
      starts <- c(0, separator)
      ends <- c(separator - 1, maxlen)
      separated_columns <- lapply(seq_along(starts), function(i) {
        substr(x, starts[i], ends[i])
      })
      separated_columns <- as.data.frame(
        do.call(rbind, separated_columns),
        stringsAsFactors = FALSE
      )
    } else {
      separated_columns <- strsplit(x, separator, perl = TRUE)
    }

    # how many new columns do we need?
    if (is.null(n_columns)) {
      # lengths of all split strings
      l <- lengths(separated_columns)
      # but without NA values
      l <- l[!vapply(l, function(i) all(is.na(i)), TRUE)]
      # define number of new columns, based on user-choice
      n_cols <- switch(guess_columns,
        "min" = min(l, na.rm = TRUE),
        "max" = max(l, na.rm = TRUE),
        "mode" = distribution_mode(l),
      )
      # tell user
      if (verbose && insight::n_unique(l) != 1 && !is.numeric(separator)) {
        insight::format_alert(paste0(
          "Column `", sep_column, "` had different number of values after splitting. Variable was split into ",
          n_cols, " column", ifelse(n_cols > 1, "s", ""), "."
        ))
      }
    } else {
      # else, if we know number of columns, use that number
      n_cols <- n_columns
    }

    # main task here - fill or drop values for all columns
    separated_columns <- tryCatch(
      .fix_separated_columns(separated_columns, fill, extra, n_cols, sep_column, verbose),
      error = function(e) NULL
    )

    # catch error
    if (is.null(separated_columns)) {
      insight::format_error(
        "Something went wrong. Probably the number of provided column names did not match number of newly created columns?"
      )
    }

    # bind separated columns into data frame and set column names
    out <- as.data.frame(do.call(rbind, separated_columns))

    # if no column names provided, use standard names
    if (is.null(new_columns[[sep_column]])) {
      new_column_names <- paste0(sep_column, "_", seq_along(out))
    } else {
      # if we have multiple columns that were separated, we avoid duplicated
      # column names of created variables by appending name of original column
      if (make_unique_colnames) {
        new_column_names <- paste0(sep_column, "_", new_columns[[sep_column]])
      } else {
        new_column_names <- new_columns[[sep_column]]
      }
    }

    colnames(out) <- new_column_names
    out
  })

  # any split performed?
  if (all(lengths(split_data) == 1)) {
    if (verbose) {
      insight::format_alert("Separator probably not found. No values were split. Returning original data.")
    }
    return(data)
  }

  # final preparation, bind or merge columns, make unique columm names
  if (isTRUE(merge_multiple) && length(split_data) > 1) {
    # we merge all split columns, which are currently saved as list
    # of data frames, together into one data frame
    for (i in 2:length(split_data)) {
      for (j in seq_along(split_data[[1]])) {
        split_data[[1]][[j]] <- gsub(" ", "",
          paste(
            split_data[[1]][[j]],
            split_data[[i]][[j]],
            sep = merge_separator
          ),
          fixed = TRUE
        )
      }
    }
    split_data <- split_data[[1]]
  } else {
    # bind all columns
    split_data <- do.call(cbind, split_data)
  }

  # convert "NA" strings into real NA?
  if (convert_na) {
    split_data[] <- lapply(split_data, function(i) {
      i[i == "NA"] <- NA_character_
      i
    })
  }

  data <- cbind(data, split_data)
  if (!isTRUE(append)) {
    data[select] <- NULL
  }

  # fin
  data
}


#' @keywords internal
.fix_separated_columns <- function(separated_columns, fill, extra, n_cols, sep_column, verbose = TRUE) {
  warn_extra <- warn_fill <- FALSE
  for (sc in seq_along(separated_columns)) {
    i <- separated_columns[[sc]]
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
        out <- c(out, i[(n_values - n_cols + 2):n_values])
      } else {
        out <- i[1:(n_cols - 1)]
        out <- c(out, paste(i[n_cols:n_values], collapse = " "))
      }
      warn_extra <- TRUE
    } else if (n_values < n_cols) {
      # we have fewer values than required - fill columns
      if (fill == "left") {
        out <- c(rep(NA_character_, times = n_cols - n_values), i)
      } else if (fill == "right") {
        out <- c(i, rep(NA_character_, times = n_cols - n_values))
      } else if (fill == "value_left") {
        out <- c(rep(i[1], times = n_cols - n_values), i)
      } else {
        out <- c(i, rep(i[length(i)], times = n_cols - n_values))
      }
      warn_fill <- TRUE
    } else {
      out <- i
    }
    separated_columns[[sc]] <- out
  }

  if (verbose) {
    if (warn_extra) {
      insight::format_alert(paste0(
        "`", sep_column, "`",
        " returned more columns than expected after splitting. ",
        switch(extra,
          "drop_left" = "Left-most columns have been dropped.",
          "drop_right" = "Right-most columns have been dropped.",
          "merge_left" = "Left-most columns have been merged together.",
          "merge_right" = "Right-most columns have been merged together."
        )
      ))
    }
    if (warn_fill) {
      insight::format_alert(paste0(
        "`", sep_column, "`",
        "returned fewer columns than expected after splitting. ",
        switch(fill,
          "left" = "Left-most columns were filled with `NA`.",
          "right" = "Right-most columns were filled with `NA`.",
          "value_left" = "Left-most columns were filled with first value.",
          "value_right" = "Right-most columns were filled with last value."
        )
      ))
    }
  }

  separated_columns
}
