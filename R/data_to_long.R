#' Reshape (pivot) data from wide to long
#'
#' This function "lengthens" data, increasing the number of rows and decreasing
#' the number of columns. This is a dependency-free base-R equivalent of
#' `tidyr::pivot_longer()`.
#'
#' @param data A data frame to pivot.
#' @param names_to The name of the new column that will contain the column
#'   names.
#' @param names_prefix A regular expression used to remove matching text from
#' the start of each variable name.
#' @param names_sep,names_pattern If `names_to` contains multiple values, this
#' argument controls how the column name is broken up.
#' `names_pattern` takes a regular expression containing matching groups, i.e. "()".
#' @param values_to The name of the new column that will contain the values of
#'   the pivoted variables.
#' @param values_drop_na If `TRUE`, will drop rows that contain only `NA` in the
#'   `values_to` column. This effectively converts explicit missing values to
#'   implicit missing values, and should generally be used only when missing values
#'   in data were created by its structure.
#' @param rows_to The name of the column that will contain the row names or row
#'   numbers from the original data. If `NULL`, will be removed.
#' @param ... Currently not used.
#' @inheritParams find_columns
#' @param cols Identical to `select`. This argument is here to ensure compatibility
#'   with `tidyr::pivot_longer()`. If both `select` and `cols` are provided, `cols`
#'   is used.
#' @param colnames_to Deprecated. Use `names_to` instead.
#'
#' @return If a tibble was provided as input, `reshape_longer()` also returns a
#' tibble. Otherwise, it returns a data frame.
#'
#' @examplesIf requireNamespace("psych") && requireNamespace("tidyr")
#' wide_data <- data.frame(replicate(5, rnorm(10)))
#'
#' # Default behaviour (equivalent to tidyr::pivot_longer(wide_data, cols = 1:5))
#' data_to_long(wide_data)
#'
#' # Customizing the names
#' data_to_long(wide_data,
#'   select = c(1, 2),
#'   names_to = "Column",
#'   values_to = "Numbers",
#'   rows_to = "Row"
#' )
#'
#' # Full example
#' # ------------------
#' data <- psych::bfi # Wide format with one row per participant's personality test
#'
#' # Pivot long format
#' data_to_long(data,
#'   select = regex("\\d"), # Select all columns that contain a digit
#'   names_to = "Item",
#'   values_to = "Score",
#'   rows_to = "Participant"
#' )
#'
#' data_to_long(
#'   tidyr::who,
#'   select = new_sp_m014:newrel_f65,
#'   names_to = c("diagnosis", "gender", "age"),
#'   names_pattern = "new_?(.*)_(.)(.*)",
#'   values_to = "count"
#' )
#'
#' @inherit data_rename
#' @export
data_to_long <- function(data,
                         select = "all",
                         names_to = "name",
                         names_prefix = NULL,
                         names_sep = NULL,
                         names_pattern = NULL,
                         values_to = "value",
                         values_drop_na = FALSE,
                         rows_to = NULL,
                         ignore_case = FALSE,
                         regex = FALSE,
                         ...,
                         cols,
                         colnames_to) {
  # Check args
  if (!missing(colnames_to)) {
    .is_deprecated("colnames_to", "names_to")
    if (is.null(names_to)) {
      names_to <- colnames_to
    }
  }

  # Prefer "cols" over "select" for compat with tidyr::pivot_longer
  if (!missing(cols)) {
    select <- substitute(cols)
    cols <- .select_nse(
      select,
      data,
      exclude = NULL,
      ignore_case = ignore_case,
      regex = regex,
      verbose = FALSE
    )
  } else {
    if (!missing(select) || !is.null(select)) {
      cols <- .select_nse(
        select,
        data,
        exclude = NULL,
        ignore_case = ignore_case,
        regex = regex,
        verbose = FALSE
      )
    } else {
      insight::format_error(
        "You need to specify columns to pivot, either with `select` or `cols`."
      )
    }
  }

  # nothing to select?
  if (length(cols) == 0L) {
    insight::format_error("No columns found for reshaping data.")
  }

  if (length(names_to) > 1L && is.null(names_sep) && is.null(names_pattern)) {
    insight::format_error(
      "If you supply multiple names in `names_to`, you must also supply one of `names_sep` or `names_pattern`."
    )
  }

  if (length(names_to) == 1L) {
    if (!is.null(names_sep)) {
      insight::format_error(
        "You can't use `names_sep` when `names_to` is of length 1."
      )
    }
    if (!is.null(names_pattern)) {
      insight::format_error(
        "You can't use `names_pattern` when `names_to` is of length 1."
      )
    }
  }

  # save custom attributes
  custom_attr <- attributes(data)

  # Remove tidyverse attributes, will add them back at the end
  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  } else {
    tbl_input <- FALSE
  }

  if (any(names_to %in% setdiff(names(data), cols))) {
    insight::format_error(
      "Some values of the columns specified in `names_to` are already present as column names.",
      paste0(
        "Either use another value in `names_to` or rename the following columns: ",
        text_concatenate(names_to[which(names_to %in% setdiff(names(data), cols))])
      )
    )
  }

  not_selected <- setdiff(names(data), cols)

  # create a temp id so that we know how to rearrange the rows once the data is
  # stacked
  not_stacked <- data[, not_selected, drop = FALSE]
  not_stacked[["_Rows"]] <- coerce_to_numeric(row.names(data))

  # stack the selected columns
  stacked_data <- .stack(data[, cols, drop = FALSE])[, 2:1]

  # reorder the rows to have a repeated sequence when all vars are selected to
  # pivot
  #
  # See with following example:
  # wide_data <- data.frame(replicate(5, rnorm(10)))
  # data_to_long(wide_data)

  needs_to_rearrange <- length(not_selected) == 0L && is.null(rows_to)
  if (isTRUE(needs_to_rearrange)) {
    # https://stackoverflow.com/questions/73984957/efficient-way-to-reorder-rows-to-have-a-repeated-sequence
    stacked_data <- stacked_data[
      matrix(
        seq_len(nrow(stacked_data)),
        nrow = length(unique(stacked_data$ind)),
        byrow = TRUE
      ),
    ]

    row.names(stacked_data) <- NULL
  }

  stacked_data <- data_rename(stacked_data, "values", values_to)

  # split columns if several names in names_to or names_pattern is specified
  if (length(names_to) > 1L) {
    if (is.null(names_pattern)) {
      # faster than strsplit
      tmp <- utils::read.csv(
        text = stacked_data$ind,
        sep = names_sep,
        stringsAsFactors = FALSE,
        header = FALSE
      )
      names(tmp) <- paste0("V", seq_len(ncol(tmp)))
      tmp[tmp == ""] <- NA

      stacked_data$ind <- NULL
      stacked_data <- cbind(tmp, stacked_data)
    } else {
      tmp <- regmatches(
        unique(stacked_data$ind),
        regexec(names_pattern, unique(stacked_data$ind))
      )
      tmp <- as.data.frame(do.call(rbind, tmp), stringsAsFactors = FALSE)
      names(tmp) <- c("ind", names_to)
      # cbind + match is faster than merge
      # cbind doesn't remove identical columns so we need to manually remove "ind"
      # which is in both datasets
      stacked_data <- cbind(stacked_data, tmp[match(stacked_data[["ind"]], tmp[["ind"]]), -1])
      stacked_data$ind <- NULL
    }
  }

  stacked_data <- data_relocate(stacked_data, select = values_to, after = -1)

  # reunite unselected data with stacked data
  out <- cbind(
    not_stacked, stats::setNames(stacked_data, c(names_to, values_to)),
    row.names = NULL
  )


  if (!is.null(names_prefix)) {
    if (length(names_to) > 1L) {
      insight::format_error(
        "`names_prefix` only works when `names_to` is of length 1."
      )
    }
    out[[names_to]] <- gsub(paste0("^", names_prefix), "", out[[names_to]])
  }

  # rearrange the rows with the temp id
  if (length(not_selected) > 0L) {
    out <- data_arrange(out, "_Rows")
  }

  # Remove or rename the row index
  if (is.null(rows_to)) {
    out[["_Rows"]] <- NULL
  } else {
    out <- data_rename(out, "_Rows", rows_to)
  }

  if (values_drop_na) {
    out <- out[!is.na(out[, values_to]), ]
  }

  # add back attributes
  out <- .replace_attrs(out, custom_attr)

  # add back tidyverse attributes
  if (isTRUE(tbl_input)) {
    class(out) <- c("tbl_df", "tbl", "data.frame")
  }

  # reset row names
  if (!insight::object_has_rownames(data)) {
    row.names(out) <- NULL
  }

  out
}


#' Code adapted from utils::stack (but largely modified)
#'
#' @noRd

.stack <- function(x) {
  ind <- rep(names(x), times = lengths(x))
  # use do.call("c", ...) instead of unlist to preserve the date format (but a
  # bit slower)
  # can't use do.call("c", ...) all the time because its behavior changed with
  # factors in 4.1.0
  values_are_dates <- all(
    vapply(x, .is_date, FUN.VALUE = logical(1L))
  )
  if (values_are_dates) {
    data.frame(values = do.call("c", unname(x)), ind, stringsAsFactors = FALSE)
  } else {
    data.frame(values = unlist(x, use.names = FALSE), ind, stringsAsFactors = FALSE)
  }
}

#' @rdname data_to_long
#' @export
reshape_longer <- data_to_long
