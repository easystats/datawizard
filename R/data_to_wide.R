#' Reshape (pivot) data from long to wide
#'
#' This function "widens" data, increasing the number of columns and decreasing
#' the number of rows. This is a dependency-free base-R equivalent of
#' `tidyr::pivot_wider()`.
#'
#' @param data A data frame to convert to wide format, so that it has more
#' columns and fewer rows post-widening than pre-widening.
#' @param id_cols The name of the column that identifies the rows in the data
#' by which observations are grouped and the gathered data is spread into new
#' columns. Usually, this is a variable containing an ID for observations that
#' have been repeatedly measured. If `NULL`, it will use all remaining columns
#' that are not in `names_from` or `values_from` as ID columns. `id_cols` can
#' also be a character vector with more than one name of identifier columns. See
#' also 'Details' and 'Examples'.
#' @param names_from The name of the column in the original data whose values
#' will be used for naming the new columns created in the widened data. Each
#' unique value in this column will become the name of one of these new columns.
#' In case `names_prefix` is provided, column names will be concatenated with
#' the string given in `names_prefix`.
#' @param names_prefix String added to the start of every variable name. This is
#'  particularly useful if `names_from` is a numeric vector and you want to create
#'  syntactic variable names.
#' @param names_sep If `names_from` or `values_from` contains multiple variables,
#' this will be used to join their values together into a single string to use
#' as a column name.
#' @param names_glue Instead of `names_sep` and `names_prefix`, you can supply a
#' [glue specification](https://glue.tidyverse.org/index.html) that uses the
#' `names_from` columns to create custom column names. Note that the only
#' delimiters supported by `names_glue` are curly brackets, `{` and `}`.
#' @param values_from The name of the columns in the original data that contains
#' the values used to fill the new columns created in the widened data.
#' @param values_fill Optionally, a (scalar) value, or a named list of (scalar)
#' values, that will be used to create additional rows for missing combinations
#' of `id_cols` and `names_from`, and then fills in the new columns with values
#' from `values_from`. If a named list is provided, the names must correspond to
#' the columns to be filled. Note that `values_fill` only applies to missing
#' combinations of `id_cols` and `names_from`, i.e. if a combination exists but
#' the value in `values_from` is `NA`, this will remain `NA`.
#' @param verbose Toggle warnings.
#' @param ... Not used for now.
#'
#' @return If a tibble was provided as input, `data_to_wide()` also returns a
#' tibble. Otherwise, it returns a data frame.
#'
#' @details
#' Reshaping data into wide format usually means that the input data frame is
#' in _long_ format, where multiple measurements taken on the same subject are
#' stored in multiple rows. The wide format stores the same information in a
#' single row, with each measurement stored in a separate column. Thus, the
#' necessary information for `data_to_wide()` is:
#'
#' - The name of the column(s) that identify the groups or repeated measurements
#'   (`id_cols`).
#' - The name of the column whose _values_ will become the new column names
#'   (`names_from`). Since these values may not necessarily reflect appropriate
#'   column names, you can use `names_prefix` to add a prefix to each newly
#'   created column name.
#' - The name of the column that contains the values (`values_from`) for the
#'   new columns that are created by `names_from`.
#'
#' In other words: repeated measurements, as indicated by `id_cols`, that are
#' saved into the column `values_from` will be spread into new columns, which
#' will be named after the values in `names_from`. See also 'Examples'.
#'
#' @examplesIf requireNamespace("lme4", quietly = TRUE)
#' data_long <- read.table(header = TRUE, text = "
#'  subject sex condition measurement
#'        1   M   control         7.9
#'        1   M     cond1        12.3
#'        1   M     cond2        10.7
#'        2   F   control         6.3
#'        2   F     cond1        10.6
#'        2   F     cond2        11.1
#'        3   F   control         9.5
#'        3   F     cond1        13.1
#'        3   F     cond2        13.8
#'        4   M   control        11.5
#'        4   M     cond1        13.4
#'        4   M     cond2        12.9")
#'
#' # converting long data into wide format
#' data_to_wide(
#'   data_long,
#'   id_cols = "subject",
#'   names_from = "condition",
#'   values_from = "measurement"
#' )
#'
#' # converting long data into wide format with custom column names
#' data_to_wide(
#'   data_long,
#'   id_cols = "subject",
#'   names_from = "condition",
#'   values_from = "measurement",
#'   names_prefix = "Var.",
#'   names_sep = "."
#' )
#'
#' # converting long data into wide format, combining multiple columns
#' production <- expand.grid(
#'   product = c("A", "B"),
#'   country = c("AI", "EI"),
#'   year = 2000:2014
#' )
#' production <- data_filter(production, (product == "A" & country == "AI") | product == "B")
#' production$production <- rnorm(nrow(production))
#'
#' data_to_wide(
#'   production,
#'   names_from = c("product", "country"),
#'   values_from = "production",
#'   names_glue = "prod_{product}_{country}"
#' )
#'
#' # using the "sleepstudy" dataset
#' data(sleepstudy, package = "lme4")
#'
#' # the sleepstudy data contains repeated measurements of average reaction
#' # times for each subjects over multiple days, in a sleep deprivation study.
#' # It is in long-format, i.e. each row corresponds to a single measurement.
#' # The variable "Days" contains the timepoint of the measurement, and
#' # "Reaction" contains the measurement itself. Converting this data to wide
#' # format will create a new column for each day, with the reaction time as the
#' # value.
#' head(sleepstudy)
#'
#' data_to_wide(
#'   sleepstudy,
#'   id_cols = "Subject",
#'   names_from = "Days",
#'   values_from = "Reaction"
#' )
#'
#' # clearer column names
#' data_to_wide(
#'   sleepstudy,
#'   id_cols = "Subject",
#'   names_from = "Days",
#'   values_from = "Reaction",
#'   names_prefix = "Reaction_Day_"
#' )
#'
#' # For unequal group sizes, missing information is filled with NA
#' d <- subset(sleepstudy, Days %in% c(0, 1, 2, 3, 4))[c(1:9, 11:13, 16:17, 21), ]
#'
#' # long format, different number of "Subjects"
#' d
#'
#' data_to_wide(
#'   d,
#'   id_cols = "Subject",
#'   names_from = "Days",
#'   values_from = "Reaction",
#'   names_prefix = "Reaction_Day_"
#' )
#'
#' # filling missing values with 0
#' data_to_wide(
#'   d,
#'   id_cols = "Subject",
#'   names_from = "Days",
#'   values_from = "Reaction",
#'   names_prefix = "Reaction_Day_",
#'   values_fill = 0
#' )
#' @inherit data_rename seealso
#' @export
data_to_wide <- function(data,
                         id_cols = NULL,
                         values_from = "Value",
                         names_from = "Name",
                         names_sep = "_",
                         names_prefix = "",
                         names_glue = NULL,
                         values_fill = NULL,
                         verbose = TRUE,
                         ...) {
  if (is.null(id_cols)) {
    id_cols <- setdiff(names(data), c(names_from, values_from))
  }

  # save custom attributes
  custom_attr <- attributes(data)

  current_colnames <- names(data)

  # Preserve attributes
  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  } else {
    tbl_input <- FALSE
  }

  variable_attr <- lapply(data, attributes)

  # make sure we have all combinations of id_cols and names_from, so we can
  # properly widen (unstack) the data.
  data <- .fill_missings(
    x = data,
    id_cols = id_cols,
    names_from = names_from,
    values_from = values_from,
    values_fill = values_fill,
    verbose = verbose
  )

  not_unstacked <- unique(data[, id_cols, drop = FALSE])

  # convert to wide format (returns the data and the order in which columns
  # should be ordered)
  unstacked <- .unstack(
    data, names_from, values_from,
    names_sep, names_prefix, names_glue
  )

  out <- unstacked$out

  if (length(values_from) > 1L) {
    unstacked$col_order <- unique(data[, names_from])
    unstacked$col_order <- as.vector(
      t(outer(values_from, unstacked$col_order, paste, sep = names_sep))
    )
  }

  # stop if some column names would be duplicated (follow tidyr workflow)
  if (any(unstacked$col_order %in% current_colnames)) {
    insight::format_error(
      "Some values of the columns specified in `names_from` are already present as column names.",
      paste0(
        "Either use `names_prefix` or rename the following columns: ",
        text_concatenate(current_colnames[which(current_colnames %in% unstacked$col_order)])
      )
    )
  }

  # reorder columns
  out <- out[, unstacked$col_order, drop = FALSE]

  # need to add the wide data to the original data
  if (!insight::is_empty_object(not_unstacked)) {
    out <- cbind(not_unstacked, out)
  }
  row.names(out) <- NULL

  # add back attributes where possible
  for (i in colnames(out)) {
    attributes(out[[i]]) <- variable_attr[[i]]
  }

  # convert back to date if original values were dates
  values_are_dates <- all(
    vapply(data[, values_from, drop = FALSE], .is_date, FUN.VALUE = logical(1L))
  )
  if (values_are_dates) {
    for (i in unstacked$col_order) {
      out[[i]] <- as.Date.numeric(out[[i]], origin = "1970-01-01")
    }
  }

  # add back attributes
  out <- .replace_attrs(out, custom_attr)

  if (isTRUE(tbl_input)) {
    class(out) <- c("tbl_df", "tbl", "data.frame")
  }

  out
}


#' Adapted from `utils::unstack` (but largely modified)
#'
#' @noRd

.unstack <- function(x, names_from, values_from, names_sep, names_prefix, names_glue = NULL) {
  # get values from names_from (future colnames)

  if (is.null(names_glue)) {
    x$future_colnames <- do.call(paste, c(x[, names_from, drop = FALSE], sep = names_sep))
  } else {
    vars <- regmatches(names_glue, gregexpr("\\{\\K[^{}]+(?=\\})", names_glue, perl = TRUE))[[1]]
    tmp_data <- x[, vars]
    x$future_colnames <- .gluestick(names_glue, src = tmp_data)
  }

  x$future_colnames <- paste0(names_prefix, x$future_colnames)

  # expand the values for each variable in "values_from"
  res <- list()
  for (i in seq_along(values_from)) {
    res[[i]] <- tapply(x[[values_from[i]]], x$future_colnames, as.vector)
    if (length(values_from) > 1L) {
      names(res[[i]]) <- paste0(values_from[i], names_sep, names(res[[i]]))
    }
  }

  # if there's a single variable in "values_from" and this variable only has
  # one value, need to make it a dataframe

  if (length(res) == 1L && !is.list(res[[1]])) {
    res <- data.frame(
      matrix(
        res[[1]],
        nrow = 1, dimnames = list(NULL, names(res[[1]]))
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  } else {
    res <- unlist(res, recursive = FALSE)
  }

  # return the wide data and the order in which the new columns should be

  list(
    out = data.frame(res, stringsAsFactors = FALSE, check.names = FALSE),
    col_order = unique(x$future_colnames)
  )
}


# this function finds all combinations of the key columns (i.e. id_cols and
# names_from) that are missing in the data frame that we want to widen
.find_missing_rows <- function(x, id_cols, names_from) {
  # if id_cols contains multiple columns, we need to create a single column
  # with all combinations of unique values. if we leave id_cols as is, we
  # get too many combinations, which would return a wrong result
  if (length(id_cols) > 1L) {
    x[[".datawizard_id"]] <- apply(x[id_cols], 1, paste, collapse = "_")
  } else {
    x[[".datawizard_id"]] <- x[[id_cols]]
  }

  key_cols <- c(".datawizard_id", names_from)

  # create all theoretically possible combinations of the key columns
  df1 <- unique(expand.grid(x[key_cols]))

  # extract all existing combinations of the key columns
  df2 <- x[key_cols]

  # Create a unique identifier for each row by pasting the key columns together.
  df1_keys <- apply(df1, 1, paste, collapse = "_")
  df2_keys <- apply(df2, 1, paste, collapse = "_")

  # Find the rows where the key from df1 is NOT in the keys from df2.
  missing_indices <- !df1_keys %in% df2_keys

  # clean up
  df1[[".datawizard_id"]] <- NULL
  df1 <- cbind(x[id_cols], df1)

  # Subset df1 to get the missing rows.
  df1[missing_indices, ]
}


.fill_missings <- function(
  x,
  id_cols,
  names_from,
  values_from,
  values_fill,
  verbose = TRUE
) {
  # find missing rows, based on id_cols and names_from
  missing_rows <- .find_missing_rows(x, id_cols, names_from)

  # if all combinations exist, nothing to do
  if (nrow(missing_rows) < 1) {
    return(x)
  }

  # we want to add the columns from our original data frame to the data frame
  # that contains the missing rows, so that we can rbind them together
  remaining_columns <- setdiff(colnames(x), colnames(missing_rows))

  # create columns with NA values for the remaining columns, but take correct
  # type into account
  for (i in remaining_columns) {
    missing_rows[[i]] <- switch(
      class(x[[i]])[1],
      factor = factor(NA, levels = levels(x[[i]])),
      character = NA_character_,
      numeric = NA_real_,
      NA
    )
  }

  # we either fill with concrete values, or leave it as NA
  if (!is.null(values_fill)) {
    if (is.list(values_fill)) {
      # we need names for a list
      fill_names <- names(values_fill)
      # values_fill must be a named list, with names corresponding to the
      # columns to be filled
      if (is.null(fill_names) || !all(nzchar(fill_names)) || !all(fill_names %in% remaining_columns)) {
        insight::format_error(
          "`values_fill` must be a named list, with names corresponding to the columns to be filled."
        )
      }
      for (i in fill_names) {
        missing_rows[[i]] <- values_fill[[i]]
      }
    } else if (is.numeric(values_fill) && all(vapply(x[remaining_columns], is.numeric, logical(1)))) {
      missing_rows <- convert_na_to(missing_rows, replace_num = values_fill, verbose = FALSE)
    } else if (is.character(values_fill) && all(vapply(x[remaining_columns], is.character, logical(1)))) {
      missing_rows <- convert_na_to(missing_rows, replace_char = values_fill, verbose = FALSE)
    } else if (is.factor(values_fill) && all(vapply(x[remaining_columns], is.factor, logical(1)))) {
      missing_rows <- convert_na_to(missing_rows, replace_fac = fill, values_fill = FALSE)
    } else {
      insight::format_error(paste0(
        "`values_fill` contains a value of unsupported type, or not all columns that need to be filled are of type ",
        class(fill)[1],
        "."
      ))
    }
  }

  out <- try(rbind(x, missing_rows), silent = TRUE)

  if (inherits(out, "try-error")) {
    insight::format_error(
      "Could not fill missing values. Please ensure that the values in `values_fill` match the types of the columns to be filled."
    )
  }

  data_arrange(out, c(id_cols, names_from))
}


#' @rdname data_to_wide
#' @export
reshape_wider <- data_to_wide
