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
#' @param values_fill Optionally, a (scalar) value that will be used to replace
#' missing values in the new columns created.
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

  not_unstacked <- data[, id_cols, drop = FALSE]
  not_unstacked <- unique(not_unstacked)

  # unstack doesn't create NAs for combinations that don't exist (contrary to
  # reshape), so we need to complete the dataset before unstacking.

  new_data <- data

  # create an id with all variables that are not in names_from or values_from
  # so that we can create missing combinations between this id and names_from
  if (length(id_cols) > 1L) {
    new_data$temporary_id <- do.call(paste, c(new_data[, id_cols, drop = FALSE], sep = "_"))
  } else if (length(id_cols) == 1L) {
    new_data$temporary_id <- new_data[[id_cols]]
  } else {
    new_data$temporary_id <- seq_len(nrow(new_data))
  }

  # check that all_groups have all possible values for names_from
  # If not, need to complete the dataset with NA for values_from where names_from
  # didn't exist
  n_rows_per_group <- table(new_data$temporary_id)
  n_values_per_group <- insight::n_unique(n_rows_per_group)

  not_all_cols_are_selected <- length(id_cols) > 0L

  incomplete_groups <-
    (n_values_per_group > 1L &&
      !all(unique(n_rows_per_group) %in% insight::n_unique(new_data[, names_from]))
    ) ||
      (n_values_per_group == 1L &&
        unique(n_rows_per_group) < length(unique(new_data[, names_from]))
      )

  # create missing combinations

  if (not_all_cols_are_selected && incomplete_groups) {
    expanded <- expand.grid(unique(new_data[["temporary_id"]]), unique(new_data[[names_from]]))
    names(expanded) <- c("temporary_id", names_from)
    new_data <- data_merge(new_data, expanded,
      join = "full", by = c("temporary_id", names_from),
      sort = FALSE
    )

    # need to make a second temporary id to keep arrange values *without*
    # rearranging the whole dataset
    # Ex:
    # "B"   1
    # "A"   3
    # "A"   NA
    # "B"   NA
    #
    # must be rearranged as "B" "B" "A" "A" and not "A" "A" "B" "B"
    lookup <- data.frame(
      temporary_id = unique(
        new_data[!is.na(new_data[[values_from]]), "temporary_id"]
      )
    )
    lookup$temporary_id_2 <- seq_len(nrow(lookup))
    new_data <- data_merge(
      new_data, lookup,
      by = "temporary_id", join = "left"
    )

    # creation of missing combinations was done with a temporary id, so need
    # to fill columns that are not selected in names_from or values_from
    new_data[, id_cols] <- lapply(id_cols, function(x) {
      data <- data_arrange(new_data, c("temporary_id_2", x))
      ind <- which(!is.na(data[[x]]))
      rep_times <- diff(c(ind, length(data[[x]]) + 1))
      rep(data[[x]][ind], times = rep_times)
    })

    new_data <- data_arrange(new_data, "temporary_id_2")
  }

  # don't need temporary ids anymore
  new_data$temporary_id <- NULL
  new_data$temporary_id_2 <- NULL

  # Fill missing values (before converting to wide)
  if (!is.null(values_fill)) {
    if (length(values_fill) == 1L) {
      if (is.numeric(new_data[[values_from]])) {
        if (is.numeric(values_fill)) {
          new_data <- convert_na_to(new_data, replace_num = values_fill)
        } else {
          insight::format_error(paste0("`values_fill` must be of type numeric."))
        }
      } else if (is.character(new_data[[values_from]])) {
        if (is.character(values_fill)) {
          new_data <- convert_na_to(new_data, replace_char = values_fill)
        } else {
          insight::format_error(paste0("`values_fill` must be of type character."))
        }
      } else if (is.factor(new_data[[values_from]])) {
        if (is.factor(values_fill)) {
          new_data <- convert_na_to(new_data, replace_fac = values_fill)
        } else {
          insight::format_error(paste0("`values_fill` must be of type factor."))
        }
      }
    } else if (verbose) {
      insight::format_error("`values_fill` must be of length 1.")
    }
  }

  # convert to wide format (returns the data and the order in which columns
  # should be ordered)
  unstacked <- .unstack(
    new_data, names_from, values_from,
    names_sep, names_prefix, names_glue
  )

  out <- unstacked$out

  if (length(values_from) > 1L) {
    unstacked$col_order <- unique(data[, names_from])
    unstacked$col_order <- sort(
      as.vector(
        outer(values_from, unstacked$col_order, paste, sep = names_sep)
      )
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
  out <- out[, unstacked$col_order]

  # need to add the wide data to the original data
  if (!insight::is_empty_object(not_unstacked)) {
    out <- cbind(not_unstacked, out)
  }
  row.names(out) <- NULL

  out <- remove_empty_columns(out)

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


#' @rdname data_to_wide
#' @export
reshape_wider <- data_to_wide
