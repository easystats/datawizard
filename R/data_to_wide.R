#' Reshape (pivot) data from long to wide
#'
#' This function "widens" data, increasing the number of columns and decreasing
#' the number of rows. This is a dependency-free base-R equivalent of
#' `tidyr::pivot_wider()`.
#'
#' @param data A data frame to pivot.
#' @param id_cols The name of the column that identifies the rows. If `NULL`,
#' it will use all the unique rows.
#' @param names_from The name of the column that contains the levels to be
#' used as future column names.
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
#' @param values_from The name of the column that contains the values to be used
#' as future variable values.
#' @param values_fill Optionally, a (scalar) value that will be used to replace
#' missing values in the new columns created.
#' @param verbose Toggle warnings.
#' @param ... Not used for now.
#' @param colnames_from Deprecated. Use `names_from` instead.
#' @param rows_from Deprecated. Use `id_cols` instead.
#' @param sep Deprecated. Use `names_sep` instead.
#'
#' @return If a tibble was provided as input, `reshape_wider()` also returns a
#' tibble. Otherwise, it returns a data frame.
#'
#' @examples
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
#'
#' reshape_wider(
#'   data_long,
#'   id_cols = "subject",
#'   names_from = "condition",
#'   values_from = "measurement"
#' )
#'
#' reshape_wider(
#'   data_long,
#'   id_cols = "subject",
#'   names_from = "condition",
#'   values_from = "measurement",
#'   names_prefix = "Var.",
#'   names_sep = "."
#' )
#'
#' production <- expand.grid(
#'   product = c("A", "B"),
#'   country = c("AI", "EI"),
#'   year = 2000:2014
#' )
#' production <- data_filter(production, (product == "A" & country == "AI") | product == "B")
#'
#' production$production <- rnorm(nrow(production))
#'
#' reshape_wider(
#'   production,
#'   names_from = c("product", "country"),
#'   values_from = "production",
#'   names_glue = "prod_{product}_{country}"
#' )
#'
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
                         ...,
                         colnames_from,
                         rows_from,
                         sep) {
  if (!missing(colnames_from)) {
    .is_deprecated("colnames_from", "names_from")
    if (is.null(names_from)) {
      names_from <- colnames_from
    }
  }
  if (!missing(rows_from)) {
    .is_deprecated("rows_from", "id_cols")
    if (is.null(id_cols)) {
      id_cols <- rows_from
    }
  }
  if (!missing(sep)) {
    .is_deprecated("sep", "names_sep")
    if (is.null(names_sep)) {
      names_sep <- sep
    }
  }
  current_colnames <- names(data)

  # Preserve attributes
  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  } else {
    tbl_input <- FALSE
  }

  variable_attr <- lapply(data, attributes)

  not_selected <- setdiff(names(data), c(names_from, values_from))
  not_unstacked <- data[, not_selected, drop = FALSE]
  not_unstacked <- unique(not_unstacked)

  # unstack doesn't create NAs for combinations that don't exist (contrary to
  # reshape), so we need to complete the dataset before unstacking.

  new_data <- data

  # create an id with all variables that are not in names_from or values_from
  # so that we can create missing combinations between this id and names_from
  if (length(not_selected) > 1) {
    new_data$temporary_id <- do.call(paste, c(new_data[, not_selected, drop = FALSE], sep = "_"))
  } else if (length(not_selected) == 1) {
    new_data$temporary_id <- new_data[[not_selected]]
  } else {
    new_data$temporary_id <- seq_len(nrow(new_data))
  }

  # check that all_groups have all possible values for names_from
  # If not, need to complete the dataset with NA for values_from where names_from
  # didn't exist
  n_rows_per_group <- table(new_data$temporary_id)
  n_values_per_group <- insight::n_unique(n_rows_per_group)

  not_all_cols_are_selected <- length(not_selected) > 0

  incomplete_groups <-
    (n_values_per_group > 1 &&
      !all(unique(n_rows_per_group) %in% insight::n_unique(new_data[, names_from]))
    ) ||
      (n_values_per_group == 1 &&
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
      new_data, lookup, by = "temporary_id", join = "left"
    )

    # creation of missing combinations was done with a temporary id, so need
    # to fill columns that are not selected in names_from or values_from
    new_data[, not_selected] <- lapply(not_selected, function(x) {
      data <- data_arrange(new_data, c("temporary_id_2", x))
      ind <- which(!is.na(data[[x]]))
      rep_times <- diff(c(ind, length(data[[x]]) + 1))
      rep(data[[x]][ind], times = rep_times)
    })

    new_data <- data_arrange(new_data, c("temporary_id_2"))
  }

  # don't need temporary ids anymore
  new_data$temporary_id <- NULL
  new_data$temporary_id_2 <- NULL

  # Fill missing values (before converting to wide)
  if (!is.null(values_fill)) {
    if (length(values_fill) == 1) {
      if (is.numeric(new_data[[values_from]])) {
        if (!is.numeric(values_fill)) {
          insight::format_error(paste0("`values_fill` must be of type numeric."))
        } else {
          new_data <- convert_na_to(new_data, replace_num = values_fill)
        }
      } else if (is.character(new_data[[values_from]])) {
        if (!is.character(values_fill)) {
          insight::format_error(paste0("`values_fill` must be of type character."))
        } else {
          new_data <- convert_na_to(new_data, replace_char = values_fill)
        }
      } else if (is.factor(new_data[[values_from]])) {
        if (!is.factor(values_fill)) {
          insight::format_error(paste0("`values_fill` must be of type factor."))
        } else {
          new_data <- convert_na_to(new_data, replace_fac = values_fill)
        }
      }
    } else {
      if (verbose) {
        insight::format_error("`values_fill` must be of length 1.")
      }
    }
  }

  # convert to wide format (returns the data and the order in which columns
  # should be ordered)
  unstacked <- .unstack(new_data, names_from, values_from,
                        names_sep, names_prefix, names_glue)

  out <- unstacked$out

  if (length(values_from) > 1) {
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
      "Some values of the columns specified in 'names_from' are already present as column names.",
      paste0(
        "Either use `name_prefix` or rename the following columns: ",
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
    vapply(data[, values_from, drop = FALSE], .is_date, FUN.VALUE = logical(1))
  )
  if (values_are_dates) {
    for (i in unstacked$col_order) {
      out[[i]] <- as.Date.numeric(out[[i]], origin = "1970-01-01")
    }
  }

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
    res[[i]] <- c(tapply(x[[values_from[i]]], x$future_colnames, as.vector))
    if (length(values_from) > 1) {
      names(res[[i]]) <- paste0(values_from[i], names_sep, names(res[[i]]))
    }
  }

  # if there's a single variable in "values_from" and this variable only has
  # one value, need to make it a dataframe

  if (length(res) == 1 && !is.list(res[[1]])) {
    res <- data.frame(
      matrix(
        res[[1]],
        nrow = 1, dimnames = list(c(), names(res[[1]]))
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
