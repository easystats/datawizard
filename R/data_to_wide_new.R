new_data_to_wide <- function(
    data,
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
    sep
  ) {

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
                           sort = FALSE)

    # creation of missing combinations was done with a temporary id, so need
    # to fill columns that are not selected in names_from or values_from
    new_data[, not_selected] <- lapply(not_selected, function(x) {
      ave(new_data[[x]], new_data$temporary_id, FUN = function(y) {
        replace(y, is.na(y), y[!is.na(y)][1L])
      })
    })

    new_data <- data_arrange(new_data, "temporary_id")
  }

  # don't need a temporary id anymore
  new_data$temporary_id <- NULL

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
  unstacked <- .unstack(new_data, names_from, values_from, names_sep, names_prefix, names_glue)

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
        res[[1]], nrow = 1, dimnames = list(c(), unique(x$future_colnames))
      ),
      stringsAsFactors = FALSE
    )
  } else {
    res <- unlist(res, recursive = FALSE)
  }

  # return the wide data and the order in which the new columns should be

  list(
    out = data.frame(res, stringsAsFactors = FALSE),
    col_order = unique(x$future_colnames)
  )

}



#' Taken from https://github.com/coolbutuseless/gluestick
#' Same functionality as `{glue}`
#'
#' @noRd

.gluestick <- function(fmt, src = parent.frame(), open = "{", close = "}", eval = TRUE) {

  nchar_open  <- nchar(open)
  nchar_close <- nchar(close)

  # Sanity checks
  stopifnot(exprs = {
    is.character(fmt)
    length(fmt) == 1L
    is.character(open)
    length(open) == 1L
    nchar_open > 0L
    is.character(close)
    length(close) == 1
    nchar_close > 0
  })

  # Brute force the open/close characters into a regular expression for
  # extracting the expressions from the format string
  open  <- gsub("(.)", "\\\\\\1", open ) # Escape everything!!
  close <- gsub("(.)", "\\\\\\1", close) # Escape everything!!
  re    <- paste0(open, ".*?", close)

  # Extract the delimited expressions
  matches  <- gregexpr(re, fmt)
  exprs    <- regmatches(fmt, matches)[[1]]

  # Remove the delimiters
  exprs <- substr(exprs, nchar_open + 1L, nchar(exprs) - nchar_close)

  # create a valid sprintf fmt string.
  #  - replace all "{expr}" strings with "%s"
  #  - escape any '%' so sprintf() doesn't try and use them for formatting
  #    but only if the '%' is NOT followed by an 's'
  #
  # gluestick() doesn't deal with any pathological cases
  fmt_sprintf <- gsub(re      , "%s", fmt)
  fmt_sprintf <- gsub("%(?!s)", "%%", fmt_sprintf, perl=TRUE)

  # Evaluate
  if (eval) {
    args <- lapply(exprs, function(expr) {eval(parse(text = expr), envir = src)})
  } else {
    args <- unname(mget(exprs, envir = as.environment(src)))
  }

  # Create the string(s)
  do.call(sprintf, c(list(fmt_sprintf), args))
}
