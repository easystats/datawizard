#' @export
old_data_to_long <- function(data,
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
  if (!missing(colnames_to)) {
    .is_deprecated("colnames_to", "names_to")
    if (is.null(names_to)) {
      names_to <- colnames_to
    }
  }

  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  } else {
    tbl_input <- FALSE
  }

  ### Prefer "cols" over "select" for compat with tidyr::pivot_longer
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


  if (any(names_to %in% setdiff(names(data), cols))) {
    insight::format_error(
      "Some values of the columns specified in 'names_to' are already present as column names.",
      paste0(
        "Either use another value in `names_to` or rename the following columns: ",
        text_concatenate(names_to[which(names_to %in% setdiff(names(data), cols))])
      )
    )
  }

  # Sanity checks ----------------

  # nothing to select?
  if (!length(cols)) {
    stop("No columns found for reshaping data.", call. = FALSE)
  }

  # Reshaping ---------------------
  # Create Index column as needed by reshape
  data[["_Row"]] <- coerce_to_numeric(row.names(data))

  # Create a new index for cases with length(names_to) > 1
  names_to_2 <- paste(names_to, collapse = "_")

  # Reshape
  long <- stats::reshape(
    data,
    varying = cols,
    idvar = "_Row",
    v.names = values_to,
    timevar = names_to_2,
    direction = "long"
  )

  # Cleaning --------------------------
  # Sort the dataframe (to match pivot_longer's output)
  long <- long[do.call(order, long[, c("_Row", names_to_2)]), ]

  # Remove or rename the row index
  if (is.null(rows_to)) {
    long[["_Row"]] <- NULL
  } else {
    names(long)[names(long) == "_Row"] <- rows_to
  }

  # Re-insert col names as levels
  long[[names_to_2]] <- cols[long[[names_to_2]]]

  # if several variable in names_to, split the names either with names_sep
  # or with names_pattern
  if (length(names_to) > 1) {
    if (is.null(names_pattern)) {
      for (i in seq_along(names_to)) {
        new_vals <- unlist(lapply(
          strsplit(unique(long[[names_to_2]]), names_sep, fixed = TRUE),
          function(x) x[i]
        ))
        long[[names_to[i]]] <- new_vals
      }
    } else {
      tmp <- regmatches(
        unique(long[[names_to_2]]),
        regexec(names_pattern, unique(long[[names_to_2]]))
      )
      tmp <- as.data.frame(do.call(rbind, tmp), stringsAsFactors = FALSE)
      names(tmp) <- c(names_to_2, names_to)
      # faster than merge
      long <- cbind(long, tmp[match(long[[names_to_2]], tmp[[names_to_2]]), -1])
    }
    long[[names_to_2]] <- NULL
  }

  # reorder
  long <- data_relocate(long, select = values_to, after = -1)

  # remove names prefix if specified
  if (!is.null(names_prefix)) {
    if (length(names_to) > 1) {
      insight::format_error(
        "`names_prefix` only works when `names_to` is of length 1."
      )
    }
    long[[names_to]] <- gsub(paste0("^", names_prefix), "", long[[names_to]])
  }

  if (values_drop_na) {
    long <- long[!is.na(long[, values_to]), ]
  }

  # Reset row names
  row.names(long) <- NULL

  # Remove reshape attributes
  attributes(long)$reshapeLong <- NULL

  if (isTRUE(tbl_input)) {
    class(long) <- c("tbl_df", "tbl", "data.frame")
  }

  long
}




#' @export
old_data_to_wide <- function(data,
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
  old_names <- names(data)

  # Preserve attributes
  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  } else {
    tbl_input <- FALSE
  }
  variable_attr <- lapply(data, attributes)


  # Create an id for stats::reshape
  if (is.null(id_cols)) {
    row_index <- do.call(paste, c(data[, !names(data) %in% c(values_from, names_from), drop = FALSE], sep = "_"))
    if (length(row_index) == 0) row_index <- rep("", nrow(data))
    data[["_Rows"]] <- row_index
    id_cols <- "_Rows"
  }


  # create pattern of column names - stats::reshape renames columns that
  # concatenates "v.names" + values - we only want values
  current_colnames <- colnames(data)
  current_colnames <- current_colnames[current_colnames != "_Rows"]
  if (is.null(names_glue)) {
    future_colnames <- unique(do.call(paste, c(data[, names_from, drop = FALSE], sep = names_sep)))
  } else {
    vars <- regmatches(names_glue, gregexpr("\\{\\K[^{}]+(?=\\})", names_glue, perl = TRUE))[[1]]
    tmp_data <- unique(data[, vars])
    future_colnames <- unique(apply(tmp_data, 1, function(x) {
      tmp_vars <- list()
      for (i in seq_along(vars)) {
        tmp_vars[[i]] <- x[vars[i]]
      }

      tmp_colname <- gsub("\\{\\K[^{}]+(?=\\})", "", names_glue, perl = TRUE)
      tmp_colname <- gsub("\\{\\}", "%s", tmp_colname)
      do.call(sprintf, c(fmt = tmp_colname, tmp_vars))
    }))
  }


  # stop if some column names would be duplicated (follow tidyr workflow)
  if (any(future_colnames %in% current_colnames)) {
    insight::format_error(
      "Some values of the columns specified in 'names_from' are already present as column names.",
      paste0(
        "Either use `name_prefix` or rename the following columns: ",
        text_concatenate(current_colnames[which(current_colnames %in% future_colnames)])
      )
    )
  }

  # stats::reshape works strangely when several variables are in idvar/timevar
  # so we unite all ids in a single temporary column that will be used by
  # stats::reshape
  data$new_time <- do.call(paste, c(data[, names_from, drop = FALSE], sep = "_"))
  data[, names_from] <- NULL

  wide <- stats::reshape(
    data,
    v.names = values_from,
    idvar = id_cols,
    timevar = "new_time",
    sep = names_sep,
    direction = "wide"
  )

  # Clean
  if ("_Rows" %in% names(wide)) wide[["_Rows"]] <- NULL
  row.names(wide) <- NULL # Reset row names

  if (length(values_from) == 1) {
    to_rename <- which(startsWith(names(wide), paste0(values_from, names_sep)))
    names(wide)[to_rename] <- future_colnames
  }

  # Order columns as in tidyr
  if (length(values_from) > 1) {
    for (i in values_from) {
      wide <- data_relocate(
        wide,
        select = grep(paste0("^", i), names(wide), value = TRUE),
        after = -1
      )
    }
  }


  new_cols <- setdiff(names(wide), old_names)

  # Add prefix
  wide <- data_rename(wide, new_cols, paste0(names_prefix, new_cols))

  # Fill missing values
  if (!is.null(values_fill)) {
    if (length(values_fill) == 1) {
      if (is.numeric(wide[[new_cols[1]]])) {
        if (!is.numeric(values_fill)) {
          insight::format_error(paste0("`values_fill` must be of type numeric."))
        } else {
          wide <- convert_na_to(wide, replace_num = values_fill)
        }
      } else if (is.character(wide[[new_cols[1]]])) {
        if (!is.character(values_fill)) {
          insight::format_error(paste0("`values_fill` must be of type character."))
        } else {
          wide <- convert_na_to(wide, replace_char = values_fill)
        }
      } else if (is.factor(wide[[new_cols[1]]])) {
        if (!is.factor(values_fill)) {
          insight::format_error(paste0("`values_fill` must be of type factor."))
        } else {
          wide <- convert_na_to(wide, replace_fac = values_fill)
        }
      }
    } else {
      if (verbose) {
        insight::format_error("`values_fill` must be of length 1.")
      }
    }
  }


  # Remove reshape attributes
  attributes(wide)$reshapeWide <- NULL

  # add back attributes where possible
  for (i in colnames(wide)) {
    attributes(wide[[i]]) <- variable_attr[[i]]
  }

  if (isTRUE(tbl_input)) {
    class(wide) <- c("tbl_df", "tbl", "data.frame")
  }

  wide
}
