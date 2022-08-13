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
#' @examples
#' \donttest{
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
#' if (require("psych")) {
#'   data <- psych::bfi # Wide format with one row per participant's personality test
#'
#'   # Pivot long format
#'   data_to_long(data,
#'     select = regex("\\d"), # Select all columns that contain a digit
#'     colnames_to = "Item",
#'     values_to = "Score",
#'     rows_to = "Participant"
#'   )
#'
#' if(require("tidyr")) {
#'   reshape_longer(
#'     tidyr::who,
#'     select = new_sp_m014:newrel_f65,
#'     names_to = c("diagnosis", "gender", "age"),
#'     names_pattern = "new_?(.*)_(.)(.*)",
#'     values_to = "count"
#'   )
#' }
#'
#' }
#' }
#'
#' @inherit data_rename seealso
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
  if (!missing(colnames_to)) {
    .is_deprecated("colnames_to", "names_to")
    if (is.null(names_to)) {
      names_to <- colnames_to
    }
  }

  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data)
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
      stop(insight::format_message(
        "You need to specify columns to pivot, either with `select` or `cols`."
      ), call. = FALSE)
    }
  }


  if (any(names_to %in% setdiff(names(data), cols))) {
    stop(insight::format_message(
      "Some values of the columns specified in 'names_to' are already present as column names.",
      paste0(
        "Either use another value in `names_to` or rename the following columns: ",
        text_concatenate(names_to[which(names_to %in% setdiff(names(data), cols))])
      )
    ), call. = FALSE)
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
    for (i in seq_along(names_to)) {
      if (is.null(names_pattern)) {
        new_vals <- unlist(lapply(
          strsplit(unique(long[[names_to_2]]), names_sep, fixed = TRUE),
          function(x) x[i]
        ))
        long[[names_to[i]]] <- new_vals
      } else {
        tmp <- regmatches(
          unique(long[[names_to_2]]),
          regexec(names_pattern, unique(long[[names_to_2]]))
        )
        tmp <- as.data.frame(do.call(rbind, tmp))[, c(1, i + 1)]
        names(tmp) <- c(names_to_2, names_to[i])
        long <- data_join(long, tmp)
      }
    }
    long[[names_to_2]] <- NULL
  }

  # reorder
  long <- data_relocate(long, select = values_to, after = -1)

  # remove names prefix if specified
  if (!is.null(names_prefix)) {
    if (length(names_to) > 1) {
      stop(insight::format_message(
        "`names_prefix` only works when `names_to` is of length 1."
      ), call. = FALSE)
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
  old_names <- names(data)

  # Preserve attributes
  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data)
  } else {
    tbl_input <- FALSE
  }
  variable_attr <- lapply(data, attributes)


  # Create an id for stats::reshape
  if (is.null(id_cols)) {
    data[["_Rows"]] <- apply(data[, !names(data) %in% c(values_from, names_from), drop = FALSE], 1, paste, collapse = "_")
    id_cols <- "_Rows"
  }


  # create pattern of column names - stats::reshape renames columns that
  # concatenates "v.names" + values - we only want values
  current_colnames <- colnames(data)
  current_colnames <- current_colnames[current_colnames != "_Rows"]
  if (is.null(names_glue)) {
    future_colnames <- unique(apply(data, 1, function(x) paste(x[c(names_from)], collapse = names_sep)))
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
    stop(insight::format_message(
      "Some values of the columns specified in 'names_from' are already present as column names.",
      paste0(
        "Either use `name_prefix` or rename the following columns: ",
        text_concatenate(current_colnames[which(current_colnames %in% future_colnames)])
      )
    ), call. = FALSE)
  }

  # stats::reshape works strangely when several variables are in idvar/timevar
  # so we unite all ids in a single temporary column that will be used by
  # stats::reshape
  data$new_time <- apply(data, 1, function(x) paste(x[names_from], collapse = "_"))
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
          stop(insight::format_message(paste0("`values_fill` must be of type numeric.")), call. = FALSE)
        } else {
          wide <- convert_na_to(wide, replace_num = values_fill)
        }
      } else if (is.character(wide[[new_cols[1]]])) {
        if (!is.character(values_fill)) {
          stop(insight::format_message(paste0("`values_fill` must be of type character.")), call. = FALSE)
        } else {
          wide <- convert_na_to(wide, replace_char = values_fill)
        }
      } else if (is.factor(wide[[new_cols[1]]])) {
        if (!is.factor(values_fill)) {
          stop(insight::format_message(paste0("`values_fill` must be of type factor.")), call. = FALSE)
        } else {
          wide <- convert_na_to(wide, replace_fac = values_fill)
        }
      }
    } else {
      if (verbose) {
        stop(insight::format_message("`values_fill` must be of length 1."), call. = FALSE)
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


# Aliases -----------------------------------------------------------------

#' @rdname data_to_long
#' @export
reshape_longer <- data_to_long

#' @rdname data_to_wide
#' @export
reshape_wider <- data_to_wide
