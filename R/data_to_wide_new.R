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
  # reshape), so it makes a list of unequal length and not a dataframe.
  # Solution: complete the dataset before unstacking, but it takes some time.

  foo <- as.data.frame(table(data[, names_from]))
  if (insight::n_unique(foo$Freq) == 1 ||
      (insight::n_unique(foo$Freq) == 2 && 0 %in% unique(foo$Freq))) {
    new_data <- data
  } else {
    new_data <- unique(expand.grid(data[, c(not_selected, names_from)]))
    new_data <- data_merge(new_data, data, by = c(not_selected, names_from))
  }

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

  for (i in names_from) {
    if (is.factor(new_data[[i]])) {
      new_data[[i]] <- as.character(new_data[[i]])
    }
  }

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

  out <- out[, unstacked$col_order]

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

  if (is.null(names_glue)) {
    x$tmp <- do.call(paste, c(x[, names_from, drop = FALSE], sep = names_sep))
  } else {
    vars <- regmatches(names_glue, gregexpr("\\{\\K[^{}]+(?=\\})", names_glue, perl = TRUE))[[1]]
    tmp_data <- x[, vars]
    x$tmp <- .gluestick(names_glue, src = tmp_data)
  }

  x$tmp <- paste0(names_prefix, x$tmp)

  res <- list()
  for (i in seq_along(values_from)) {
    res[[i]] <- c(tapply(x[[values_from[i]]], x$tmp, as.vector))
    if (length(values_from) > 1) {
      names(res[[i]]) <- paste0(values_from[i], names_sep, names(res[[i]]))
    }
  }

  if (length(res) == 1 && !is.list(res[[1]])) {
    res <- data.frame(
      matrix(
        res[[1]], nrow = 1, dimnames = list(c(), unique(x$tmp))
      ),
      stringsAsFactors = FALSE
    )
  } else {
    res <- unlist(res, recursive = FALSE)
  }

  list(
    out = data.frame(res, stringsAsFactors = FALSE),
    col_order = unique(x$tmp)
  )

}



#' Taken from https://github.com/coolbutuseless/gluestick
#' @noRd

.gluestick <- function(fmt, src = parent.frame(), open = "{", close = "}", eval = TRUE) {

  nchar_open  <- nchar(open)
  nchar_close <- nchar(close)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity checks
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Brute force the open/close characters into a regular expression for
  # extracting the expressions from the format string
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  open  <- gsub("(.)", "\\\\\\1", open ) # Escape everything!!
  close <- gsub("(.)", "\\\\\\1", close) # Escape everything!!
  re    <- paste0(open, ".*?", close)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract the delimited expressions
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  matches  <- gregexpr(re, fmt)
  exprs    <- regmatches(fmt, matches)[[1]]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Remove the delimiters
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  exprs <- substr(exprs, nchar_open + 1L, nchar(exprs) - nchar_close)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # create a valid sprintf fmt string.
  #  - replace all "{expr}" strings with "%s"
  #  - escape any '%' so sprintf() doesn't try and use them for formatting
  #    but only if the '%' is NOT followed by an 's'
  #
  # gluestick() doesn't deal with any pathological cases
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  fmt_sprintf <- gsub(re      , "%s", fmt)
  fmt_sprintf <- gsub("%(?!s)", "%%", fmt_sprintf, perl=TRUE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Evaluate
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (eval) {
    args <- lapply(exprs, function(expr) {eval(parse(text = expr), envir = src)})
  } else {
    args <- unname(mget(exprs, envir = as.environment(src)))
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the string(s)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  do.call(sprintf, c(list(fmt_sprintf), args))
}
