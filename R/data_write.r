#' @param data The data frame that should be written to a file.
#' @param delimiter For CSV-files, specifies the delimiter. Defaults to `","`,
#'   but in particular in European regions, `";"` might be a useful alternative,
#'   especially when CSV-files should be imported into MS Excel.
#' @param save_variable_labels Only applies to CSV files. If `TRUE`, variable
#' labels (if any) will be saved to the CSV file as additional row after the
#' column names row.
#' @rdname data_read
#' @export
data_write <- function(data,
                       path,
                       delimiter = ",",
                       convert_factors = FALSE,
                       save_variable_labels = FALSE,
                       verbose = TRUE,
                       ...) {
  # check file type, so we know the target dta format
  file_type <- .file_ext(path)
  type <- switch(file_type,
    "txt" = ,
    "csv" = "csv",
    "sav" = ,
    "por" = "spss",
    "zsav" = "zspss",
    "dta" = "stata",
    "xpt" = "sas",
    "unknown"
  )

  # no file type provided?
  if (!is.character(file_type) || file_type == "") {
    insight::format_error(
      "Could not detect file type. The `path` argument has no file extension.",
      "Please provide a file path including extension, like \"myfile.csv\" or \"c:/Users/Default/myfile.sav\"."
    )
  }

  if (type == "csv") {
    .write_csv(data, path, delimiter, convert_factors, save_variable_labels, verbose, ...)
  } else if (type == "unknown") {
    .write_unknown(data, path, convert_factors, save_variable_labels, verbose, ...)
  } else {
    .write_haven(data, path, verbose, type, ...)
  }
}


# saving into CSV -----

.write_csv <- function(data,
                       path,
                       delimiter = ",",
                       convert_factors = FALSE,
                       save_variable_labels = FALSE,
                       verbose = TRUE,
                       ...) {
  insight::check_if_installed("readr")

  # this might make sense when writing labelled data to CSV
  if (convert_factors) {
    data <- .pre_process_exported_data(data, convert_factors)
  }

  # add row with variable labels
  if (save_variable_labels) {
    data <- .add_labels_as_row(data)
  }

  if (delimiter == ",") {
    readr::write_csv(x = data, file = path, ...)
  } else {
    readr::write_csv2(x = data, file = path, ...)
  }
}


# saving into unknown -----

.write_unknown <- function(data,
                           path,
                           convert_factors = FALSE,
                           save_variable_labels = FALSE,
                           verbose = TRUE,
                           ...) {
  insight::check_if_installed("rio")

  # this might make sense when writing labelled data to CSV
  if (convert_factors) {
    data <- .pre_process_exported_data(data, convert_factors)
  }

  # add row with variable labels
  if (save_variable_labels) {
    data <- .add_labels_as_row(data)
  }

  rio::export(x = data, file = path, ...)
}


# saving into haven format -----

.write_haven <- function(data, path, verbose = TRUE, type = "spss", ...) {
  insight::check_if_installed("haven")

  if (identical(type, "zspss")) {
    compress <- "zsav"
  } else {
    compress <- "byte"
  }

  # labelled data needs "labelled" class attributes
  data <- .set_haven_class_attributes(data, verbose)

  # fix invalid column names
  data <- .fix_column_names(data)

  if (type %in% c("spss", "zspss")) {
    # write to SPSS
    haven::write_sav(data = data, path = path, compress = compress, ...)
  } else if (type == "stata") {
    # write to Stata
    haven::write_dta(data = data, path = path, ...)
  } else {
    # write to SAS
    haven::write_xpt(data = data, path = path, ...)
  }
}




# helper -------------------------------


# make sure we have the "labelled" class for labelled data
.set_haven_class_attributes <- function(x, verbose = TRUE) {
  insight::check_if_installed("haven")

  if (verbose) {
    insight::format_alert("Preparing data file: converting variable types.")
  }
  x[] <- lapply(x, function(i) {
    value_labels <- attr(i, "labels", exact = TRUE)
    variable_label <- attr(i, "label", exact = TRUE)
    if (is.factor(i)) {
      # factor requires special preparation to save levels as labels
      haven::labelled(
        x = as.numeric(i),
        labels = stats::setNames(seq_along(levels(i)), levels(i)),
        label = variable_label
      )
    } else if (!is.null(value_labels) || !is.null(variable_label)) {
      # make sure we have labelled class for labelled data
      haven::labelled(x = i, labels = value_labels, label = variable_label)
    } else {
      # non labelled data can be saved "as is"
      i
    }
  })
  x
}


# packages like SPSS cannot deal with variable which names end with a dot
# fix column names here by added a "fix" suffix
.fix_column_names <- function(x) {
  # check for correct column names
  dot_ends <- vapply(colnames(x), endsWith, FUN.VALUE = TRUE, suffix = ".")
  if (any(dot_ends)) {
    colnames(x)[dot_ends] <- paste0(colnames(x)[dot_ends], "fix")
  }
  x
}


# variable labels cannot be saved to CSV format, but we can add
# variable labels as data row to the file
.add_labels_as_row <- function(x) {
  # extract labels
  labs <- vapply(x, function(i) {
    l <- attr(i, "label", exact = TRUE)
    if (is.null(l)) {
      l <- ""
    }
    l
  }, character(1))

  # any labels?
  if (!all(labs == "")) {
    # convert variables to character, else we cannot add the character label
    # to it as new value. The row with labels is a data row...
    x[] <- lapply(x, function(i) {
      l <- attr(i, "label", exact = TRUE)
      as.character(i)
    })

    # add new row with labels
    x[nrow(x) + 1, ] <- labs

    # re-order rows, so labels are first row
    x <- x[c(nrow(x), seq_len(nrow(x) - 1)), ]
  }

  x
}


# process imported data from SPSS, SAS or Stata -----------------------

.pre_process_exported_data <- function(x, convert_factors) {
  # user may decide whether we automatically detect variable type or not
  if (isTRUE(convert_factors)) {
    x[] <- lapply(x, function(i) {
      # only proceed if not all missing
      if (!all(is.na(i))) {
        # save labels
        value_labels <- attr(i, "labels", exact = TRUE)
        variable_labels <- attr(i, "label", exact = TRUE)

        # filter, so only matching value labels remain
        value_labels <- value_labels[value_labels %in% unique(i)]

        # guess variable type
        if (!is.character(i)) {
          # if all values are labelled, we assume factor. Use labels as levels
          if (!is.null(value_labels) && length(value_labels) == insight::n_unique(i)) {
            if (is.numeric(i)) {
              i <- factor(i, labels = names(value_labels))
            } else {
              i <- factor(as.character(i), labels = names(value_labels))
            }
            i <- as.character(i)
          } else {
            # else, fall back to numeric
            i <- as.numeric(as.character(i))
          }
        } else {
          # we need this to drop haven-specific class attributes
          i <- as.character(i)
        }
        # add back variable label
        attr(i, "label") <- variable_labels
      }
      i
    })
  } else {
    # drop haven class attributes
    x[] <- lapply(x, function(i) {
      # save labels
      class(i) <- setdiff(class(i), c("haven_labelled", "vctrs_vctr"))
      i
    })
  }

  class(x) <- "data.frame"
  x
}
