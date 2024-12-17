#' @param data The data frame that should be written to a file.
#' @param delimiter For CSV-files, specifies the delimiter. Defaults to `","`,
#'   but in particular in European regions, `";"` might be a useful alternative,
#'   especially when exported CSV-files should be opened in Excel.
#' @param save_labels Only applies to CSV files. If `TRUE`, value and variable
#'   labels (if any) will be saved as additional CSV file. This file has the same
#'   file name as the exported CSV file, but includes a `"_labels"` suffix (i.e.
#'   when the file name is `"mydat.csv"`, the additional file with value and
#'   variable labels is named `"mydat_labels.csv"`).
#' @rdname data_read
#' @export
data_write <- function(data,
                       path,
                       delimiter = ",",
                       convert_factors = FALSE,
                       save_labels = FALSE,
                       verbose = TRUE,
                       ...) {
  # check file type, so we know the target dta format
  file_type <- .file_ext(path)
  type <- switch(file_type,
    txt = ,
    csv = "csv",
    sav = ,
    por = "spss",
    zsav = "zspss",
    dta = "stata",
    xpt = "sas",
    "unknown"
  )

  # no file type provided?
  if (!is.character(file_type) || file_type == "") {
    insight::format_error(
      "Could not detect file type. The `path` argument has no file extension.",
      "Please provide a file path including extension, like \"myfile.csv\" or \"c:/Users/Default/myfile.sav\"."
    )
  }

  if (type %in% c("csv", "unknown")) {
    .write_csv_or_unknown(data, path, type, delimiter, convert_factors, save_labels, verbose, ...)
  } else {
    .write_haven(data, path, verbose, type, ...)
  }
}


# saving into CSV or unknown format -----

.write_csv_or_unknown <- function(data,
                                  path,
                                  type = "csv",
                                  delimiter = ",",
                                  convert_factors = FALSE,
                                  save_labels = FALSE,
                                  verbose = TRUE,
                                  ...) {
  # save labels
  if (save_labels && type == "csv") {
    data <- .save_labels_to_file(data, path, delimiter, verbose)
  }

  # this might make sense when writing labelled data to CSV
  if (convert_factors) {
    data <- .pre_process_exported_data(data, convert_factors)
  }

  if (type == "csv") {
    insight::check_if_installed("readr")
    if (delimiter == ",") {
      readr::write_csv(x = data, file = path, ...)
    } else {
      readr::write_csv2(x = data, file = path, ...)
    }
  } else {
    insight::check_if_installed("rio")
    rio::export(x = data, file = path, ...)
  }
}


# saving into haven format -----

.write_haven <- function(data, path, verbose = TRUE, type = "spss", ...) {
  insight::check_if_installed("haven")

  # check if user provided "compress" argument for SPSS files,
  # else, default to compression
  dots <- list(...)
  if (!is.null(dots$compress)) {
    compress <- dots$compress
  } else if (identical(type, "zspss")) {
    compress <- "zsav"
  } else {
    compress <- "byte"
  }

  # labelled data needs "labelled" class attributes
  data <- .set_haven_class_attributes(data, verbose)

  # fix invalid column names
  data <- .fix_column_names(data, verbose)

  if (type %in% c("spss", "zspss")) {
    # write to SPSS
    haven::write_sav(data = data, path = path, compress = compress)
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
    # make sure we have labelled class for labelled data
    value_labels <- attr(i, "labels", exact = TRUE)
    variable_label <- attr(i, "label", exact = TRUE)
    # factor requires special preparation to save levels as labels
    # haven:::vec_cast_named requires "x" and "labels" to be of same type
    if (is.factor(i)) {
      haven::labelled(
        x = as.numeric(i),
        labels = stats::setNames(seq_along(levels(i)), levels(i)),
        label = variable_label
      )
    } else if (!is.null(value_labels) || !is.null(variable_label)) {
      # character requires special preparation to save value labels
      # haven:::vec_cast_named requires "x" and "labels" to be of same type
      if (is.character(i)) {
        # only prepare value labels when these are not NULL
        if (!is.null(value_labels)) {
          value_labels <- stats::setNames(as.character(value_labels), names(value_labels))
        }
        haven::labelled(
          x = i,
          labels = value_labels,
          label = variable_label
        )
      } else {
        # this should work for the remaining types...
        haven::labelled(x = i, labels = value_labels, label = variable_label)
      }
    } else {
      # non labelled data can be saved "as is"
      i
    }
  })
  x
}


# packages like SPSS cannot deal with variable which names end with a dot
# fix column names here by added a "fix" suffix
.fix_column_names <- function(x, verbose = TRUE) {
  # check for correct column names
  dot_ends <- vapply(colnames(x), endsWith, FUN.VALUE = TRUE, suffix = ".")
  if (any(dot_ends)) {
    if (verbose) {
      insight::format_alert("Found and fixed invalid column names so they can be read by other software packages.")
    }
    colnames(x)[dot_ends] <- paste0(colnames(x)[dot_ends], "fix")
  }
  x
}


# save value and variable labels as addtional file
.save_labels_to_file <- function(x, path, delimiter, verbose = TRUE) {
  insight::check_if_installed("readr")

  # get file path information
  fpath <- dirname(path)
  fname <- sub("\\.csv$", "", basename(path))
  path <- paste0(fpath, .Platform$file.sep, fname, "_labels.csv")

  if (verbose) {
    insight::format_alert(
      paste0("Saving variable and value labels to \"", path, "\".")
    )
  }

  # extract labels
  var_labs <- vapply(x, function(i) {
    l <- attr(i, "label", exact = TRUE)
    if (is.null(l)) {
      l <- ""
    }
    l
  }, character(1))

  # extract value labels
  value_labs <- vapply(x, function(i) {
    l <- attr(i, "labels", exact = TRUE)
    if (is.null(l)) {
      ""
    } else {
      paste0(l, "=", names(l), collapse = "; ")
    }
  }, character(1))

  out <- data.frame(
    variable = colnames(x),
    label = var_labs,
    labels = value_labs,
    stringsAsFactors = FALSE
  )

  if (delimiter == ",") {
    readr::write_csv(x = out, file = path)
  } else {
    readr::write_csv2(x = out, file = path)
  }
}


# process data for export, use factor levels as data values -------------------

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
        if (is.character(i)) {
          # we need this to drop haven-specific class attributes
          i <- as.character(i)
        } else if (!is.null(value_labels) && length(value_labels) == insight::n_unique(i)) {
          # if all values are labelled, we assume factor. Use labels as levels
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
