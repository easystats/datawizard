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
  type <- switch(.file_ext(path),
    "txt" = ,
    "csv" = "csv",
    "sav" = ,
    "por" = "spss",
    "zsav" = "zspss",
    "dta" = "stata",
    "xpt" = "sas",
    NULL
  )

  # stop on unsupported
  if (is.null(type)) {
    insight::format_error(
      "Unknow file type. Supported file types are \".csv\", \".sav\", \".dta\" and \".xpt\"."
    )
  }

  if (type == "csv") {
    .write_csv(data, path, delimiter, convert_factors, save_variable_labels, verbose, ...)
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
    data <- .post_process_imported_data(
      data,
      convert_factors,
      verbose
    )
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

  # repair duplicated value labels
  data <- .repair_value_labels(data)

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


# haven used to be very strict about the type of value labels (attached as
# attributes), which had to be of the same type as the variable. Here we
# harmonize the type of variables and their value labels
.set_haven_class_attributes <- function(x, verbose = TRUE) {
  if (verbose) {
    insight::format_alert("")
  }
  x[] <- laply(x, function(i) {
    # is type of labels same as type of vector? typically, character
    # vectors can have numeric labels or vice versa, numeric vectors
    # have "numeric" labels as character strings. in this case,
    # harmonize types of vector and labels, so haven doesn't complain.
    # we skip factors, because those are automatically labelled
    if (!is.factor(i) && !is.null(attr(i, "labels", exact = TRUE))) {
      label_type <- as.vector(attr(i, "labels", exact = TRUE))
      if (!is.null(label_type) && typeof(label_type) != typeof(i)) {
        lab.at <- attr(i, "labels", exact = TRUE)
        nlab <- names(lab.at)
        if (is.integer(i) && !is.integer(label_type)) {
          lab.at <- as.integer(lab.at)
        } else if (.is_numeric_character(label_type, na.rm = TRUE)) {
          lab.at <- as.numeric(lab.at)
        } else {
          lab.at <- as.character(lab.at)
        }
        names(lab.at) <- nlab
        attr(i, "labels") <- lab.at
      }
      class(i) <- c("haven_labelled", "vctrs_vctr")
    }
    i
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


# sometimes we have duplicated labels for partially labelled vectors, e.g.
#  low   ..   ..   .. high
#    1    2    3    4    5
# In this case, to ensure proper writing into SPSS or similar, we need to
# repair those labels to avoid duplicates.
.repair_value_labels <- function(x) {
  for (i in seq_len(ncol(x))) {
    labs <- attr(x[[i]], "labels", exact = TRUE)
    # check if we have a labelled vector
    if (!is.null(labs)) {
      # extract all duplicates, if any
      duped_labels <- duplicated(names(labs), fromLast = TRUE) | duplicated(names(labs))
      if (any(duped_labels)) {
        # append value to label, so we have unique labels again
        names(labs)[duped_labels] <- paste0(names(labs)[duped_labels], "_", labs[duped_labels])
        attr(x[[i]], "labels") <- labs
      }
    }
  }
  x
}


# helper to check whether a character can be coerced into numeric
.is_numeric_character <- function(x, na.rm = FALSE) {
  # check if we have numeric character values only
  if (na.rm) x <- stats::na.omit(x)
  !anyNA(suppressWarnings(as.numeric(x)))
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
      if (!is.null(l)) {
        as.character(i)
      } else {
        i
      }
    })

    # add new row with labels
    x[nrow(x) + 1, ] <- labs

    # re-order rows, so labels are first row
    x <- x[c(nrow(x), seq_len(nrow(x) - 1)), ]
  }

  x
}
