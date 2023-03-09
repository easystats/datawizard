#' @param data The data frame that should be written to a file.
#' @param delimiter For CSV-files, specifies the delimiter. Defaults to `","`,
#'   but in particular in European regions, `";"` might be a useful alternative,
#'   especially when CSV-files should be imported into MS Excel.
#' @rdname data_read
#' @export
data_write <- function(data,
                       path,
                       delimiter = ",",
                       convert_factors = FALSE,
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
    .write_csv(data, path, delimiter, verbose, ...)
  } else if (type %in% c("spss", "zspss")) {
    .write_spss(data, path, verbose, type, ...)
  } else if (type == "stata") {
    .write_spss(data, path, verbose, type, ...)
  } else if (type == "sas") {
    .write_spss(data, path, verbose, type, ...)
  }
}


# saving into CSV -----

.write_csv <- function(data, path, delimiter = ",", verbose = TRUE, ...) {
  insight::check_if_installed("readr")

  # this might make sense when writing labelled data to CSV
  if (convert_factors) {
    data <- .post_process_imported_data(
      data,
      convert_factors,
      verbose
    )
  }

  if (delimiter == ",") {
    readr::write_csv(x = data, file = path, ...)
  } else {
    readr::write_csv2(x = data, file = path, ...)
  }
}


# saving into SPSS -----

.write_spss <- function(data, path, verbose = TRUE, type = "spss", ...) {
  insight::check_if_installed("haven")

  if (identical(type, "zspss")) {
    compress <- "zsav"
  } else {
    compress <- "byte"
  }

  # labelled data needs "labelled" class attributes
  data <- .set_haven_class_attributes(data, verbose)

  # write to SPSS
  haven::write_sav(data = data, path = path, compress = compress, ...)
}


# helper -------------------------------

.set_haven_class_attributes <- function(x, verbose = TRUE) {
  if (verbose) {
    insight::format_alert("")
  }
  x[] <- laply(x, function(i) {
    # is type of labels same as type of vector? typically, character
    # vectors can have numeric labels or vice versa, numeric vectors
    # have "numeric" labels as character strings. in this case,
    # harmonize types of vector and labels, so haven doesn't complain
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

# helper to check whether a character can be coerced into numeric
.is_numeric_character <- function(x, na.rm = FALSE) {
  # check if we have numeric character values only
  if (na.rm) x <- stats::na.omit(x)
  !anyNA(suppressWarnings(as.numeric(x)))
}
