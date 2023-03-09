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

  if (convert_factors) {
    data <- .post_process_imported_data(
      data,
      convert_factors,
      verbose
    )
  }
}

.write_csv <- function(data, path, delimiter = ",") {
  insight::check_if_installed("readr")
  if (delimiter == ",") {
    readr::write_csv(x = data, file = path, ...)
  } else {
    readr::write_csv2(x = data, file = path, ...)
  }
}
