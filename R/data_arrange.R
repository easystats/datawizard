#' Arrange rows by column values
#'
#' `data_arrange()` orders the rows of a data frame by the values of selected
#' columns.
#'
#' @param data A data frame, or an object that can be coerced to a data frame.
#' @param select Character vector of column names. Use a dash just before column
#'   name to arrange in decreasing order, for example `"-x1"`.
#' @param safe Do not throw an error if one of the variables specified doesn't
#'   exist.
#'
#' @return A data frame.
#'
#' @examples
#'
#' # Arrange using several variables
#' data_arrange(head(mtcars), c("gear", "carb"))
#'
#' # Arrange in decreasing order
#' data_arrange(head(mtcars), "-carb")
#'
#' # Throw an error if one of the variables specified doesn't exist
#' try(data_arrange(head(mtcars), c("gear", "foo"), safe = FALSE))
#' @export
data_arrange <- function(data, select = NULL, safe = TRUE) {
  UseMethod("data_arrange")
}


#' @export
data_arrange.default <- function(data, select = NULL, safe = TRUE) {
  if (is.null(select) || length(select) == 0) {
    return(data)
  }

  # Input validation check
  data <- .coerce_to_dataframe(data)

  # find which vars should be decreasing
  desc <- select[startsWith(select, "-")]
  desc <- gsub("^-", "", desc)
  select <- gsub("^-", "", select)

  # check for variables that are not in data
  dont_exist <- setdiff(select, colnames(data))

  if (length(dont_exist) > 0) {
    if (safe) {
      insight::format_warning(
        paste0(
          "The following column(s) don't exist in the dataset: ",
          text_concatenate(dont_exist), "."
        ),
        .misspelled_string(names(data), dont_exist, "Possibly misspelled?")
      )
    } else {
      insight::format_error(
        paste0(
          "The following column(s) don't exist in the dataset: ",
          text_concatenate(dont_exist), "."
        ),
        .misspelled_string(names(data), dont_exist, "Possibly misspelled?")
      )
    }
    select <- select[-which(select %in% dont_exist)]
  }

  if (length(select) == 0) {
    return(data)
  }

  already_sorted <- all(vapply(data[, select, drop = FALSE], .is_sorted, logical(1L)))

  if (isTRUE(already_sorted)) {
    return(data)
  }

  out <- data

  # reverse order for variables that should be decreasing
  if (length(desc) > 0) {
    for (i in desc) {
      out[[i]] <- -xtfrm(out[[i]])
    }
  }

  # apply ordering
  if (length(select) == 1) {
    out <- data[order(out[[select]]), ]
  } else {
    out <- data[do.call(order, out[, select]), ]
  }

  if (!insight::object_has_rownames(data)) {
    rownames(out) <- NULL
  }

  out
}


#' @export
data_arrange.grouped_df <- function(data, select = NULL, safe = TRUE) {
  grps <- attr(data, "groups", exact = TRUE)
  grps <- grps[[".rows"]]

  out <- lapply(grps, function(x) {
    data_arrange.default(data[x, ], select = select, safe = safe)
  })

  out <- do.call(rbind, out)

  if (!insight::object_has_rownames(data)) {
    rownames(out) <- NULL
  }

  out
}
