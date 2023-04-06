#' @title Convert non-missing values in a variable into missing values.
#' @name convert_to_na
#'
#' @description
#' Convert non-missing values in a variable into missing values.
#'
#' @param x A vector, factor or a data frame.
#' @param na Numeric, character vector or logical (or a list of numeric, character
#'   vectors or logicals) with values that should be converted to `NA`. Numeric
#'   values applied to numeric vectors, character values are used for factors,
#'   character vectors or date variables, and logical values for logical vectors.
#' @param drop_levels Logical, for factors, when specific levels are replaced
#'   by `NA`, should unused levels be dropped?
#' @param ... Not used.
#' @inheritParams find_columns
#'
#' @return
#' `x`, where all values in `na` are converted to `NA`.
#'
#' @examples
#' x <- sample(1:6, size = 30, replace = TRUE)
#' x
#' # values 4 and 5 to NA
#' convert_to_na(x, na = 4:5)
#'
#' # data frames
#' set.seed(123)
#' x <- data.frame(
#'   a = sample(1:6, size = 20, replace = TRUE),
#'   b = sample(letters[1:6], size = 20, replace = TRUE),
#'   c = sample(c(30:33, 99), size = 20, replace = TRUE)
#' )
#' # for all numerics, convert 5 to NA. Character/factor will be ignored.
#' convert_to_na(x, na = 5)
#'
#' # for numerics, 5 to NA, for character/factor, "f" to NA
#' convert_to_na(x, na = list(6, "f"))
#'
#' # select specific variables
#' convert_to_na(x, select = c("a", "b"), na = list(6, "f"))
#' @export
convert_to_na <- function(x, ...) {
  UseMethod("convert_to_na")
}


#' @export
convert_to_na.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert(
      sprintf(
        "Converting values into missing values (`NA`) currently not possible for variables of class `%s`.",
        class(x)[1]
      )
    )
  }
  x
}


#' @rdname convert_to_na
#' @export
convert_to_na.numeric <- function(x, na = NULL, verbose = TRUE, ...) {
  # if we have a list, use first valid element
  if (is.list(na)) {
    na <- unlist(na[vapply(na, is.numeric, FUN.VALUE = TRUE)], use.names = FALSE)
  }

  if (insight::is_empty_object(na) || !is.numeric(na)) {
    if (isTRUE(verbose)) {
      insight::format_alert(
        "Could not convert values into `NA` for a numeric variable.",
        "To do this, `na` needs to be a numeric vector, or a list that contains numeric vector elements."
      )
    }
  } else {
    matches <- which(x %in% na)
    x[matches] <- NA
    # drop unused labels
    value_labels <- attr(x, "labels", exact = TRUE)
    attr(x, "labels") <- value_labels[!value_labels %in% na]
  }
  x
}


#' @rdname convert_to_na
#' @export
convert_to_na.factor <- function(x, na = NULL, drop_levels = FALSE, verbose = TRUE, ...) {
  # if we have a list, use first valid element
  if (is.list(na)) {
    na <- unlist(na[vapply(na, is.character, FUN.VALUE = TRUE)], use.names = FALSE)
  }

  if (insight::is_empty_object(na) || (!is.factor(na) && !is.character(na))) {
    if (isTRUE(verbose)) {
      insight::format_alert(
        "Could not convert values into `NA` for a factor or character variable.",
        "To do this, `na` needs to be a character vector, or a list that contains character vector elements."
      )
    }
  } else {
    matches <- which(x %in% na)
    x[matches] <- NA
    # drop unused labels
    value_labels <- attr(x, "labels", exact = TRUE)
    if (is.factor(x) && isTRUE(drop_levels)) {
      x <- droplevels(x)
    }
    attr(x, "labels") <- value_labels[!value_labels %in% na]
  }
  x
}


#' @export
convert_to_na.character <- convert_to_na.factor


#' @export
convert_to_na.Date <- function(x, na = NULL, verbose = TRUE, ...) {
  # if we have a list, use first valid element
  if (is.list(na)) {
    na <- na[vapply(na, .is_date, FUN.VALUE = logical(1L))]
    if (length(na) > 1) {
      na <- na[[1]]
    }
  }

  if (insight::is_empty_object(na) || !.is_date(na)) {
    if (isTRUE(verbose)) {
      insight::format_alert(
        "Could not convert values into `NA` for a date/time variable.",
        "To do this, `na` must be of class 'Date'."
      )
    }
  } else {
    matches <- which(x == na)
    x[matches] <- NA
  }
  x
}


#' @export
convert_to_na.logical <- function(x, na = NULL, verbose = TRUE, ...) {
  # if we have a list, use first valid element
  if (is.list(na)) {
    na <- unlist(na[vapply(na, is.logical, FUN.VALUE = TRUE)], use.names = FALSE)
  }

  if (insight::is_empty_object(na) || !is.logical(na)) {
    if (isTRUE(verbose)) {
      insight::format_alert(
        "Could not convert values into `NA` for a logical variable.",
        "To do this, `na` needs to be a logical vector, or a list that contains logical vector elements."
      )
    }
  } else {
    matches <- which(x == na)
    x[matches] <- NA
  }
  x
}


#' @rdname convert_to_na
#' @export
convert_to_na.data.frame <- function(x,
                                     select = NULL,
                                     exclude = NULL,
                                     na = NULL,
                                     drop_levels = FALSE,
                                     ignore_case = FALSE,
                                     regex = FALSE,
                                     verbose = TRUE,
                                     ...) {
  # evaluate arguments
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  x[select] <- lapply(
    x[select],
    convert_to_na,
    na = na,
    drop_levels = drop_levels,
    verbose = verbose,
    ...
  )

  x
}
