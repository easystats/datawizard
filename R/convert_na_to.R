#' @title Replace missing values in a variable or a dataframe.
#' @name convert_na_to
#'
#' @description
#' Replace missing values in a variable or a dataframe.
#'
#' @param x A numeric, factor, or character vector, or a data frame.
#' @param replacement Numeric or character value that will be used to replace `NA`.
#' @inheritParams standardize
#' @param verbose Toggle warnings.
#' @param ... Not used.
#'
#' @return
#' `x`, where all `NA` values are replaced by `replacement`.
#'
#' @export

convert_na_to <- function(x, ...) {
  UseMethod("convert_na_to")
}


#' @rdname convert_na_to
#' @export
convert_na_to.numeric <- function(x, replacement = NULL, verbose = TRUE) {

  if (is_empty_object(replacement) || !is.numeric(replacement)) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("`replacement` needs to be a numeric vector."), call. = FALSE)
    }
  } else if (length(replacement) > 1) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("`replacement` needs to be of length one."), call. = FALSE)
    }
  } else {
    x[is.na(x)] <- replacement
  }
  x
}


#' @export
convert_na_to.factor <- function(x, replacement = NULL, verbose = TRUE, ...) {

  if (is_empty_object(replacement) || length(replacement) > 1) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("`replacement` needs to be of length one."), call. = FALSE)
    }
  } else {
    x <- addNA(x)
    levels(x) <- c(levels(x), replacement)
    x[is.na(x)] <- replacement
  }
  x
}


#' @export
convert_na_to.character <- function(x, replacement = NULL, verbose = TRUE) {

  if (is_empty_object(replacement) || !is.character(replacement)) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("`replacement` needs to be a character vector."), call. = FALSE)
    }
  } else if (length(replacement) > 1) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("`replacement` needs to be of length one."), call. = FALSE)
    }
  } else {
    x[is.na(x)] <- replacement
  }
  x
}


#' @rdname convert_na_to
#' @export
convert_na_to.data.frame <- function(x, replacement_num = NULL, replacement_char = NULL, replacement_fac= NULL, select = NULL, exclude = NULL, verbose = TRUE, ...) {
  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  select <- .select_variables(x, select, exclude, force = TRUE)

  x[select] <- lapply(x[select], function(x) {
    if (is.numeric(x)) {
      repl <- replacement_num
    } else if (is.character(x)) {
      repl <- replacement_char
    } else if (is.factor(x)) {
      repl <- replacement_fac
    }
    convert_na_to(x, replacement = repl, verbose = FALSE, ...)
  })

  x

}

