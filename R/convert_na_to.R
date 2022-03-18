#' @title Replace missing values in a variable or a dataframe.
#' @name convert_na_to
#'
#' @description
#' Replace missing values in a variable or a dataframe.
#'
#' @param x A numeric, factor, or character vector, or a data frame.
#' @param replacement Numeric or character value that will be used to
#' replace `NA`.
#' @param verbose Toggle warnings.
#' @param ... Not used.
#'
#' @return
#' `x`, where `NA` values are replaced by `replacement`.
#'
#' @examples
#' # Convert NA to 0 in a numeric vector
#' convert_na_to(
#'   c(9, 3, NA, 2, 3, 1, NA, 8),
#'   replacement = 0
#' )
#'
#' # Convert NA to "missing" in a character vector
#' convert_na_to(
#'   c("a", NA, "d", "z", NA, "t"),
#'   replacement = "missing"
#' )
#'
#' ### For dataframes
#'
#' test_df <- data.frame(
#'   x = c(1, 2, NA),
#'   x2 = c(4, 5, NA),
#'   y = c("a", "b", NA)
#' )
#'
#' # Convert all NA to 0 in numeric variables, and all NA to "missing" in
#' # character variables
#' convert_na_to(
#'   test_df,
#'   replace_num = 0,
#'   replace_char = "missing"
#' )
#'
#' # Convert a specific variable in the dataframe
#' convert_na_to(
#'   test_df,
#'   replace_num = 0,
#'   replace_char = "missing",
#'   select = "x"
#' )
#'
#' # Convert all variables starting with "x"
#' convert_na_to(
#'   test_df,
#'   replace_num = 0,
#'   replace_char = "missing",
#'   select = starts_with("x")
#' )
#'
#' # Convert NA to 1 in variable 'x2' and to 0 in all other numeric
#' # variables
#' convert_na_to(
#'   test_df,
#'   replace_num = 0,
#'   select = list(x2 = 1)
#' )
#'
#' @export

convert_na_to <- function(x, ...) {
  UseMethod("convert_na_to")
}


#' @rdname convert_na_to
#' @export
convert_na_to.numeric <- function(x, replacement = NULL, verbose = TRUE, ...) {

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


#' @rdname convert_na_to
#' @export
convert_na_to.character <- function(x, replacement = NULL, verbose = TRUE, ...) {

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


#' @param replace_num Value to replace `NA` when variable is of type numeric.
#' @param replace_char Value to replace `NA` when variable is of type character.
#' @param replace_fac Value to replace `NA` when variable is of type factor.
#' @param select 	Either
#'
#'   - a variable specified as a literal variable name (e.g., `column_name`),
#'   - a string with the variable name (e.g., `"column_name"`),
#'   - a formula with variable names (e.g., `~column_1 + column_2`),
#'   - one of the following select-helpers: `starts_with("")`, `ends_with("")`,
#'   `contains("")`, a range using `:` or `regex("")`,
#'   - or a named list specifying variables and their specific replacement.
#'
#' Multiple variables can also be extracted using a character vector of
#' length > 1, or a numeric vector containing column indices.
#'
#' See `Examples` for more details.
#'
#' @param exclude A vector of variable names in which `NA` will not be
#' replaced.
#' @param ignore_case Logical. If `TRUE` and when one of the select-helpers or
#' a regular expression is used in `select`, ignores lower/upper case in the
#' search pattern when matching against variable names.
#'
#' @rdname convert_na_to
#' @export
convert_na_to.data.frame <- function(x, replace_num = NULL, replace_char = NULL, replace_fac = NULL, select = NULL, exclude = NULL, verbose = TRUE, ignore_case = FALSE, ...) {

  if (is.list(select)) {

    names_data <- names(x)
    names_sel <- names(select)
    not_in_sel <- names_data[which(!names_data %in% names_sel)]

    for (i in seq_along(names_sel)) {
      if (!names_sel[i] %in% names_data) next
      x[[names_sel[i]]] <- convert_na_to(
        x[[names_sel[i]]],
        replacement = select[[i]],
        verbose = verbose
      )
    }

    for (i in seq_along(not_in_sel)) {
      to_convert <- x[[not_in_sel[i]]]
      if (is.numeric(to_convert)) {
        repl <- replace_num
      } else if (is.character(to_convert)) {
        repl <- replace_char
      } else if (is.factor(to_convert)) {
        repl <- replace_fac
      }
      if (!is.null(repl)) {
        x[[not_in_sel[i]]] <- convert_na_to(to_convert, replacement = repl, verbose = FALSE)
      }
    }

    return(x)

  }

  data <- x
  fixed <- TRUE
  # avoid conflicts
  conflicting_packages <- .conflicting_packages("poorman")

  # in case pattern is a variable from another function call...
  p <- try(eval(select), silent = TRUE)
  if (inherits(p, c("try-error", "simpleError"))) {
    p <- substitute(select)
  }

  # check if pattern is a function like "starts_with()"
  select <- tryCatch(
    eval(p),
    error = function(e)
      NULL
  )

  # if select could not be evaluated (because expression "makes no sense")
  # try to evaluate and find select-helpers. In this case, set fixed = FALSE,
  # so we can use grepl()
  if (is.null(select)) {
    evaluated_pattern <-
      .evaluate_pattern(insight::safe_deparse(p), data, ignore_case = ignore_case)
    select <- evaluated_pattern$pattern
    fixed <- evaluated_pattern$fixed
  }

  # seems to be no valid column name or index, so try to grep
  if (isFALSE(fixed)) {
    select <-
      colnames(data)[grepl(select, colnames(data), ignore.case = ignore_case)]
  }

  # load again
  .attach_packages(conflicting_packages)

  # return valid column names, based on pattern
  select <- .evaluated_pattern_to_colnames(select, data, ignore_case, verbose = FALSE, exclude)


  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  select <- .select_variables(x, select, exclude, force = TRUE)

  x[select] <- lapply(x[select], function(x) {
    if (is.numeric(x)) {
      repl <- replace_num
    } else if (is.character(x)) {
      repl <- replace_char
    } else if (is.factor(x)) {
      repl <- replace_fac
    }
    convert_na_to(x, replacement = repl, verbose = FALSE)
  })

  x

}

