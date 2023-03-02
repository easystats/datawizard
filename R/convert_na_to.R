#' @title Replace missing values in a variable or a data frame.
#' @name convert_na_to
#'
#' @description
#' Replace missing values in a variable or a data frame.
#'
#' @param x A numeric, factor, or character vector, or a data frame.
#' @param replacement Numeric or character value that will be used to
#' replace `NA`.
#' @param verbose Toggle warnings.
#' @param ... Not used.
#'
#' @inheritSection center Selection of variables - the `select` argument
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
#' ### For data frames
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
#' # Convert a specific variable in the data frame
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


#' @export
convert_na_to.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert(
      sprintf(
        "Converting missing values (`NA`) into regular values currently not possible for variables of class `%s`.",
        class(x)[1]
      )
    )
  }
  x
}


#' @rdname convert_na_to
#' @export
convert_na_to.numeric <- function(x, replacement = NULL, verbose = TRUE, ...) {
  if (is_empty_object(replacement) || !is.numeric(replacement)) {
    if (isTRUE(verbose)) {
      insight::format_warning("`replacement` needs to be a numeric vector.")
    }
  } else if (length(replacement) > 1) {
    if (isTRUE(verbose)) {
      insight::format_warning("`replacement` needs to be of length one.")
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
      insight::format_warning("`replacement` needs to be of length one.")
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
  if (is_empty_object(replacement) || !is.character(replacement) && !is.numeric(replacement)) {
    if (isTRUE(verbose)) {
      insight::format_warning(
        "`replacement` needs to be a character or numeric vector."
      )
    }
  } else if (length(replacement) > 1) {
    if (isTRUE(verbose)) {
      insight::format_warning("`replacement` needs to be of length one.")
    }
  } else {
    x[is.na(x)] <- as.character(replacement)
  }
  x
}


#' @param replace_num Value to replace `NA` when variable is of type numeric.
#' @param replace_char Value to replace `NA` when variable is of type character.
#' @param replace_fac Value to replace `NA` when variable is of type factor.
#' @inheritParams find_columns
#'
#' @rdname convert_na_to
#' @export
convert_na_to.data.frame <- function(x,
                                     select = NULL,
                                     exclude = NULL,
                                     replacement = NULL,
                                     replace_num = replacement,
                                     replace_char = replacement,
                                     replace_fac = replacement,
                                     ignore_case = FALSE,
                                     regex = FALSE,
                                     verbose = TRUE,
                                     ...) {
  data <- x
  select_nse <- .select_nse(
    select,
    data,
    exclude = exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  # default
  lookup <- lapply(x, function(y) {
    if (is.numeric(y)) {
      replace_num
    } else if (is.character(y)) {
      replace_char
    } else if (is.factor(y)) {
      replace_fac
    }
  })

  # override for specific vars
  try_eval <- try(eval(select), silent = TRUE)
  select_is_list <- !inherits(try_eval, "try-error") && is.list(select)

  if (select_is_list) {
    for (i in select_nse) {
      lookup[[i]] <- select[[i]]
    }
  } else {
    lookup <- lookup[names(lookup) %in% select_nse]
  }

  lookup <- Filter(Negate(is.null), lookup)

  for (i in names(lookup)) {
    x[[i]] <- convert_na_to(x[[i]], replacement = lookup[[i]], verbose = verbose)
  }

  x
}
