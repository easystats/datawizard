#' Create new variables in a data frame
#'
#' Create new variables in a data frame. Unlike `base::transform()`, `data_modify()`
#' can be used on grouped data frames, and newly created variables can be directly
#' used.
#'
#' @param data The data frame, for which new variables should be created.
#' @param ... One or more expressions that define the new variable name and the
#' values or recoding of those new variables. These expressions can be one of:
#' - A sequence of named, literal expressions, where the left-hand side refers
#'   to the name of the new variable, while the right-hand side represent the
#'   values of the new variable. Example: `Sepal.Width = center(Sepal.Width)`.
#' - The *right-hand* side can also be represented as character string. Example:
#'   `Sepal_Width_c = "Sepal.Width - mean(Sepal.Width"`
#' - You can mix both literal and character strings as right-hand side. Example:
#'   ```
#'   data_modify(
#'     iris,
#'     Sepal.Width = center(Sepal.Width),
#'     Sepal_Width_c = "Sepal.Width - mean(Sepal.Width"
#'   )
#'   ```
#' - A (single) character vector of expressions. Example:
#'   `c("SW_double = 2 * Sepal.Width", "SW_fraction = SW_double / 10")`. This
#'   type of expression cannot be mixed with other ways of defining expressions,
#'   i.e. this example will **not** work:
#'   ```
#'   data_modify( # doesn't work!
#'     iris,
#'     c(
#'       "Sepal.Width = center(Sepal.Width)",
#'       "Sepal_Width_c = Sepal.Width - mean(Sepal.Width)"
#'     ),
#'     Sepal_W_z = Sepal_Width_c / sd(Sepal.With)
#'   )
#'   ```
#' - A (single) list of character expressions. Example:
#'   `list("SW_double = 2 * Sepal.Width", "SW_fraction = SW_double / 10")`.
#'   Like the single character vector of expressions, this type of expression
#'   cannot be mixed with other ways of defining expressions.
#'
#' Note that newly created variables can be used in subsequent expressions.
#' See also 'Examples'.
#' @param verbose Toggle messages.
#'
#' @note `data_modify()` can also be used inside functions. However, it is
#' recommended to pass the recode-expression as character vector or list of
#' characters.
#'
#' @examples
#' data(efc)
#' new_efc <- data_modify(
#'   efc,
#'   c12hour_c = center(c12hour),
#'   c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE),
#'   c12hour_z2 = standardize(c12hour)
#' )
#' head(new_efc)
#'
#' # using character strings
#' new_efc <- data_modify(
#'   efc,
#'   c12hour_c = center(c12hour),
#'   c12hour_z = "c12hour_c / sd(c12hour, na.rm = TRUE)"
#' )
#' head(new_efc)
#'
#' # attributes - in this case, value and variable labels - are preserved
#' str(new_efc)
#'
#' # works on grouped data
#' grouped_efc <- data_group(efc, "c172code")
#' new_efc <- data_modify(
#'   grouped_efc,
#'   c12hour_c = center(c12hour),
#'   c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE),
#'   c12hour_z2 = standardize(c12hour)
#' )
#' head(new_efc)
#'
#' # works from inside functions
#' foo <- function(data, z) {
#'   head(data_modify(data, z))
#' }
#' foo(iris, "var_a = Sepal.Width / 10")
#'
#' # works from inside functions
#' foo <- function(data, z) {
#'   head(data_modify(data, z))
#' }
#' new_exp <- c("SW_double = 2 * Sepal.Width", "SW_fraction = SW_double / 10")
#' foo(iris, new_exp)
#' @export
data_modify <- function(data, ...) {
  UseMethod("data_modify")
}

#' @export
data_modify.default <- function(data, ...) {
  insight::format_error("`data` must be a data frame.")
}

#' @rdname data_modify
#' @export
data_modify.data.frame <- function(data, ..., verbose = TRUE) {
  dots <- match.call(expand.dots = FALSE)$`...`

  # we check for list or character vector of expressions, in which case
  # "dots" should be unnamed, and also only of length 1
  if (length(dots) == 1 && is.null(names(dots))) {
    # expression is given as character string, e.g.
    # a <- "double_SepWidth = 2 * Sepal.Width"
    # data_modify(iris, a)
    # or as character vector / list of character strings e.g.
    # data_modify(iris, c("var_a = Sepal.Width / 10", "var_b = Sepal.Width * 10"))
    # data_modify(iris, list("var_a = Sepal.Width / 10", "var_b = Sepal.Width * 10"))
    character_symbol <- tryCatch(.dynEval(dots[[1]]), error = function(e) NULL)
    # check if not NULL - then we can assume we have either a list or a vector
    if (!is.null(character_symbol)) {
      # turn list into vector
      if (is.list(character_symbol)) {
        character_symbol <- unlist(character_symbol)
      }
      # do we have a character vector? Then we can proceed
      if (is.character(character_symbol)) {
        dots <- lapply(character_symbol, function(s) {
          # turn value from character vector into expression
          str2lang(.dynEval(s))
        })
        names(dots) <- vapply(dots, function(n) insight::safe_deparse(n[[2]]), character(1))
      }
    }
  }

  for (i in seq_along(dots)) {
    # iterate expressions for new variables
    symbol <- dots[[i]]

    # expression is given as character, e.g.
    # data_modify(iris, "double_SepWidth = 2 * Sepal.Width")
    if (is.character(symbol)) {
      symbol <- str2lang(symbol)
    }

    # finally, we can evaluate expression and get values for new variables
    new_variable <- with(data, eval(symbol))

    # give informative error when new variable doesn't match number of rows
    if (length(new_variable) != nrow(data) && (nrow(data) %% length(new_variable)) != 0) {
      insight::format_error("New variable has not the same length as the other variables in the data frame.")
    }

    # give informative message when variable was recycled
    if (length(new_variable) != nrow(data) && verbose) {
      insight::format_alert(paste0(
        "Variable ", names(dots)[i], " had ", length(new_variable),
        " value", ifelse(length(new_variable) > 1, "s", ""),
        " and was recycled to match the number of rows in the data."
      ))
    }
    data[[names(dots)[i]]] <- new_variable
  }
  data
}

#' @export
data_modify.grouped_df <- function(data, ..., verbose = TRUE) {
  # we need to evaluate dots here, and pass them with "do.call" to
  # the data.frame method later...
  dots <- match.call(expand.dots = FALSE)$`...`
  # save attributes, convert to regular data frame
  grps <- attr(data, "groups", exact = TRUE)[[".rows"]]
  attr_data <- attributes(data)
  data <- as.data.frame(data)
  # create new variables now - else we cannot replace rows later
  for (i in names(dots)) {
    data[[i]] <- NA
  }
  # perform transform on groups
  for (rows in grps) {
    data[rows, ] <- do.call("data_modify", c(list(data[rows, , drop = FALSE]), dots, list(verbose = verbose)))
    # only throw message once
    verbose <- FALSE
  }
  # set back class, so data frame still works with dplyr
  data <- .replace_attrs(data, attr_data)
  data
}
