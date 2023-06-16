#' Create new variables in a data frame
#'
#' Create new variables or modify existing variables in a data frame. Unlike `base::transform()`, `data_modify()`
#' can be used on grouped data frames, and newly created variables can be directly
#' used.
#'
#' @param data A data frame
#' @param ... One or more expressions that define the new variable name and the
#' values or recoding of those new variables. These expressions can be one of:
#' - A sequence of named, literal expressions, where the left-hand side refers
#'   to the name of the new variable, while the right-hand side represent the
#'   values of the new variable. Example: `Sepal.Width = center(Sepal.Width)`.
#' - A sequence of string values, representing expressions.
#' - A variable that contains a string representation of the expression. Example:
#'   ```r
#'   a <- "2 * Sepal.Width"
#'   data_modify(iris, a)
#'   ```
#' - A character vector of expressions. Example:
#'   `c("SW_double = 2 * Sepal.Width", "SW_fraction = SW_double / 10")`. This
#'   type of expression cannot be mixed with other expressions, i.e. if a
#'   character vector is provided, you may not add further elements to `...`.
#' - Using `NULL` as right-hand side removes a variable from the data frame.
#'   Example: `Petal.Width = NULL`.
#'
#' Note that newly created variables can be used in subsequent expressions.
#' See also 'Examples'.
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
#' # using strings instead of literal expressions
#' new_efc <- data_modify(
#'   efc,
#'   "c12hour_c = center(c12hour)",
#'   "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)",
#'   "c12hour_z2 = standardize(c12hour)"
#' )
#' head(new_efc)
#'
#' # using character strings, provided as variable
#' stand <- "c12hour_c / sd(c12hour, na.rm = TRUE)"
#' new_efc <- data_modify(
#'   efc,
#'   c12hour_c = center(c12hour),
#'   c12hour_z = stand
#' )
#' head(new_efc)
#'
#' # providing expressions as character vector
#' new_exp <- c(
#'   "c12hour_c = center(c12hour)",
#'   "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)"
#' )
#' new_efc <- data_modify(efc, new_exp)
#' head(new_efc)
#'
#' # attributes - in this case, value and variable labels - are preserved
#' str(new_efc)
#'
#' # overwrite existing variable, remove old variable
#' out <- data_modify(iris, Petal.Length = 1 / Sepal.Length, Sepal.Length = NULL)
#' head(out)
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

#' @export
data_modify.data.frame <- function(data, ...) {
  dots <- eval(substitute(alist(...)))
  column_names <- colnames(data)

  # we check for character vector of expressions, in which case
  # "dots" should be unnamed
  if (is.null(names(dots))) {
    # if we have multiple strings, concatenate them to a character vector
    # and put it into a list...
    if (length(dots) > 1) {
      if (all(vapply(dots, is.character, logical(1)))) {
        dots <- list(unlist(dots))
      } else {
        insight::format_error("You cannot mix string and literal representation of expressions.")
      }
    }
    # expression is given as character string, e.g.
    # a <- "double_SepWidth = 2 * Sepal.Width"
    # data_modify(iris, a)
    # or as character vector, e.g.
    # data_modify(iris, c("var_a = Sepal.Width / 10", "var_b = Sepal.Width * 10"))
    character_symbol <- tryCatch(.dynEval(dots[[1]]), error = function(e) NULL)
    # do we have a character vector? Then we can proceed
    if (is.character(character_symbol)) {
      dots <- lapply(character_symbol, function(s) {
        # turn value from character vector into expression
        str2lang(.dynEval(s))
      })
      names(dots) <- vapply(dots, function(n) insight::safe_deparse(n[[2]]), character(1))
    }
  }

  for (i in seq_along(dots)) {
    # iterate expressions for new variables
    symbol <- dots[[i]]

    # expression is given as character string in a variable, but named, e.g.
    # a <- "2 * Sepal.Width"
    # data_modify(iris, double_SepWidth = a)
    # we reconstruct the symbol as if it were provided as literal expression.
    # However, we need to check that we don't have a character vector,
    # like: data_modify(iris, new_var = "a")
    # this one should be recycled instead.
    if (!is.character(symbol)) {
      eval_symbol <- .dynEval(symbol, ifnotfound = NULL)
      if (is.character(eval_symbol)) {
        symbol <- try(str2lang(paste0(names(dots)[i], " = ", eval_symbol)), silent = TRUE)
        # we may have the edge-case of having a function that returns a character
        # vector, like "new_var = sample(letters[1:3])". In this case, "eval_symbol"
        # is of type character, but no symbol, thus str2lang() above creates a
        # wrong pattern. We then take "eval_symbol" as character input.
        if (inherits(symbol, "try-error")) {
          symbol <- str2lang(paste0(
            names(dots)[i],
            " = c(", paste0("\"", eval_symbol, "\"", collapse = ","), ")"
          ))
        }
      }
    }

    # finally, we can evaluate expression and get values for new variables
    new_variable <- try(with(data, eval(symbol)), silent = TRUE)

    # successful, or any errors, like misspelled variable name?
    if (inherits(new_variable, "try-error")) {
      # in which step did error happen?
      step_number <- switch(as.character(i),
        "1" = "the first expression",
        "2" = "the second expression",
        "3" = "the third expression",
        paste("expression", i)
      )
      step_msg <- paste0("There was an error in ", step_number, ".")
      # try to find out which variable was the cause for the error
      error_msg <- attributes(new_variable)$condition$message
      if (grepl("object '(.*)' not found", error_msg)) {
        error_var <- gsub("object '(.*)' not found", "\\1", error_msg)
        insight::format_error(
          paste0(step_msg, " Variable \"", error_var, "\" was not found in the dataset or in the environment."),
          .misspelled_string(colnames(data), error_var, "Possibly misspelled or not yet defined?")
        )
      } else {
        insight::format_error(paste0(
          step_msg, " ", insight::format_capitalize(error_msg),
          ". Possibly misspelled or not yet defined?"
        ))
      }
    }

    # give informative error when new variable doesn't match number of rows
    if (!is.null(new_variable) && length(new_variable) != nrow(data) && (nrow(data) %% length(new_variable)) != 0) {
      insight::format_error(
        "New variable has not the same length as the other variables in the data frame and cannot be recycled."
      )
    }

    data[[names(dots)[i]]] <- new_variable
  }

  data
}

#' @export
data_modify.grouped_df <- function(data, ...) {
  # we need to evaluate dots here, and pass them with "do.call" to
  # the data.frame method later...
  dots <- match.call(expand.dots = FALSE)$`...`

  # works only for dplyr >= 0.8.0
  grps <- attr(data, "groups", exact = TRUE)
  grps <- grps[[".rows"]]
  attr_data <- attributes(data)

  # remove conflicting class attributes
  class_attr <- class(data)
  data <- as.data.frame(data)

  # we check for character vector of expressions, in which case
  # "dots" should be unnamed
  if (is.null(names(dots))) {
    # if we have multiple strings, concatenate them to a character vector
    # and put it into a list...
    if (length(dots) > 1) {
      if (all(vapply(dots, is.character, logical(1)))) {
        dots <- list(unlist(dots))
      } else {
        insight::format_error("You cannot mix string and literal representation of expressions.")
      }
    }
    # expression is given as character string, e.g.
    # a <- "double_SepWidth = 2 * Sepal.Width"
    # data_modify(iris, a)
    # or as character vector, e.g.
    # data_modify(iris, c("var_a = Sepal.Width / 10", "var_b = Sepal.Width * 10"))
    character_symbol <- tryCatch(.dynEval(dots[[1]]), error = function(e) NULL)
    # do we have a character vector? Then we can proceed
    if (is.character(character_symbol)) {
      dots <- lapply(character_symbol, function(s) {
        # turn value from character vector into expression
        str2lang(.dynEval(s))
      })
      names(dots) <- vapply(dots, function(n) insight::safe_deparse(n[[2]]), character(1))
    }
  }

  # create new variables as dummys, do for-loop works
  for (i in names(dots)) {
    # don't overwrite / fill existing variables with NA,
    # e.g. if we have "data_modify(iris, Sepal.Length = normalize(Sepal.Length))"
    # normalize() won't work when we fill with NA
    if (!i %in% colnames(data)) {
      data[[i]] <- NA
    }
  }

  # create new variables per group
  for (rows in grps) {
    data[rows, ] <- data_modify.data.frame(data[rows, ], ...)
  }

  # set back attributes and class
  data <- .replace_attrs(data, attr_data)
  class(data) <- class_attr
  data
}
