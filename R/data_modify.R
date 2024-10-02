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
#' - For data frames (including grouped ones), the function `n()` can be used to count the
#'   number of observations and thereby, for instance, create index values by
#'   using `id = 1:n()` or `id = 3:(n()+2)` and similar.
#'
#' Note that newly created variables can be used in subsequent expressions,
#' including `.at` or `.if`. See also 'Examples'.
#'
#' @param .at A character vector of variable names that should be modified. This
#' argument is used in combination with the `.modify` argument. Note that only one
#' of `.at` or `.if` can be provided, but not both at the same time. Newly created
#' variables in `...` can also be selected, see 'Examples'.
#' @param .if A function that returns `TRUE` for columns in the data frame where
#' `.if` applies. This argument is used in combination with the `.modify` argument.
#' Note that only one of `.at` or `.if` can be provided, but not both at the same
#' time. Newly created variables in `...` can also be selected, see 'Examples'.
#' @param .modify A function that modifies the variables defined in `.at` or `.if`.
#' This argument is used in combination with either the `.at` or the `.if` argument.
#' Note that the modified variable (i.e. the result from `.modify`) must be either
#' of length 1 or of same length as the input variable.
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
#'   c12hour_z2 = standardize(c12hour),
#'   id = 1:n()
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
#'
#' # modify at specific positions or if condition is met
#' d <- iris[1:5, ]
#' data_modify(d, .at = "Species", .modify = as.numeric)
#' data_modify(d, .if = is.factor, .modify = as.numeric)
#'
#' # can be combined with dots
#' data_modify(d, new_length = Petal.Length * 2, .at = "Species", .modify = as.numeric)
#'
#' # new variables used in `.at` or `.if`
#' data_modify(
#'   d,
#'   new_length = Petal.Length * 2,
#'   .at = c("Petal.Length", "new_length"),
#'   .modify = round
#' )
#'
#' # combine "extract_column_names()" and ".at" argument
#' out <- data_modify(
#'   d,
#'   .at = extract_column_names(d, select = starts_with("Sepal")),
#'   .modify = as.factor
#' )
#' # "Sepal.Length" and "Sepal.Width" are now factors
#' str(out)
#'
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
data_modify.data.frame <- function(data, ..., .if = NULL, .at = NULL, .modify = NULL) {
  dots <- eval(substitute(alist(...)))

  # error for data frames with no rows...
  if (nrow(data) == 0) {
    insight::format_error("`data` is an empty data frame. `data_modify()` only works for data frames with at least one row.") # nolint
  }

  # check if we have dots, or only at/modify ----

  if (length(dots)) {
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
      # create new variable
      new_variable <- .get_new_dots_variable(dots, i, data)
      # give informative error when new variable doesn't match number of rows
      if (!is.null(new_variable) && length(new_variable) != nrow(data) && (nrow(data) %% length(new_variable)) != 0) {
        insight::format_error(
          "New variable has not the same length as the other variables in the data frame and cannot be recycled."
        )
      }
      data[[names(dots)[i]]] <- new_variable
    }
  }

  # check if we have at/modify ----
  data <- .modify_at(data, .at, .if, .modify)

  data
}

#' @export
data_modify.grouped_df <- function(data, ..., .if = NULL, .at = NULL, .modify = NULL) {
  # we need to evaluate dots here, and pass them with "do.call" to
  # the data.frame method later...
  dots <- match.call(expand.dots = FALSE)[["..."]]

  # error for data frames with no rows...
  if (nrow(data) == 0) {
    insight::format_error("`data` is an empty data frame. `data_modify()` only works for data frames with at least one row.") # nolint
  }

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

  # check if we have at/modify ----
  data <- .modify_at(data, .at, .if, .modify)

  # set back attributes and class
  data <- .replace_attrs(data, attr_data)
  class(data) <- class_attr
  data
}


# helper -------------

.modify_at <- function(data, .at, .if, .modify) {
  # check if ".at" or ".if" is defined, but not ".modify"
  if (is.null(.modify)) {
    if (!is.null(.at) || !is.null(.if)) {
      insight::format_error("You need to specify `.modify` when using `.at` or `.if`.")
    }
    return(data)
  }
  # make sure "modify" is a function
  if (!is.function(.modify)) {
    insight::format_error("`.modify` must be a function.")
  }
  # make sure either .at or .if is defined, not both
  if (!is.null(.at) && !is.null(.if)) {
    insight::format_error("You cannot use both `.at` and `.if` at the same time.")
  }
  # make sure at least one of .at or .if is defined
  if (is.null(.at) && is.null(.if)) {
    insight::format_error("You need to specify either `.at` or `.if`.")
  }

  column_names <- colnames(data)

  # if we have ".if" defined, specify ".at"
  if (!is.null(.if)) {
    .at <- column_names[vapply(data, .if, logical(1))]
  }
  # check for valid defined column names
  if (!all(.at %in% column_names)) {
    not_found <- .at[!.at %in% column_names]
    insight::format_error(
      paste0(
        "Variable",
        ifelse(length(not_found) > 1, "s ", " "),
        text_concatenate(not_found, enclose = "\""),
        ifelse(length(not_found) > 1, " were", " was"),
        " not found in the dataset."
      ),
      .misspelled_string(column_names, not_found, "Possibly misspelled or not yet defined?")
    )
  }
  for (i in .at) {
    result <- tryCatch(.modify(data[[i]]), warning = function(e) e, error = function(e) e)
    if (inherits(result, c("error", "warning"))) {
      insight::format_error(
        paste0("Error in modifying variable \"", i, "\": ", result$message),
        "Please check if you correctly specified the `.modify` function."
      )
    } else {
      data[[i]] <- result
    }
  }

  data
}

.get_new_dots_variable <- function(dots, i, data) {
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
  symbol_string <- insight::safe_deparse(symbol)
  if (!is.null(symbol_string) && all(symbol_string == "n()")) {
    # "special" functions - using "n()" just returns number of rows
    new_variable <- nrow(data)
  } else if (!is.null(symbol_string) && length(symbol_string) == 1 && grepl("\\bn\\(\\)", symbol_string)) {
    # "special" functions, like "1:n()" or similar - but not "1:fun()"
    symbol_string <- str2lang(gsub("n()", "nrow(data)", symbol_string, fixed = TRUE))
    new_variable <- try(with(data, eval(symbol_string)), silent = TRUE)
  } else {
    # default evaluation of expression
    new_variable <- try(with(data, eval(symbol)), silent = TRUE)
  }

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

  new_variable
}
