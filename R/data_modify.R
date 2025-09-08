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
#' - A vector of length 1 (which will be recycled to match the number of rows
#'   in the data), or of same length as the data.
#' - A variable that contains a value to be used. Example:
#'   ```r
#'   a <- "abc"
#'   data_modify(iris, var_abc = a) # var_abc contains "abc"
#'   ```
#' - An expression can also be provided as string and wrapped in
#'   `as_expr()`. Example:
#'   ```r
#'   data_modify(iris, as_expr("Sepal.Width = center(Sepal.Width)"))
#'   # or
#'   a <- "center(Sepal.Width)"
#'   data_modify(iris, Sepal.Width = as_expr(a))
#'   # or
#'   a <- "Sepal.Width = center(Sepal.Width)"
#'   data_modify(iris, as_expr(a))
#'   ```
#'   Note that `as_expr()` is no real function, which cannot be used outside
#'   of `data_modify()`, and hence it is not exported nor documented. Rather,
#'   it is only used for internally processing expressions.
#' - Using `NULL` as right-hand side removes a variable from the data frame.
#'   Example: `Petal.Width = NULL`.
#' - For data frames (including grouped ones), the function `n()` can be used to
#'   count the number of observations and thereby, for instance, create index
#'   values by using `id = 1:n()` or `id = 3:(n()+2)` and similar. Note that,
#'   like `as_expr()`, `n()` is also no true function and cannot be used outside
#'   of `data_modify()`.
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
#'   as_expr("c12hour_c = center(c12hour)"),
#'   as_expr("c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)"),
#'   as_expr("c12hour_z2 = standardize(c12hour)")
#' )
#' head(new_efc)
#'
#' # using a character vector, provided a variable
#' xpr <- c(
#'   "c12hour_c = center(c12hour)",
#'   "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)",
#'   "c12hour_z2 = standardize(c12hour)"
#' )
#' new_efc <- data_modify(efc, as_expr(xpr))
#' head(new_efc)
#'
#' # using character strings, provided as variable
#' stand <- "c12hour_c / sd(c12hour, na.rm = TRUE)"
#' new_efc <- data_modify(
#'   efc,
#'   c12hour_c = center(c12hour),
#'   c12hour_z = as_expr(stand)
#' )
#' head(new_efc)
#'
#' # attributes - in this case, value and variable labels - are preserved
#' str(new_efc)
#'
#' # using `paste()` to build a string-expression
#' to_standardize <- c("Petal.Length", "Sepal.Length")
#' out <- data_modify(
#'   iris,
#'   as_expr(
#'     paste0(to_standardize, "_stand = standardize(", to_standardize, ")")
#'   )
#' )
#' head(out)
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
#' foo1 <- function(data, ...) {
#'   head(data_modify(data, ...))
#' }
#' foo1(iris, SW_fraction = Sepal.Width / 10)
#' # or
#' foo1(iris, as_expr("SW_fraction = Sepal.Width / 10"))
#'
#' # also with string arguments, using `as_expr()`
#' foo2 <- function(data, modification) {
#'   head(data_modify(data, as_expr(modification)))
#' }
#' foo2(iris, "SW_fraction = Sepal.Width / 10")
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
    # Check if dots are named. Usually, all dots should be named, i.e. include
    # the name of the new variable. There's only one exception, if a string is
    # masked as expression, and this string includes the new name, e.g.
    #
    # data_modify(iris, as_expr("sepwid = 2 * Sepal.Width"))
    # a <- "sepwid = 2 * Sepal.Width"
    # data_modify(iris, as_expr(a))
    #
    dots <- .process_unnamed_expressions(dots, data)

    # next, we check for named expression-tags and convert these into regular
    # expressions, e.g.
    #
    # data_modify(iris, sepwid =  = as_expr("2 * Sepal.Width"))
    # a <- "2 * Sepal.Width"
    # data_modify(iris, sepwid = as_expr(a))
    #
    dots <- .process_named_expressions(dots, data)

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

  if (length(dots)) {
    # check is dots are named. Usually, all dots should be named, i.e. include
    # the name of the new variable. There's only one exception, if a string is
    # masked as expression, and this string includes the new name, e.g.
    #
    # data_modify(iris, as_expr("sepwid = 2 * Sepal.Width"))
    # a <- "sepwid = 2 * Sepal.Width"
    # data_modify(iris, as_expr(a))
    #
    dots <- .process_unnamed_expressions(dots, data)

    # next, we check for named expression-tags and convert these into regular
    # expressions, e.g.
    #
    # data_modify(iris, sepwid =  = as_expr("2 * Sepal.Width"))
    # a <- "2 * Sepal.Width"
    # data_modify(iris, sepwid = as_expr(a))
    #
    dots <- .process_named_expressions(dots, data)
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


# expression processing ----------------------------------------------------

.process_unnamed_expressions <- function(dots, data) {
  # dots are only unnamed, when the full expression is saved in a string,
  # e.g. data_modify(iris, as_expr("sepwid = 2 * Sepal.Width")).
  # Thus, we know we *have to* find an expression here, and the string value
  # *must* contain a name definition. If not, fail. If yes, convert string
  # into a language expression...

  if (!is.null(names(dots)) && all(nzchar(names(dots)))) {
    # if all elements are named, return early
    return(dots)
  }

  # find which dots are unnamed, check those for expressions
  if (is.null(names(dots))) {
    unnamed_dots <- seq_along(dots)
  } else {
    unnamed_dots <- which(!nzchar(names(dots)))
  }

  for (i in rev(unnamed_dots)) {
    # copy dot-element and convert to string for manipulation
    dot_element <- dots[[i]]
    symbol_string <- insight::safe_deparse(dot_element)
    # sanity check - this may happen when user wants to remove a variable,
    # e.g. data_modify(iris, as_expr("Species = NULL"))
    if (is.null(symbol_string)) next
    # we only allow unnamed elements if these are masked as expression. String
    # values or numeric values require a named element, i.e. we can only have
    # data_modify(iris, newvar = "a"), but we cannot have data_modify(iris, "a").
    # For expression, missing name is possible.
    if (!startsWith(symbol_string, "as_expr")) {
      insight::format_error(paste0(
        "A variable name for the expression `", symbol_string, "` is missing. ",
        "Please use something like `new_name = ", symbol_string, "`."
      ))
    }
    # next, check if the string-expression includes a name for the new variable
    # therefore, we remove the "as_expr()" token
    if (startsWith(symbol_string, "as_expr")) {
      symbol_string <- insight::trim_ws(
        gsub("as_expr\\((.*)\\)", "\\1", symbol_string)
      )
    }
    # remove c(), split at comma, if we have a vector of expressions
    if (startsWith(symbol_string, "c(")) {
      symbol_string <- gsub("c\\((.*)\\)", "\\1", symbol_string)
      # only split at highest-level comma
      pattern <- ",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)" # suggestion by Co-pilot
      # Locate commas not inside quotes
      symbol_string <- insight::trim_ws(unlist(strsplit(symbol_string, pattern, perl = TRUE), use.names = FALSE))
    }
    # check if we have any symbols instead of strings as expression, e.g.
    # xpr <- "sepwid = 2 * Sepal.Width"
    # data_modify(iris, as_expr(xpr))
    #
    # in this case, we need to evaluate the symbol (i.e. convert symbol string
    # into a language expression and then evaluate)
    symbol_string <- .evaluate_expression_in_string(symbol_string, data)
    # check whether we have exact one = sign. We need to have a name definition,
    # i.e. something like "var = a+b" - if the string has no "=" sign, name is
    # definitely missing
    pattern <- "(?<!=)=(?!=)"
    has_names <- grepl(pattern, symbol_string, perl = TRUE)
    if (!all(has_names)) {
      insight::format_error(paste0(
        "A variable name for the expression `", symbol_string[!has_names[1]], "` is missing. ",
        "Please use something like `new_name = ", symbol_string[!has_names[1]], "`."
      ))
    }
    # extract names (split at =), separate name from new variable from its
    # expression. we need this to create a named list of expressions for the dots
    symbol_string <- lapply(
      strsplit(symbol_string, "=", fixed = TRUE),
      function(split_result) {
        # we may have multiple "=" signs, e.g. when we have the pattern
        # "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)". In this case
        # paste remaining parts to one string again
        first_part <- split_result[1]
        second_part <- paste(split_result[-1], collapse = "=")
        insight::trim_ws(c(first_part, second_part))
      }
    )
    # extract names (LHS)
    symbol_names <- vapply(symbol_string, function(i) i[1], character(1))
    # extract expressions (RHS)
    symbol_string <- lapply(symbol_string, function(i) str2lang(.fix_quotes(i[2])))
    names(symbol_string) <- symbol_names
    # copy to dots... if we have a character vector, one dot element may
    # return more than one expression elements. Thus, we have to insert /
    # replace the old element by one or more new elements
    if (length(dots) == 1) {
      return_value <- symbol_string
    } else if (i == 1) {
      return_value <- c(symbol_string, dots[(i + 1):length(dots)])
    } else if (i == length(dots)) {
      return_value <- c(dots[1:(i - 1)], symbol_string)
    } else {
      return_value <- c(dots[1:(i - 1)], symbol_string, dots[(i + 1):length(dots)])
    }
    dots <- return_value
  }
  dots
}


.process_named_expressions <- function(dots, data) {
  # this is basically a shorter version of ".process_unnamed_expressions()",
  # because we don't need to extract the name definition of the string, which
  # makes the handling easier. See ".process_unnamed_expressions()" for a more
  # comprehensive documentation of the single steps.

  for (i in seq_along(dots)) {
    dot_element <- dots[[i]]
    symbol_string <- insight::safe_deparse(dot_element)
    # sanity check, this may happen when user wants to remove a variable
    # e.g. data_modify(iris, Species = NULL)
    if (is.null(symbol_string)) next
    # extract string-expression, if we have any
    if (startsWith(symbol_string, "as_expr")) {
      symbol_string <- gsub("as_expr\\((.*)\\)", "\\1", symbol_string)
    } else {
      # no expression token found
      symbol_string <- NULL
    }
    # here we found an expression token - convert string into a regular expression
    if (!is.null(symbol_string)) {
      # check if we have any symbols instead of strings as expression
      symbol_string <- .evaluate_expression_in_string(symbol_string, data)
      # remove quotes from strings and save symbol name
      symbol_string <- .fix_quotes(symbol_string)
      symbol_name <- names(dots)[i]
      # convert string into language and replace in dots
      return_value <- try(str2lang(symbol_string), silent = TRUE)
      # sanity check - for invalid expressions, like
      # data_modify(iris, a = as_expr(c("1 + 1", "2 + 2")))
      # we get an error here
      if (inherits(return_value, "try-error")) {
        insight::format_error(paste0(
          "Could not evaluate expression `", symbol_string[1], "`. ",
          "Please check if it's correctly specified. If you think there's a bug ",
          "in `data_modify()`, please file an issue at {.url https://github.com/easystats/datawizard/issues}"
        ))
      }
      dots[[i]] <- return_value
      names(dots)[i] <- symbol_name
    }
  }
  dots
}


# helper -------------


.evaluate_expression_in_string <- function(symbol_string, data) {
  # check if we have any symbols instead of strings as expression, e.g.
  # xpr <- "sepwid = 2 * Sepal.Width"
  # data_modify(iris, as_expr(xpr))
  #
  # in this case, we need to evaluate the symbol (i.e. convert symbol string
  # into a language expression and then evaluate)
  symbol_string <- unlist(lapply(symbol_string, function(symbol_element) {
    if (startsWith(symbol_element, "\"")) {
      symbol_element
    } else {
      return_value <- .dynEval(str2lang(symbol_element))
      # dynEval might fail if we don't look in data - sanity check
      if (identical(return_value, symbol_element)) {
        return_value <- .dynEval(str2lang(symbol_element), data = data)
      }
      return_value
    }
  }), use.names = FALSE)
  # now we should have the expression as character string. Next, we
  # # remove quotes from strings
  gsub("^\"(.*)\"$", "\\1", symbol_string)
}


.fix_quotes <- function(symbol_string) {
  # if user uses double-quotes inside double-quotes, these are escaped by
  # "\", e.g. data_modify(iris, foo = as_expr("grepl(\"a\", Species)"))
  # In this case, we have double-backslashes, which need to be removed.
  # Furthermore, to avoid adding back backslashes, we replace by single quotes
  # Using single quotes inside a string, even if escaped with backslash, is no
  # problem here. Main issue is that if a string is parsed, double-quotes are
  # *always* escaped, so we just need to make sure we only have single quotes
  # and then remove all backslashes
  gsub("\\", "", gsub("\"", "'", symbol_string, fixed = TRUE), fixed = TRUE)
}


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

  # we evaluate the content of "symbol", hence, "eval_symbol" either contains
  # the values of the expression, or the expression itself as string
  eval_symbol <- .dynEval(symbol, ifnotfound = NULL, data = data)

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
    # evaluate symbol
    new_variable <- try(with(data, eval(symbol)), silent = TRUE)
    # if evaluation fails, we have a value - and directly use it
    if (inherits(new_variable, "try-error") && !is.null(eval_symbol)) {
      new_variable <- eval_symbol
    }
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
