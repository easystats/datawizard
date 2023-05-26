#' Create new variables in a data frame
#'
#' Create new variables in a data frame. Unlike `base::transform()`, `data_modify()`
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

#' @rdname data_modify
#' @export
data_modify.data.frame <- function(data, ..., verbose = TRUE) {
  dots <- match.call(expand.dots = FALSE)$`...`
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

  # for message later - we collect all recycled variables here
  recycled_variables <- NULL

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
        symbol <- str2lang(paste0(names(dots)[i], " = ", eval_symbol))
      }
    }

    # finally, we can evaluate expression and get values for new variables
    new_variable <- with(data, eval(symbol))

    # give informative error when new variable doesn't match number of rows
    if (!is.null(new_variable) && length(new_variable) != nrow(data) && (nrow(data) %% length(new_variable)) != 0) {
      insight::format_error("New variable has not the same length as the other variables in the data frame.")
    }

    # give informative message when variable was recycled
    if (!is.null(new_variable) && length(new_variable) != nrow(data)) {
      recycled_variables <- c(
        recycled_variables,
        stats::setNames(length(new_variable), names(dots)[i])
      )
    }
    data[[names(dots)[i]]] <- new_variable
  }

  # inform about recycled variables
  if (!is.null(recycled_variables) && verbose) {
    # prepare some constants...
    l_recycled <- length(recycled_variables)
    n_recycled <- insight::n_unique(recycled_variables)
    min_r <- min(recycled_variables)
    max_r <- max(recycled_variables)
    # ...probably makes this composition a bit clearer
    insight::format_alert(paste0(
      "Variable", ifelse(l_recycled > 1, "s", ""), " ",
      text_concatenate(names(recycled_variables), enclose = "`"), " had ",
      min_r, ifelse(n_recycled > 1, paste0(" to ", max_r), ""), " value",
      ifelse(max_r > 1, "s", ""), " and ", ifelse(l_recycled > 1, "were", "was"),
      " recycled to match the number of rows in the data."
    ))
  }

  # inform about replaced (overwritten) variables
  if (any(names(dots) %in% column_names) && verbose) {
    overwritten <- intersect(names(dots), column_names)
    insight::format_alert(paste0(
      "The existing variable", ifelse(length(overwritten) > 1, "s ", " "),
      text_concatenate(overwritten, enclose = "`"),
      ifelse(length(overwritten) > 1, " have ", " has "),
      "been modified."
    ))
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

  ## TODO This code is duplicated, taken from the .data.frame method
  ## @etiennebacher can we move this into a separate function and ".dynEval()" still works?

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
