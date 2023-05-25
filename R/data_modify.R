#' Create new variables in a data frame
#'
#' Create new variables in a data frame. Unlike `base::transform()`, `data_modify()`
#' can be used on grouped data frames, and newly created variables can be directly
#' used.
#'
#' @param data The data frame, for which new variables should be created.
#' @param ... A sequence of named expressions, where the left-hand side refers
#' to the name of the new variable, while the right-hand side represent the
#' values of the new variable. The right-hand side can also be represented
#' as character string. See 'Examples'.
#' @param verbose Toggle messages.
#'
#' @note This function is still experimental and well tested for interactive
#' use. There might be corner cases, e.g. when called from inside functions
#' or similar, where `data_modify()` does not yet work. Please carefully
#' check your results before using this function, say, in package code.
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
#' @export
data_modify <- function(data, ...) {
  UseMethod("data_modify")
}

#' @export
data_modify.default <- function(data, ...) {
  insight::format_error("`data` must be a data frame.")
}

#' @export
data_modify.data.frame <- function(data, ..., verbose = TRUE) {
  dots <- match.call(expand.dots = FALSE)$`...`
  for (i in seq_along(dots)) {
    # iterate expressions for new variables
    symbol <- dots[[i]]

    # expression is given as character, e.g.
    # data_modify(iris, "double_SepWidth = 2 * Sepal.Width")
    if (is.character(symbol)) {
      symbol <- str2lang(symbol)
    } else {
      # expression is given as character vector, e.g.
      # a <- "double_SepWidth = 2 * Sepal.Width"
      # data_modify(iris, a)
      character_symbol <- tryCatch(.dynEval(symbol), error = function(e) NULL)
      if (is.character(character_symbol)) {
        # turn value from character vector into expression
        symbol <- str2lang(.dynEval(symbol))
        # make sure "dots" still has names
        names(dots)[i] <- insight::safe_deparse(symbol[[2]])
      }
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
