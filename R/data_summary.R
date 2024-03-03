#' @title Summarize data
#' @name data_summary
#'
#' @description This function can be used to compute summary statistics for a
#' data frame or a matrix.
#'
#' @param x A (grouped) data frame.
#' @param by Optional character string, indicating the name of a variable in `x`.
#' If supplied, the data will be split by this variable and summary statistics
#' will be computed for each group.
#' @param ... One or more named expressions that define the new variable name
#' and the function to compute the summary statistic. Example:
#' `mean_sepal_width = mean(Sepal.Width)`. The expression can also be provided
#' as a character string, e.g. `"mean_sepal_width = mean(Sepal.Width)"`.
#'
#' @return A data frame with the requested summary statistics.
#'
#' @examples
#' data(iris)
#' data_summary(iris, MW = mean(Sepal.Width), SD = sd(Sepal.Width))
#' data_summary(
#'   iris,
#'   MW = mean(Sepal.Width),
#'   SD = sd(Sepal.Width),
#'   by = "Species"
#' )
#'
#' # same as
#' d <- data_group(iris, "Species")
#' data_summary(d, MW = mean(Sepal.Width), SD = sd(Sepal.Width))
#'
#' # multiple groups
#' data(mtcars)
#' data_summary(mtcars, MW = mean(mpg), SD = sd(mpg), by = c("am", "gear"))
#'
#' # expressions can also be supplied as character strings
#' data_summary(mtcars, "MW = mean(mpg)", "SD = sd(mpg)", by = c("am", "gear"))
#' @export
data_summary <- function(x, ...) {
  UseMethod("data_summary")
}


#' @export
data_summary.matrix <- function(x, ..., by = NULL) {
  data_summary(as.data.frame(x), ..., by = by)
}


#' @export
data_summary.default <- function(x, ...) {
  insight::format_error("`data_summary()` only works for (grouped) data frames and matrices.")
}


#' @rdname data_summary
#' @export
data_summary.data.frame <- function(x, ..., by = NULL) {
  dots <- eval(substitute(alist(...)))

  if (is.null(by)) {
    # when we have no grouping, just compute a one-row summary
    summarise <- .process_datasummary_dots(dots, x)
    out <- data.frame(summarise)
    colnames(out) <- vapply(summarise, names, character(1))
  } else {
    # split data
    splitted_data <- split(x, x[by])
    out <- lapply(splitted_data, function(s) {
      # no data for combination? Return NULL
      if (nrow(s) == 0) {
        return(NULL)
      }
      # summarize data
      summarise <- .process_datasummary_dots(dots, s)
      # coerce to data frame
      summarised_data <- data.frame(summarise)
      # bind grouping-variables and values
      summarised_data <- cbind(s[1, by], summarised_data)
      # make sure we have proper column names
      colnames(summarised_data) <- c(by, vapply(summarise, names, character(1)))
      summarised_data
    })
    out <- do.call(rbind, out)
  }
  class(out) <- "data.frame"
  rownames(out) <- NULL
  out
}


#' @export
data_summary.grouped_df <- function(x, ..., by = NULL) {
  # extract group variables
  grps <- attr(x, "groups", exact = TRUE)
  group_variables <- data_remove(grps, ".rows")
  # if "by" is not supplied, use group variables
  if (is.null(by)) {
    by <- colnames(group_variables)
  }
  # remove information specific to grouped df's
  attr(x, "groups") <- NULL
  class(x) <- "data.frame"
  data_summary(x, ..., by = by)
}


# helper -----------------------------------------------------------------------

.process_datasummary_dots <- function(dots, data) {
  out <- NULL
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

    out <- lapply(seq_along(dots), function(i) {
      new_variable <- .get_new_dots_variable(dots, i, data)
      stats::setNames(new_variable, names(dots)[i])
    })
  }

  out
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

  new_variable
}
