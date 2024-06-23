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
#' @param remove_na Logical. If `TRUE`, missing values are omitted from the
#' grouping variable. If `FALSE` (default), missing values are included as a
#' level in the grouping variable.
#' @param ... One or more named expressions that define the new variable name
#' and the function to compute the summary statistic. Example:
#' `mean_sepal_width = mean(Sepal.Width)`. The expression can also be provided
#' as a character string, e.g. `"mean_sepal_width = mean(Sepal.Width)"`. The
#' summary function `n()` can be used to count the number of observations.
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
#'
#' # count observations within groups
#' data_summary(mtcars, observations = n(), by = c("am", "gear"))
#'
#' # first and last observations of "mpg" within groups
#' data_summary(
#'   mtcars,
#'   first = mpg[1],
#'   last = mpg[length(mpg)],
#'   by = c("am", "gear")
#' )
#' @export
data_summary <- function(x, ...) {
  UseMethod("data_summary")
}


#' @export
data_summary.matrix <- function(x, ..., by = NULL, remove_na = FALSE) {
  data_summary(as.data.frame(x), ..., by = by, remove_na = remove_na)
}


#' @export
data_summary.default <- function(x, ...) {
  insight::format_error("`data_summary()` only works for (grouped) data frames and matrices.")
}


#' @rdname data_summary
#' @export
data_summary.data.frame <- function(x, ..., by = NULL, remove_na = FALSE) {
  dots <- eval(substitute(alist(...)))

  # do we have any expression at all?
  if (length(dots) == 0) {
    insight::format_error("No expressions for calculating summary statistics provided.")
  }

  if (is.null(by)) {
    # when we have no grouping, just compute a one-row summary
    summarise <- .process_datasummary_dots(dots, x)
    out <- data.frame(summarise)
    colnames(out) <- vapply(summarise, names, character(1))
  } else {
    # sanity check - is "by" a character string?
    if (!is.character(by)) {
      insight::format_error("Argument `by` must be a character string indicating the name of variables in the data.")
    }
    # is "by" in the data?
    if (!all(by %in% colnames(x))) {
      by_not_found <- by[!by %in% colnames(x)]
      insight::format_error(
        paste0(
          "Variable",
          ifelse(length(by_not_found) > 1, "s ", " "),
          text_concatenate(by_not_found, enclose = "\""),
          " not found in the data."
        ),
        .misspelled_string(colnames(x), by_not_found, "Possibly misspelled?")
      )
    }
    # split data, add NA levels, if requested
    l <- lapply(x[by], function(i) {
      if (remove_na || !anyNA(i)) {
        i
      } else {
        addNA(i)
      }
    })
    split_data <- split(x, l, drop = TRUE)
    out <- lapply(split_data, function(s) {
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
  # sort data
  out <- data_arrange(out, select = by)
  # data attributes
  class(out) <- c("dw_data_summary", "data.frame")
  rownames(out) <- NULL
  out
}


#' @export
data_summary.grouped_df <- function(x, ..., by = NULL, remove_na = FALSE) {
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
  data_summary(x, ..., by = by, remove_na = remove_na)
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
      # a <- "mean_sepwid = mean(Sepal.Width)"
      # data_summary(iris, a, by = "Species")
      # or as character vector, e.g.
      # data_summary(iris, c("var_a = mean(Sepal.Width)", "var_b = sd(Sepal.Width)"))
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

  # check for correct length of output - must be a single value!
  if (any(lengths(out) != 1)) {
    insight::format_error(
      paste0(
        "Each expression must return a single value. Following expression",
        ifelse(sum(lengths(out) != 1) > 1, "s", " "),
        " returned more than one value: ",
        text_concatenate(vapply(dots[lengths(out) != 1], insight::safe_deparse, character(1)), enclose = "\"")
      )
    )
  }

  out
}


# methods ----------------------------------------------------------------------

#' @export
print.dw_data_summary <- function(x, ...) {
  if (nrow(x) == 0) {
    cat("No matches found.\n")
  } else {
    cat(insight::export_table(x, missing = "<NA>", ...))
  }
}
