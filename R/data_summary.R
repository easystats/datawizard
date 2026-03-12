#' @title Summarize data
#' @name data_summary
#'
#' @description This function can be used to compute summary statistics for a
#' data frame or a matrix.
#'
#' @param x A (grouped) data frame.
#' @param by Optional character string, indicating the names of one or more
#' variables in the data frame. If supplied, the data will be split by these
#' variables and summary statistics will be computed for each group.
#' @param remove_na Logical. If `TRUE`, missing values are omitted from the
#' grouping variable. If `FALSE` (default), missing values are included as a
#' level in the grouping variable.
#' @param suffix Optional, suffixes to be added to the new variable names,
#' especially useful when a function returns several values (e.g. `quantile()`).
#' Can be:
#' * a character vector: all expressions in `...` must return the same number
#'    of values as elements in `suffix`.
#' * a list of named character vectors: the names of elements in `suffix` must
#'    match the names of the expressions. It is also allowed to specify suffixes
#'    for selected expressions only.
#'
#' The new column names are a combination of the left-hand side (i.e.,
#' the name) of the expression and the related suffixes. If `suffix = NULL` (the
#' default), and a summary expression returns multiple values, either the names
#' of the returned values (if any) or automatically numbered suffixes such as
#' `_1`, `_2`, etc. are used. See 'Examples'.
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
#'
#' # allow more than one-column-summaries for expressions
#' d <- data.frame(
#'   x = rnorm(100, 1, 1),
#'   y = rnorm(100, 2, 2),
#'   groups = rep(1:4, each = 25)
#' )
#'
#' # since we have multiple columns for one expression, the names of the
#' # returned summary results are used as suffix by default
#' data_summary(
#'   d,
#'   quant_x = quantile(x, c(0.25, 0.75)),
#'   mean_x = mean(x),
#'   quant_y = quantile(y, c(0.25, 0.5, 0.75))
#' )
#'
#' # if a summary function, like `fivenum()`, returns no named vector, suffixes
#' # are automatically numbered
#' data_summary(
#'   d,
#'   quant_x = quantile(x, c(0.25, 0.75)),
#'   mean_x = mean(x),
#'   fivenum_y = fivenum(y)
#' )
#'
#' # specify column suffix for expressions, matching by names
#' data_summary(
#'   d,
#'   quant_x = quantile(x, c(0.25, 0.75)),
#'   mean_x = mean(x),
#'   quant_y = quantile(y, c(0.25, 0.5, 0.75)),
#'   suffix = list(quant_y = c("_Q1", "_Q2", "_Q3"))
#' )
#'
#' # name multiple expression suffixes, grouped by variable
#' data_summary(
#'   d,
#'   quant_x = quantile(x, c(0.25, 0.75)),
#'   mean_x = mean(x),
#'   quant_y = quantile(y, c(0.25, 0.5, 0.75)),
#'   suffix = list(quant_x = c("Q1", "Q3"), quant_y = c("_Q1", "_Q2", "_Q3")),
#'   by = "groups"
#' )
#'
#' @export
data_summary <- function(x, ...) {
  UseMethod("data_summary")
}


#' @export
data_summary.matrix <- function(
  x,
  ...,
  by = NULL,
  remove_na = FALSE,
  suffix = NULL
) {
  data_summary(
    as.data.frame(x),
    ...,
    by = by,
    remove_na = remove_na,
    suffix = suffix
  )
}


#' @export
data_summary.default <- function(x, ...) {
  insight::format_error(
    "`data_summary()` only works for (grouped) data frames and matrices."
  )
}


#' @rdname data_summary
#' @export
data_summary.data.frame <- function(
  x,
  ...,
  by = NULL,
  remove_na = FALSE,
  suffix = NULL
) {
  dots <- eval(substitute(alist(...)))

  # do we have any expression at all?
  if (length(dots) == 0) {
    insight::format_error(
      "No expressions for calculating summary statistics provided."
    )
  }

  if (is.null(by)) {
    # when we have no grouping, just compute a one-row summary
    summarise <- .process_datasummary_dots(dots, x, suffix)
    # coerce to data frame
    out <- as.data.frame(t(summarise))
    colnames(out) <- names(summarise)
  } else {
    # sanity check - is "by" a character string?
    if (!is.character(by)) {
      insight::format_error(
        "Argument `by` must be a character string indicating the name of variables in the data."
      )
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
      summarise <- .process_datasummary_dots(dots, s, suffix)
      # coerce to data frame
      summarised_data <- as.data.frame(t(summarise))
      # bind grouping-variables and values
      summarised_data <- cbind(s[1, by], summarised_data)
      # make sure we have proper column names
      colnames(summarised_data) <- c(by, names(summarise))
      summarised_data
    })
    # check for correct number of columns. If one expression returns different
    # number of values (which now means, we have different number of columns
    # to bind) for each group, tell user
    if (!all(lengths(out) == lengths(out)[1])) {
      insight::format_error(
        "Each expression must return the same number of values for each group. Some of the expressions seem to return varying numbers of values."
      )
    }
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
data_summary.grouped_df <- function(
  x,
  ...,
  by = NULL,
  remove_na = FALSE,
  suffix = NULL
) {
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
  data_summary(x, ..., by = by, remove_na = remove_na, suffix = suffix)
}


# helper -----------------------------------------------------------------------

.process_datasummary_dots <- function(dots, data, suffix = NULL) {
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
          insight::format_error(
            "You cannot mix string and literal representation of expressions."
          )
        }
      }
      # expression is given as character string, e.g.
      # a <- "mean_sepwid = mean(Sepal.Width)"
      # data_summary(iris, a, by = "Species")
      # or as character vector, e.g.
      # data_summary(iris, c("var_a = mean(Sepal.Width)", "var_b = sd(Sepal.Width)"))
      character_symbol <- tryCatch(.dynEval(dots[[1]]), error = function(e) {
        NULL
      })
      # do we have a character vector? Then we can proceed
      if (is.character(character_symbol)) {
        dots <- lapply(character_symbol, function(s) {
          # turn value from character vector into expression
          str2lang(.dynEval(s))
        })
        names(dots) <- vapply(
          dots,
          function(n) insight::safe_deparse(n[[2]]),
          character(1)
        )
      }
    }

    # sanity check: check the input for the `suffix` argument
    # `suffix` can be NULL, or must be a (named) list
    if (!is.null(suffix)) {
      # if `suffix` is a character vector, we transform it into a list,
      # matching the names of the expressions
      if (is.character(suffix)) {
        suffix <- rep(list(suffix), length(dots))
        names(suffix) <- names(dots)
      }
      # no list? error
      if (!is.list(suffix)) {
        insight::format_error(
          "Argument `suffix` must be a list of (named) character vectors, where the names match the names of the expressions, e.g.:",
          paste0(
            "`suffix = list(",
            names(dots)[1],
            " = c(\"_suffix1\", \"_suffix2\")`."
          )
        )
      }
      # not all elements named? error
      if (!length(which(nzchar(names(suffix), keepNA = TRUE)))) {
        insight::format_error("All elements of `suffix` must have names.")
      }
      # names of suffix do not match names of expressions? error
      if (!all(names(suffix) %in% names(dots))) {
        wrong_name <- which(!names(suffix) %in% names(dots))[1]
        insight::format_error(
          paste0(
            "Names of `suffix` must match the names of the expressions. Suffix `",
            names(suffix)[wrong_name],
            "` has no corresponding expression."
          )
        )
      }
      # identical suffixes for one expression? error
      identical_suffix <- vapply(
        suffix,
        function(i) insight::n_unique(i) != length(i),
        logical(1)
      )
      if (any(identical_suffix)) {
        insight::format_error(
          paste0(
            "All suffixes for a single expression must be unique. Suffix for element `",
            names(identical_suffix)[which(identical_suffix)][1],
            "` has duplicate values."
          )
        )
      }
    }

    out <- lapply(seq_along(dots), function(i) {
      new_variable <- .get_new_dots_variable(dots, i, data)
      # check special case here - we want bayestestR::ci to work with
      # data summary, to easily create CIs for, say, posterior draws
      if (inherits(new_variable, c("bayestestR_ci", "bayestestR_eti"))) {
        stats::setNames(new_variable, c("CI", "CI_low", "CI_high"))
      } else {
        # init
        current_suffix <- NULL
        # find matches and set use suffix if found
        matching_names <- which(names(suffix) == names(dots)[i])
        # either use suffixes based on matching names, or try to extract
        # names from the returned summary expression (saved in "new_variable"),
        # if the summary function returned a named vector
        if (length(matching_names) > 0) {
          current_suffix <- suffix[[matching_names]]
        } else if (
          length(new_variable) > 1 &&
            all(nzchar(names(new_variable), keepNA = TRUE))
        ) {
          current_suffix <- names(new_variable)
        }
        # if we don't have suffixes for multiple columns, but expression
        # returns multiple columns, we get NA column names - we use
        # automatically numbered suffixes in this case
        if (is.null(current_suffix) && length(new_variable) > 1) {
          current_suffix <- paste0("_", seq_along(new_variable))
        }

        # if number of suffixes does not match the number of returned values
        # by the expression, error
        if (
          !is.null(current_suffix) &&
            length(current_suffix) != length(new_variable)
        ) {
          insight::format_error(
            paste0(
              "Argument `suffix` must have the same length as the result of the corresponding summary expression. `suffix` has ",
              length(current_suffix),
              " elements (",
              text_concatenate(current_suffix, enclose = "`"),
              ") for the expression `",
              insight::safe_deparse(dots[[i]]),
              "`, which returned ",
              length(new_variable),
              " values."
            )
          )
        }
        stats::setNames(new_variable, paste0(names(dots)[i], current_suffix))
      }
    })
  }

  unlist(out)
}


# methods ----------------------------------------------------------------------

#' @export
print.dw_data_summary <- function(x, ...) {
  if (nrow(x) == 0) {
    cat("No matches found.\n")
  } else {
    if (all(c("CI", "CI_low", "CI_high") %in% colnames(x))) {
      ci <- insight::format_table(x[c("CI", "CI_low", "CI_high")], ...)
      x$CI <- x$CI_low <- x$CI_high <- NULL
      x <- cbind(x, ci)
    }
    cat(insight::export_table(x, missing = "<NA>", ...))
  }
}
