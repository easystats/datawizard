# this function evaluates the select-expression and allows non-standard evaluation

.select_nse <- function(select, data, exclude, ignore_case, regex = FALSE, verbose = FALSE) {

  .check_data(data)
  columns <- colnames(data)

  # directly return all names if select == exclude == NULL
  if (is.null(substitute(select, env = parent.frame())) &&
      is.null(substitute(exclude, env = parent.frame()))) {
    return(columns)
  }

  selected <- .get_cols_of_("select", data, select, exclude, ignore_case, regex, verbose, columns)
  excluded <- .get_cols_of_("exclude", data, select, exclude, ignore_case, regex, verbose, columns)

  if (length(selected) == 0L) {
    if (length(excluded) == 0L) {
      out <- NULL
    } else {
      out <- setdiff(columns, excluded)
    }
  } else {
    out <- setdiff(selected, excluded)
  }

  out
}



.get_cols_of_ <- function(which, data, select, exclude, ignore_case, regex, verbose, columns) {

  if (which == "select") {
    obj <- substitute(select, env = parent.frame(2L))
  } else {
    obj <- substitute(exclude, env = parent.frame(2L))
  }

  if (is.null(obj)) return(NULL)

  deparsed <- insight::safe_deparse(obj)

  is_select_helper <- FALSE
  is_unquoted_variable <- FALSE
  is_negated <- startsWith(deparsed, "-")
  out <- NULL

  if (is_negated) {
    .check_mixed_numeric(deparsed)
    deparsed <- gsub("^-", "", deparsed)
    deparsed <- gsub('^"|"$', '', deparsed)
    deparsed <- gsub("^'|'$", '', deparsed)
  }

  # Try to evaluate the expression.
  # Should pass: character vector (var names), numeric vector (var positions),
  #              functions, formulas
  # Should fail: select helpers, unquoted variable names
  try_eval <- try(eval(str2lang(deparsed), envir = parent.frame(2L)), silent = TRUE)
  obj_is_not_evaluable <- inherits(try_eval, "try-error")

  # If regex then character is evaluable but should pass through grep()
  if (obj_is_not_evaluable || isTRUE(regex)) {

    is_select_helper <- grepl("^(starts_with|ends_with|contains|regex)", deparsed)
    is_unquoted_variable <- deparsed %in% columns

    if (isTRUE(regex) && is.character(obj)) {
      out <- grep(obj, columns, value = TRUE, ignore.case = ignore_case)
    } else if (is_unquoted_variable) {
      out <- deparsed
    } else if (is_select_helper) {
      pattern <- .extract_pattern_from_select_helper(deparsed)
      if (is.null(pattern)) {
        out <- NULL
      } else {
        pattern_to_eval <- .find_regex_to_eval(deparsed, pattern)
        out <- grep(pattern_to_eval, columns, value = TRUE, ignore.case = ignore_case)
      }
    }

    if (is.null(out)) {
      # if x is an object that was called through a function or a loop, e.g select = i
      # where i = starts_with("Sep"), then we need to find the value of i. This can be
      # done with dynGet() but evaluating starts_with("Sep") errors, either because
      # this function doesn't exist, or because it was already imported via a tidyselect
      # related package and cannot be evaluated outside of a select environment.
      # However, the expression starts_with("Sep") is part of the error message, so
      # we can use this to retrieve "i".
      suppressWarnings({
        # if y is in global environment (e.g i = "Petal" in globenv but i = starts_with("Sep")
        # in the function environment), then evaluation works and we never go through
        # dynGet() whereas it's needed. So we force y to go in the tryCatch().
        deparsed_is_in_globenv <- deparsed %in% ls(globalenv())
        x <- tryCatch(
          dynGet(deparsed, inherits = FALSE, minframe = 0L),
          error = function(e) {
            # if starts_with() et al. don't exist
            fn <- insight::safe_deparse(e$call)

            # if starts_with() et al. come from tidyselect but need to be used in
            # a select environment, then the error doesn't have the same structure.
            if (is.null(fn) && grepl("must be used within a", e$message, fixed = TRUE)) {
              trace <- lapply(e$trace$call, function(x) {
                tmp <- insight::safe_deparse(x)
                if (grepl(paste0("^", .regex_select_helper()), tmp)) {
                  tmp
                }
              })
              fn <- Filter(Negate(is.null), trace)[1]
            }
            # if we actually obtain the select helper call, return it, else return
            # what we already had
            if (length(fn) > 0 && grepl(.regex_select_helper(), fn)) {
              return(fn)
            } else if (!is.null(deparsed)) {
              return(deparsed)
            } else {
              NULL
            }
          }
        )
      })

      # if element is a select helper but the arg in the select helper is not a
      # character, e.g starts_with(i), then we need to find the value of this object
      # by going up all parent.frame() gradually with dynGet()
      if (!is.null(x) && length(x) == 1 &&
          grepl(.regex_select_helper(), x) &&
          !grepl(paste0(.regex_select_helper(), "\\(\"(.*)\"\\)"), x)) {
        obj <- gsub(.regex_select_helper(), "", x)
        obj <- gsub("^\\(", "", obj)
        obj <- gsub("\\)$", "", obj)
        obj_eval <- try(dynGet(obj, inherits = FALSE, minframe = 0L), silent = TRUE)
        x <- gsub(paste0("\\(", obj, "\\)"), paste0("\\(\"", obj_eval, "\"\\)"), x)
      }

      if (length(x) > 1L) {
        deparsed <- paste0("c(\"", paste(x, collapse = "\", \""), "\")")
      } else {
        deparsed <- x
      }

      # Try to evaluate the expression.
      # Should pass: character vector (var names), numeric vector (var positions),
      #              functions, formulas
      # Should fail: select helpers, unquoted variable names
      try_eval <- try(eval(str2lang(deparsed)), silent = TRUE)
      obj_is_not_evaluable <- inherits(try_eval, "try-error")

      # If regex then character is evaluable but should pass through grep()
      if (obj_is_not_evaluable || isTRUE(regex)) {

        is_select_helper <- grepl("^(starts_with|ends_with|contains|regex)", deparsed)
        is_unquoted_variable <- deparsed %in% columns

        if (isTRUE(regex)) {
          out <- grep(deparsed, columns, value = TRUE, ignore.case = ignore_case)
        } else if (is_unquoted_variable) {
          out <- deparsed
        } else if (is_select_helper) {
          pattern <- .extract_pattern_from_select_helper(deparsed)
          pattern_to_eval <- .find_regex_to_eval(deparsed, pattern)
          out <- grep(pattern_to_eval, columns, value = TRUE, ignore.case = ignore_case)
        }

      } else {

        out <- .extract_vars_from_evaluable_object(try_eval, data, columns)

      }
    }

  } else {

    out <- .extract_vars_from_evaluable_object(try_eval, data, columns)

  }

  if (is_negated && !is.null(out)) {
    out <- setdiff(columns, out)
  }

  out
}



.extract_pattern_from_select_helper <- function(x) {

  pattern <- gsub("\\\\", "\\", x, fixed = TRUE)
  pattern <- gsub("^(starts_with|ends_with|contains|regex)\\(", "", pattern)
  pattern <- gsub("\\)$", "", pattern)

  if (!startsWith(pattern, "\"") && !startsWith(pattern, "'")) {
    return(NULL)
  }

  pattern <- strsplit(pattern, "\", ")[[1]]
  pattern <- gsub("'", "", pattern)
  pattern <- gsub('"', "", pattern)

  return(pattern)

}

.find_regex_to_eval <- function(x, pattern) {

  if (grepl("^starts_with\\(", x)) {
    paste0("^(", paste(pattern, collapse = "|") ,")")
  } else if (grepl("^ends_with\\(", x)) {
    paste0("(", paste(pattern, collapse = "|") ,")$")
  } else if (grepl("^contains\\(", x)) {
    paste0("(", paste(pattern, collapse = "|") ,")")
  } else if (grepl("^regex\\(", x)) {
    paste0("(", paste(pattern, collapse = "|") ,")")
  }

}


.check_mixed_numeric <- function(x) {
  num_output <- try(eval(str2lang(x)), silent = TRUE)
  if (any(num_output < 0) && any(num_output > 0)) {
    insight::format_error(
      paste0("You can't mix negative and positive numeric indices in `select` or `exclude`.")
    )
  }
}

.extract_vars_from_evaluable_object <- function(x, data, cols) {
  if (is.numeric(x)) {
    out <- cols[x]
  } else if (is.character(x)) {
    out <- x[x %in% cols]
  } else if (is.function(x)) {
    out <- cols[vapply(data, x, FUN.VALUE = logical(1L))]
  } else if (is.list(x)) {
    out <- names(x)[names(x) %in% cols]
  }
  out
}







# this function checks if the pattern (which should be valid column names now)
# is in the column names of the data, and returns the final column names to select

.evaluated_pattern_to_colnames <- function(pattern, data, ignore_case, verbose, exclude = NULL) {
  # check selected variables
  pattern <- .check_pattern_and_exclude(pattern, data, ignore_case, verbose)
  # check if some variables should be excluded...
  if (!is.null(exclude)) {
    exclude <- .check_pattern_and_exclude(exclude, data, ignore_case, verbose)
    pattern <- setdiff(pattern, exclude)
  }
  pattern
}


# workhorse for .evaluated_pattern_to_colnames()

.check_pattern_and_exclude <- function(pattern, data, ignore_case, verbose) {
  columns <- colnames(data)

  # if pattern is formula, simply extract all variables
  if (inherits(pattern, "formula")) {
    pattern <- all.vars(pattern)
  }

  # if numeric, make sure we have valid column indices
  if (is.numeric(pattern)) {
    if (any(pattern < 0) && any(pattern > 0)) {
      insight::format_error(
        paste0("You can't mix negative and positive numeric indices in `select` or `exclude`.")
      )
    }
    pattern <- columns[pattern]
  }

  # special token - select all columns?
  if (length(pattern) == 1 && identical(pattern, "all")) {
    pattern <- columns
  }

  # check if column names match when all are lowercase
  if (!all(pattern %in% columns) && isTRUE(ignore_case)) {
    pattern <- columns[tolower(columns) %in% tolower(pattern)]
  }

  # check if colnames are in data
  if (!all(pattern %in% columns)) {
    if (isTRUE(verbose)) {
      insight::format_warning(
        paste0("Following variable(s) were not found: ", toString(setdiff(pattern, columns))),
        .misspelled_string(columns, setdiff(pattern, columns), default_message = "Possibly misspelled?")
      )
    }
    pattern <- intersect(pattern, columns)
  }

  pattern
}



.conflicting_packages <- function(packages = NULL) {
  if (is.null(packages)) {
    packages <- "poorman"
  }

  namespace <- vapply(packages, isNamespaceLoaded, FUN.VALUE = logical(1L))
  attached <- paste0("package:", packages) %in% search()
  attached <- stats::setNames(attached, packages)

  for (i in packages) {
    unloadNamespace(i)
  }

  list(package = packages, namespace = namespace, attached = attached)
}


.attach_packages <- function(packages = NULL) {
  if (!is.null(packages)) {
    pkg <- packages$package
    for (i in seq_along(pkg)) {
      if (isTRUE(packages$namespace[i])) {
        loadNamespace(pkg[i])
      }
      if (isTRUE(packages$attached[i])) {
        suppressPackageStartupMessages(suppressWarnings(require(pkg[i], quietly = TRUE, character.only = TRUE)))
      }
    }
  }
}

.regex_select_helper <- function() "(starts\\_with|ends\\_with|col\\_ends\\_with|contains|regex)"

.check_data <- function(data) {
  if (is.null(data)) {
    insight::format_error("The `data` argument must be provided.")
  }
  # check data frame input
  if (!is.null(data)) {
    data <- .coerce_to_dataframe(data)
  }
}

# extract variable names from deparsed select helpers
# e.g "starts_with(\"Sep\", \"Petal\")" -> c("Sep", "Petal")
.extract_vars_from <- function(x, select_helper) {
  tmp <- gsub(paste0(select_helper, "\\(\"(.*)\"\\)"), "\\1", x)
  tmp <- gsub("'", "", tmp, fixed = TRUE)
  tmp <- gsub('"', "", tmp, fixed = TRUE)
  tmp <- strsplit(tmp, ",", fixed = TRUE)[[1]]
  tmp <- insight::trim_ws(tmp)
  if (select_helper == "contains") {
    tmp <- paste0("\\Q", tmp, "\\E")
  }
  paste(tmp, collapse = "|")
}

.is_negated <- function(x) {
  grepl("^(-|\"-|\'-)", x)
}

.remove_minus <- function(x) {
  y <- gsub("^-", "", x)
  y <- gsub("^\'-", "\'", y)
  y <- gsub("^\"-", "\"", y)
  y <- gsub("^\"", "", y)
  y <- gsub("^\'", "", y)
  y <- gsub("\"$", "", y)
  y <- gsub("\'$", "", y)
  y
}
