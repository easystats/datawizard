# this function evaluates the select-expression and allows non-standard evaluation

.select_nse <- function(select, data, exclude, ignore_case, regex = FALSE, verbose = FALSE) {

  .check_data(data)
  columns <- colnames(data)

  # directly return all names if select == exclude == NULL
  if (is.null(substitute(select, env = parent.frame())) &&
      is.null(substitute(exclude, env = parent.frame()))) {
    return(columns)
  }

  selected <- .get_cols_of_("select", select, exclude, ignore_case, regex, verbose, columns)
  excluded <- .get_cols_of_("exclude", select, exclude, ignore_case, regex, verbose, columns)

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



.get_cols_of_ <- function(which, select, exclude, ignore_case, regex, verbose, columns) {

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

  } else {

    if (is.numeric(try_eval)) {
      out <- columns[try_eval]
    } else if (is.character(try_eval)) {
      out <- try_eval[try_eval %in% columns]
    } else if (is.function(try_eval)) {
      out <- columns[vapply(data, try_eval, FUN.VALUE = logical(1L))]
    } else if (is.list(try_eval)) {
      out <- names(try_eval)[names(try_eval) %in% columns]
    }

  }

  if (isTRUE(regex)) {
    out <- grep(obj, columns, value = TRUE, ignore.case = ignore_case)
  } else if (is_unquoted_variable) {
    out <- deparsed
  } else if (is_select_helper) {
    pattern <- .extract_pattern_from_select_helper(deparsed)
    pattern_to_eval <- .find_regex_to_eval(deparsed, pattern)
    out <- grep(pattern_to_eval, columns, value = TRUE, ignore.case = ignore_case)
  }

  if (is_negated) {
    out <- setdiff(columns, out)
  }

  out
}



.extract_pattern_from_select_helper <- function(x) {

  pattern <- gsub("\\\\", "\\", x, fixed = TRUE)
  pattern <- gsub("^(starts_with|ends_with|contains|regex)\\(", "", pattern)
  pattern <- gsub("\\)$", "", pattern)
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
