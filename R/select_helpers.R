# this function evaluates the select-expression and allows non-standard evaluation

.select_nse <- function(select, data, exclude, ignore_case, regex = FALSE, verbose = FALSE) {
  .check_data(data)

  columns <- colnames(data)

  # directly return all names if select == exclude == NULL
  if (is.null(substitute(select, env = parent.frame())) &&
    is.null(substitute(exclude, env = parent.frame()))) {
    return(columns)
  }

  fixed_select <- TRUE
  fixed_exclude <- TRUE
  # avoid conflicts
  conflicting_packages <- .conflicting_packages("poorman")

  # in case pattern is a variable from another function call...
  p <- try(eval(select), silent = TRUE)
  p2 <- try(eval(exclude), silent = TRUE)
  if (inherits(p, c("try-error", "simpleError"))) {
    p <- substitute(select, env = parent.frame())
  }
  if (inherits(p2, c("try-error", "simpleError"))) {
    p2 <- substitute(exclude, env = parent.frame())
  }

  # check if pattern is a function like "starts_with()"
  is_p_expr_not_in_globenv <- !is.null(p) && !(insight::safe_deparse(p) %in% ls(globalenv()))
  is_p2_expr_not_in_globenv <- !is.null(p2) && !insight::safe_deparse(p2) %in% ls(globalenv())
  if (is_p_expr_not_in_globenv) {
    select <- tryCatch(eval(p), error = function(e) NULL)
  } else {
    select <- NULL
  }
  if (is_p2_expr_not_in_globenv) {
    exclude <- tryCatch(eval(p2), error = function(e) NULL)
  } else {
    exclude <- NULL
  }

  # select and exclude might also be a (user-defined) function
  if (inherits(select, "function")) {
    select <- columns[sapply(data, select)]
  }
  if (inherits(exclude, "function")) {
    exclude <- columns[sapply(data, exclude)]
  }

  # if select could not be evaluated (because expression "makes no sense")
  # try to evaluate and find select-helpers. In this case, set fixed = FALSE,
  # so we can use grepl()
  if (is.null(select)) {
    evaluated_pattern <- .evaluate_pattern(insight::safe_deparse(p), data, ignore_case = ignore_case, y = p)
    select <- evaluated_pattern$pattern
    fixed_select <- evaluated_pattern$fixed
  }
  if (is.null(exclude)) {
    evaluated_pattern <- .evaluate_pattern(insight::safe_deparse(p2), data, ignore_case = ignore_case, y = p2)
    exclude <- evaluated_pattern$pattern
    fixed_exclude <- evaluated_pattern$fixed
  }


  # seems to be no valid column name or index, so try to grep
  if (isFALSE(fixed_select) || isTRUE(regex)) {
    select <- columns[grepl(select, columns, ignore.case = ignore_case, perl = TRUE)]
  }
  if (isFALSE(fixed_exclude)) {
    exclude <- columns[grepl(exclude, columns, ignore.case = ignore_case, perl = TRUE)]
  }
  # if exclude = NULL, we want to exclude 0 variables, not all of them
  if (!inherits(exclude, "formula") && length(exclude) == ncol(data)) {
    exclude <- NULL
  }

  # load again
  .attach_packages(conflicting_packages)

  # return valid column names, based on pattern
  .evaluated_pattern_to_colnames(select, data, ignore_case, verbose = verbose, exclude)
}


# this function looks for function-name-patterns (select-helpers) and
# returns the regular expression that mimics the behaviour of that select-helper

.evaluate_pattern <- function(x, data = NULL, ignore_case = FALSE, y) {
  fixed <- FALSE
  columns <- colnames(data)

  # check if negation is requested
  negate <- !is.null(x) && length(x) == 1 && .is_negated(x)
  # and if so, remove -
  if (negate) {
    x <- .remove_minus(x)
  }

  # deal with aliases
  if (!is.null(x) && length(x) == 1 && grepl("^col_(starts|ends|contains)(.*)\\)$", x)) {
    x <- substring(x, 5)
  }

  # remove parentheses for functions
  if (!is.null(x) && length(x) == 1 && substr(x, nchar(x) - 1, nchar(x)) == "()") {
    x <- substr(x, 0, nchar(x) - 2)
  }

  # is it a function? usually, this is already done in ".select_nse()", unless
  # the user negates a function with "-", like "-is.numeric". In this case,
  # we apply the function here again.
  user_function <- NULL
  if (!is.null(x)) {
    user_function <- try(get(x, envir = parent.frame()), silent = TRUE)
    if (inherits(user_function, c("try-error", "simpleError")) || !inherits(user_function, "function")) {
      user_function <- NULL
    }
  }

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
    y_is_in_globenv <- insight::safe_deparse(y) %in% ls(globalenv())
    y_is_evaluable <- if (isTRUE(y_is_in_globenv)) {
      FALSE
    } else {
      # remove minus sign if this is a custom function, e.g -testfun -> testfun
      !inherits(try(eval(str2lang(gsub("^-", "", insight::safe_deparse(y)))), silent = TRUE), "try-error")
    }
    if (!is.null(x) && !y_is_evaluable) {
      x <- tryCatch(
        dynGet(x, inherits = FALSE, minframe = 0L),
        error = function(e) {
          # if starts_with() et al. don't exist
          fn <- insight::safe_deparse(e$call)

          # if starts_with() et al. come from tidyselect but need to be used in
          # a select environment, then the error doesn't have the same structure.
          if (is.null(fn) && grepl("must be used within a", e$message)) {
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
          } else if (!is.null(x)) {
            return(x)
          } else {
            NULL
          }
        }
      )
    }
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

  # create pattern
  if (is.null(x) && !is.null(data)) {
    # default -----
    pattern <- columns
    fixed <- TRUE
  } else if (!is.null(data) && !is.null(x) && all(x == "all")) {
    # select all columns -----
    pattern <- columns
    fixed <- TRUE
  } else if (all(x %in% columns)) {
    if (negate) {
      pattern <- setdiff(columns, x)
    } else {
      pattern <- x
    }
    fixed <- TRUE
  } else if (grepl("^starts_with\\(\"(.*)\"\\)", x)) {
    # select-helper starts_with -----
    x <- .extract_vars_from(x, "starts_with")
    if (negate) {
      pattern <- paste0("^(?!", x, ")")
    } else {
      pattern <- paste0("^(", x, ")")
    }
  } else if (grepl("^ends_with\\(\"(.*)\"\\)", x)) {
    # select-helper end_with -----
    x <- .extract_vars_from(x, "ends_with")
    if (negate) {
      pattern <- paste0("(?<!", x, ")$")
    } else {
      pattern <- paste0(x, "$")
    }
  } else if (grepl("^contains\\(\"(.*)\"\\)", x)) {
    # select-helper contains -----
    x <- .extract_vars_from(x, "contains")
    if (negate) {
      pattern <- paste0("^((?!", x, ").)*$")
    } else {
      pattern <- x
    }
  } else if (grepl("^matches\\(\"(.*)\"\\)", x)) {
    # matches is an alias for regex -----
    pattern <- gsub("matches\\(\"(.*)\"\\)", "\\1", x)
  } else if (grepl("^regex\\(\"(.*)\"\\)", x)) {
    # regular expression -----
    pattern <- gsub("regex\\(\"(.*)\"\\)", "\\1", x)
  } else if (!is.null(data) && !is.null(user_function)) {
    # function -----
    if (negate) {
      pattern <- columns[!sapply(data, function(i) user_function(i))]
    } else {
      pattern <- columns[sapply(data, user_function)]
    }
    fixed <- TRUE
  } else if (!is.null(data) && grepl("^c\\((.*)\\)$", x)) {
    # here we most likely have a character vector with minus (negate) -----
    cols <- try(eval(parse(text = x)), silent = TRUE)
    if (!inherits(cols, c("try-error", "simpleError")) && !is.null(data)) {
      if (negate) {
        pattern <- setdiff(columns, cols)
      } else {
        pattern <- cols
      }
      fixed <- TRUE
    } else {
      pattern <- x
    }
  } else if (!is.null(data) && grepl(":", x, fixed = TRUE)) {
    # range -----
    from_to <- unlist(strsplit(x, ":", fixed = TRUE))
    cn <- columns
    if (isTRUE(ignore_case)) {
      from_to <- tolower(from_to)
      cn <- tolower(cn)
    }
    from <- which(cn == from_to[1])
    to <- which(cn == from_to[2])
    if (!length(from)) {
      # guess the misspelled column
      insight::format_error(
        paste0("Could not find variable \"", from_to[1], "\" in data."),
        .misspelled_string(cn, from_to[1], default_message = "Possibly misspelled?")
      )
    }
    if (!length(to)) {
      # guess the misspelled column
      insight::format_error(
        paste0("Could not find variable \"", from_to[2], "\" in data."),
        .misspelled_string(cn, from_to[2], default_message = "Possibly misspelled?")
      )
    }
    if (negate) {
      pattern <- columns[setdiff(seq_len(ncol(data)), from:to)]
    } else {
      pattern <- columns[from:to]
    }
    fixed <- TRUE
  } else {
    # everything else
    pattern <- x

    # sanity check - we might have negate with literal variable name
    if (negate && length(pattern) == 1 && !is.null(data) && pattern %in% columns) {
      pattern <- setdiff(columns, pattern)
      fixed <- TRUE
    }
  }

  list(pattern = gsub("\\\\", "\\", pattern, fixed = TRUE), fixed = fixed)
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
        paste0("Following variable(s) were not found: ", paste0(setdiff(pattern, columns), collapse = ", ")),
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

  namespace <- sapply(packages, isNamespaceLoaded)
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
  tmp <- strsplit(tmp, ",")[[1]]
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
