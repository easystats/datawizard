# this function evaluates the select-expression and allows non-standard evaluation

.select_nse <- function(select, data, exclude, ignore_case, regex = FALSE, verbose = FALSE) {
  # check if data argument is valid
  if (is.null(data)) {
    stop(insight::format_message("The 'data' argument must be provided."), call. = FALSE)
  }

  # check data frame input
  if (!is.null(data) && !is.data.frame(data)) {
    data <- try(as.data.frame(data), silent = TRUE)
    if (inherits(data, c("try-error", "simpleError"))) {
      stop(insight::format_message("The 'data' argument must be a data frame, or an object that can be coerced to a data frame."), call. = FALSE)
    }
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
  select <- tryCatch(eval(p), error = function(e) NULL)
  exclude <- tryCatch(eval(p2), error = function(e) NULL)

  # select and exclude might also be a (user-defined) function
  if (inherits(select, "function")) {
    select <- colnames(data)[sapply(data, select)]
  }
  if (inherits(exclude, "function")) {
    exclude <- colnames(data)[sapply(data, exclude)]
  }

  # if select could not be evaluated (because expression "makes no sense")
  # try to evaluate and find select-helpers. In this case, set fixed = FALSE,
  # so we can use grepl()
  if (is.null(select)) {
    evaluated_pattern <- .evaluate_pattern(insight::safe_deparse(p), data, ignore_case = ignore_case)
    select <- evaluated_pattern$pattern
    fixed_select <- evaluated_pattern$fixed
  }
  if (is.null(exclude)) {
    evaluated_pattern <- .evaluate_pattern(insight::safe_deparse(p2), data, ignore_case = ignore_case)
    exclude <- evaluated_pattern$pattern
    fixed_exclude <- evaluated_pattern$fixed
  }


  # seems to be no valid column name or index, so try to grep
  if (isFALSE(fixed_select) || isTRUE(regex)) {
    select <- colnames(data)[grepl(select, colnames(data), ignore.case = ignore_case, perl = TRUE)]
  }
  if (isFALSE(fixed_exclude)) {
    exclude <- colnames(data)[grepl(exclude, colnames(data), ignore.case = ignore_case, perl = TRUE)]
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

.evaluate_pattern <- function(x, data = NULL, ignore_case = FALSE) {
  fixed <- FALSE

  ## TODO once "data_findcols()" is removed, we can remove the checks
  # for "is.null(data)" here, because then data is *always* provided and
  # cannot be NULL. The only place where ".evaluate_pattern()" is called
  # with no data argument is in "data_findcols()".

  # check if negation is requested
  negate <- !is.null(x) && length(x) == 1 && substr(x, 0, 1) == "-"
  # and if so, remove -
  if (negate) {
    x <- substring(x, 2)
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

  # create pattern
  if (is.null(x) && !is.null(data)) {
    # default -----
    pattern <- colnames(data)
    fixed <- TRUE
  } else if (!is.null(data) && !is.null(x) && all(x == "all")) {
    # select all columns -----
    pattern <- colnames(data)
    fixed <- TRUE
  } else if (grepl("^starts_with\\(\"(.*)\"\\)", x)) {
    # select-helper starts_with -----
    if (negate) {
      pattern <- paste0("^(?!", gsub("starts_with\\(\"(.*)\"\\)", "\\1", x), ")")
    } else {
      pattern <- paste0("^", gsub("starts_with\\(\"(.*)\"\\)", "\\1", x))
    }
  } else if (grepl("^ends_with\\(\"(.*)\"\\)", x)) {
    # select-helper end_with -----
    if (negate) {
      pattern <- paste0("(?<!", gsub("ends_with\\(\"(.*)\"\\)", "\\1", x), ")$")
    } else {
      pattern <- paste0(gsub("ends_with\\(\"(.*)\"\\)", "\\1", x), "$")
    }
  } else if (grepl("^contains\\(\"(.*)\"\\)", x)) {
    # select-helper contains -----
    if (negate) {
      pattern <- paste0("^((?!\\Q", gsub("contains\\(\"(.*)\"\\)", "\\1", x), "\\E).)*$")
    } else {
      pattern <- paste0("\\Q", gsub("contains\\(\"(.*)\"\\)", "\\1", x), "\\E")
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
      pattern <- colnames(data)[!sapply(data, function(i) user_function(i))]
    } else {
      pattern <- colnames(data)[sapply(data, user_function)]
    }
    fixed <- TRUE
  } else if (!is.null(data) && grepl("^c\\((.*)\\)$", x)) {
    # here we most likely have a character vector with minus (negate) -----
    cols <- try(eval(parse(text = x)), silent = TRUE)
    if (!inherits(cols, c("try-error", "simpleError")) && !is.null(data)) {
      if (negate) {
        pattern <- setdiff(colnames(data), cols)
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
    cn <- colnames(data)
    if (isTRUE(ignore_case)) {
      from_to <- tolower(from_to)
      cn <- tolower(cn)
    }
    from <- which(cn == from_to[1])
    to <- which(cn == from_to[2])
    if (!length(from)) {
      stop("Could not find variable '", from_to[1], "' in data.", call. = FALSE)
    }
    if (!length(to)) {
      stop("Could not find variable '", from_to[2], "' in data.", call. = FALSE)
    }
    if (negate) {
      pattern <- colnames(data)[setdiff(1:ncol(data), from:to)]
    } else {
      pattern <- colnames(data)[from:to]
    }
    fixed <- TRUE
  } else {
    # everything else
    pattern <- x

    # sanity check - we might have negate with literal variable name
    if (negate && length(pattern) == 1 && !is.null(data) && pattern %in% colnames(data)) {
      pattern <- setdiff(colnames(data), pattern)
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
  # if pattern is formula, simply extract all variables
  if (inherits(pattern, "formula")) {
    pattern <- all.vars(pattern)
  }

  # if numeric, make sure we have valid column indices
  if (is.numeric(pattern)) {
    if (any(pattern < 0)) {
      # select last column(s)
      pattern[pattern < 0] <- sort(ncol(data) + pattern[pattern < 0] + 1)
    }
    pattern <- colnames(data)[intersect(pattern, 1:ncol(data))]
  }

  # special token - select all columns?
  if (length(pattern) == 1 && identical(pattern, "all")) {
    pattern <- colnames(data)
  }

  # check if column names match when all are lowercase
  if (!all(pattern %in% colnames(data)) && isTRUE(ignore_case)) {
    pattern <- colnames(data)[tolower(colnames(data)) %in% tolower(pattern)]
  }

  # check if colnames are in data
  if (!all(pattern %in% colnames(data))) {
    if (isTRUE(verbose)) {
      warning(insight::format_message(
        paste0("Following variable(s) were not found: ", paste0(setdiff(pattern, colnames(data)), collapse = ", "))
      ), call. = FALSE)
    }
    pattern <- intersect(pattern, colnames(data))
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
    for (i in 1:length(pkg)) {
      if (isTRUE(packages$namespace[i])) {
        loadNamespace(pkg[i])
      }
      if (isTRUE(packages$attached[i])) {
        suppressPackageStartupMessages(suppressWarnings(require(pkg[i], quietly = TRUE, character.only = TRUE)))
      }
    }
  }
}
