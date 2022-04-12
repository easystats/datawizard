# this function evaluates the select-expression and allows non-standard evaluation

.select_nse <- function(select, data, exclude, ignore_case, regex = FALSE, verbose = FALSE) {
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
    select <- colnames(data)[grepl(select, colnames(data), ignore.case = ignore_case)]
  }
  if (isFALSE(fixed_exclude)) {
    exclude <- colnames(data)[grepl(exclude, colnames(data), ignore.case = ignore_case)]
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
  if (is.null(x) && !is.null(data)) {
    pattern <- colnames(data)
    fixed <- TRUE
  } else if (!is.null(x) && all(x == "all")) {
    pattern <- colnames(data)
    fixed <- TRUE
  } else if (grepl("^starts_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("^", gsub("starts_with\\(\"(.*)\"\\)", "\\1", x))
  } else if (grepl("^ends_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0(gsub("ends_with\\(\"(.*)\"\\)", "\\1", x), "$")
  } else if (grepl("^contains\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("\\Q", gsub("contains\\(\"(.*)\"\\)", "\\1", x), "\\E")
  } else if (grepl("^matches\\(\"(.*)\"\\)", x)) {
    pattern <- gsub("matches\\(\"(.*)\"\\)", "\\1", x)
  } else if (grepl("^col_starts_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("^", gsub("col_starts_with\\(\"(.*)\"\\)", "\\1", x))
  } else if (grepl("^col_ends_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0(gsub("^ends_with\\(\"(.*)\"\\)", "\\1", x), "$")
  } else if (grepl("^col_contains\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("\\Q", gsub("col_contains\\(\"(.*)\"\\)", "\\1", x), "\\E")
  } else if (grepl("^col_matches\\(\"(.*)\"\\)", x)) {
    pattern <- gsub("col_matches\\(\"(.*)\"\\)", "\\1", x)
  } else if (grepl("^regex\\(\"(.*)\"\\)", x)) {
    pattern <- gsub("regex\\(\"(.*)\"\\)", "\\1", x)
  } else if (grepl("is.numeric()", x, fixed = TRUE)) {
    pattern <- colnames(data)[sapply(data, is.numeric)]
    fixed <- TRUE
  } else if (grepl("is.integer()", x, fixed = TRUE)) {
    pattern <- colnames(data)[sapply(data, is.integer)]
    fixed <- TRUE
  } else if (grepl("is.factor()", x, fixed = TRUE)) {
    pattern <- colnames(data)[sapply(data, is.factor)]
    fixed <- TRUE
  } else if (grepl("is.character()", x, fixed = TRUE)) {
    pattern <- colnames(data)[sapply(data, is.character)]
    fixed <- TRUE
  } else if (grepl("is.logical()", x, fixed = TRUE)) {
    pattern <- colnames(data)[sapply(data, is.logical)]
    fixed <- TRUE
  } else if (!is.null(data) && grepl(":", x, fixed = TRUE)) {
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
    pattern <- colnames(data)[from:to]
    fixed <- TRUE
  } else {
    pattern <- x
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
