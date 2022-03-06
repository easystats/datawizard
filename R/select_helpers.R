# this function looks for function-name-patterns (select-helpers) and
# returns the regular expression that mimics the behaviour of that select-helper

.evaluate_pattern <- function(x, data = NULL, ignore_case = FALSE) {
  fixed <- FALSE
  if (grepl("^starts_with\\(\"(.*)\"\\)", x)) {
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

.evaluated_pattern_to_colnames <- function(pattern, data, ignore_case, verbose) {
  # if numeric, make sure we have valid column indices
  if (is.numeric(pattern)) {
    pattern <- colnames(data)[intersect(pattern, 1:ncol(data))]
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
      ))
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
