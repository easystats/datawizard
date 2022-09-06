# this function evaluates the select-expression and allows non-standard evaluation

.select_nse <- function(select, data, exclude, ignore_case, regex = FALSE, verbose = FALSE) {

  .check_data(data)
  cols <- names(data)

  # avoid conflicts for starts_with(), etc.
  # (has to be in this order, otherwise error because e.g tidyselect is imported
  # in tidyr)
  conflicting_packages <- .conflicting_packages(c("poorman", "tidyr", "dplyr", "tidyselect"))

  # Selected vars ---------------------------------------------

  select_deparsed <- insight::safe_deparse(substitute(select, env = parent.frame(1L)))
  select_is_negated <- FALSE
  selected <- NULL

  if (!.is_numeric(select_deparsed) &&
      !.is_object_in_env(select_deparsed) &&
      .is_negated(select_deparsed)) {
    select_deparsed <- gsub("^-", "", select_deparsed)
    select_is_negated <- TRUE
    tmp <- try(eval(parse(text = select_deparsed)), silent = TRUE)
    if (!inherits(tmp, "try-error")) {
      select <- tmp
    } else if(!.is_select_helper(select_deparsed)) {
      select <- select_deparsed
    }
  }

  if (.is_object_in_env(select_deparsed)) {
    tmp <- try(get(select_deparsed, envir = parent.frame(2L)), silent = TRUE)
    if (!inherits(tmp, "try-error")) {
      select <- tmp
    }
  }

  # Deal with range of variable names, e.g var1:var2
  if (.is_range_character(select_deparsed)) {
    from_to <- unlist(strsplit(select_deparsed, ":", fixed = TRUE))
    bound1 <- grep(paste0("^", from_to[1], "$"), cols, ignore.case = ignore_case)
    bound2 <- grep(paste0("^", from_to[2], "$"), cols, ignore.case = ignore_case)
    select_deparsed <- cols[bound1:bound2]
    select <- select_deparsed
  }

  # Deal with functions with parenthesis, e.g is.numeric()
  if (length(select_deparsed) == 1 && .is_function(select_deparsed)) {
    select_deparsed <- gsub("\\(\\)$", "", select_deparsed)
    select <- get(select_deparsed, envir = parent.frame())
  }

  # NSE e.g input is Petal.Length
  if (all(select_deparsed %in% cols)) {
    selected <- select_deparsed
    select <- select_deparsed # avoid evaluation below because it would return
                              # non-evaluable
  }

  select_is_evaluable <- !inherits(try(eval(select), silent = TRUE), "try-error")

  if (select_is_evaluable) {
    if (is.null(select)) {
      selected <- names(data)
    } else if (is.character(select)) {
      if (regex) {
        selected <- cols[which(grepl(select, cols, ignore.case = ignore_case))]
      } else {
        selected <- vector(length = length(select))
        for (i in seq_along(select)) {
          tmp <- cols[grep(paste0("^", select[i], "$"), cols, ignore.case = ignore_case)]
          if (length(tmp) > 0) {
            selected[i] <- tmp
          }
        }
      }
    } else if (is.numeric(select)) {
      original_select <- select
      # negative values in "select" mean "start from the right"
      tmp_cols <- cols
      selected <- vector(length = length(select))
      for (i in seq_along(select)) {
        selected[i] <- if (select[i] > 0) {
          tmp_cols[select[i]]
        } else if (select[i] < 0) {
          select[i] <- select[i] * -1
          tmp_cols <- rev(tmp_cols)
          to_return <- tmp_cols[select[i]]
          tmp_cols <- rev(tmp_cols) # back in the original order
          to_return
        }
      }
      if (all(original_select < 0)) selected <- rev(selected)
    } else if (inherits(select, "formula")) {
      selected <- all.vars(select)
    } else if (inherits(select, "function")) {
      selected <- cols[sapply(data, select)]
    } else if (inherits(select, "call")) {
      tmp <- try(cols[eval(select)], silent = TRUE)
      if (!inherits(tmp, "try-error")) {
        selected <- tmp
      }
    }
  }

  if (!select_is_evaluable) {
    match_info <- .get_match(select_deparsed)
    match <- match_info$out
    if (match_info$accessed_via_envir) {
      select_deparsed <- match_info$deparsed
    }
    # each backslash in the original expression gets duplicated because
    # of insight::safe_deparse, so we de-duplicate them.
    # Hardcoded workaround so not ideal but I don't know how to have a more
    # general solution
    match <- gsub("\\\\\\\\", "\\\\", match)
    if (grepl("^starts_with\\(", select_deparsed)) {
      selected <- .starts_with(match, data, ignore_case = ignore_case)
    }
    if (grepl("^ends_with\\(|^col_ends_with\\(", select_deparsed, perl = TRUE)) {
      selected <- .ends_with(match, data, ignore_case = ignore_case)
    }
    if (grepl("^contains\\(", select_deparsed)) {
      selected <- .contains(match, data, ignore_case = ignore_case)
    }
    if (grepl("^regex\\(", select_deparsed)) {
      selected <- .contains(match, data, ignore_case = ignore_case)
    }
  }

  if (select_is_negated) {
    selected <- setdiff(cols, selected)
  }

  # excluded vars ---------------------------------------------

  exclude_deparsed <- insight::safe_deparse(substitute(exclude, env = parent.frame()))
  exclude_is_negated <- FALSE

  if (!.is_numeric(exclude_deparsed) && .is_negated(exclude_deparsed)) {
    exclude_deparsed <- gsub("^-", "", exclude_deparsed)
    exclude_is_negated <- TRUE
  }

  if (.is_object_in_env(exclude_deparsed)) {
    tmp <- try(get(exclude_deparsed, envir = parent.frame(2L)), silent = TRUE)
    if (!inherits(tmp, "try-error")) {
      exclude <- tmp
    }
  }

  # Deal with range of variable names, e.g var1:var2
  if (.is_range_character(exclude_deparsed)) {
    from_to <- unlist(strsplit(exclude_deparsed, ":", fixed = TRUE))
    bound1 <- grep(paste0("^", from_to[1], "$"), cols, ignore.case = ignore_case)
    bound2 <- grep(paste0("^", from_to[2], "$"), cols, ignore.case = ignore_case)
    exclude_deparsed <- cols[bound1:bound2]
    exclude <- exclude_deparsed
  }

  # Deal with functions with parenthesis, e.g is.numeric()
  if (length(exclude_deparsed) == 1 && .is_function(exclude_deparsed)) {
    exclude_deparsed <- gsub("\\(\\)$", "", exclude_deparsed)
    exclude <- get(exclude_deparsed, envir = parent.frame())
  }

  # NSE e.g input is Petal.Length
  if (all(exclude_deparsed %in% cols)) {
    excluded <- exclude_deparsed
    exclude <- exclude_deparsed # avoid evaluation below because it would return
    # non-evaluable
  }


  exclude_is_evaluable <- !inherits(try(eval(exclude), silent = TRUE), "try-error")

  if (exclude_is_evaluable) {
    if (is.null(exclude)) {
      excluded <- NULL
    } else if (is.character(exclude)) {
      if (regex) {
        excluded <- cols[which(grepl(exclude, cols))]
      } else {
        excluded <- vector(length = length(exclude))
        for (i in seq_along(exclude)) {
          tmp <- cols[grep(paste0("^", exclude[i], "$"), cols, ignore.case = ignore_case)]
          if (length(tmp) > 0) {
            excluded[i] <- tmp
          }
        }
      }
    } else if (is.numeric(exclude)) {
      # negative values in "select" mean "start from the right" so if all values
      # are negative, I reverse the ordering of colnames, which is why I need
      # a tmp vector
      tmp_cols <- cols
      from_the_left <- exclude[exclude > 0]
      from_the_right <- exclude[exclude < 0]
      if (length(from_the_right) > 0) {
        from_the_right <- from_the_right * -1
        tmp_cols <- rev(tmp_cols)
        from_the_right <- tmp_cols[from_the_right]
        tmp_cols <- rev(tmp_cols) # back in the original order
      }
      from_the_left <- tmp_cols[from_the_left]
      excluded <- sort(c(from_the_left, from_the_right))
    } else if (inherits(exclude, "formula")) {
      excluded <- all.vars(exclude)
    } else if (inherits(exclude, "function")) {
      excluded <- cols[sapply(data, exclude)]
    } else if (inherits(exclude, "call")) {
      tmp <- try(cols[eval(exclude)], silent = TRUE)
      if (!inherits(tmp, "try-error")) {
        excluded <- tmp
      }
    }
  }

  if (!exclude_is_evaluable) {
    match_info <- .get_match(exclude_deparsed)
    match <- match_info$out
    if (match_info$accessed_via_envir) {
      exclude_deparsed <- match_info$deparsed
    }
    if (grepl("^starts_with\\(", exclude_deparsed)) {
      excluded <- .starts_with(match, data, ignore_case = ignore_case)
    }
    if (grepl("^ends_with\\(|^col_ends_with\\(", exclude_deparsed, perl = TRUE)) {
      excluded <- .ends_with(match, data, ignore_case = ignore_case)
    }
    if (grepl("^contains\\(", exclude_deparsed)) {
      excluded <- .contains(match, data, ignore_case = ignore_case)
    }
    if (grepl("^regex\\(", exclude_deparsed)) {
      excluded <- .contains(match, data, ignore_case = ignore_case)
    }
  }

  if (exclude_is_negated) {
    excluded <- setdiff(cols, excluded)
  }

  # Summarize selected and excluded vars -----------------------------

  selected <- Filter(is.character, selected)
  excluded <- Filter(is.character, excluded)

  if (any(!c(selected, excluded) %in% cols)) {
    if (isTRUE(verbose)) {
      warning(insight::format_message(
        paste0("Following variable(s) were not found: ",
               paste0(setdiff(c(selected, excluded), cols), collapse = ", "))
      ), call. = FALSE)
    }
  }


  # only keep cols that actually exist
  if (!is.null(selected) && length(selected) > 0) {
    selected <- selected[selected %in% cols]
  }
  if (!is.null(excluded) && length(excluded) > 0) {
    excluded <- excluded[excluded %in% cols]
  }

  out <- selected
  if (!is.null(excluded) && length(excluded) > 0) {
    to_exclude <- which(out %in% excluded)
    if (length(to_exclude) > 0) {
      out <- out[-to_exclude]
    }
  }

  if (is.null(out)) {
    insight::format_message(
      warning(
        "No column names that matched the required search pattern were found.",
        call. = FALSE
      )
    )
  }

  # load again
  .attach_packages(conflicting_packages)

  out
}

# Extract the match that is used by starts_with, ends_with, etc.
# What they use is not always a character because they can use a variable
# that was defined previously or in a loop.
# See e.g datawizard#180

.get_match <- function(x) {

  # is it an object that is present in the environment (ex: "i" which is
  # called via a function or a loop)
  # The tryCatch below will capture the select helper, e.g if "i" = starts_with("foo")
  accessed_via_envir <- FALSE
  tmp_orig <- invisible(tryCatch(
    {
      dynGet(x, inherits = TRUE)
      return(x)
    },
    error = function(e) {
      fn <- insight::safe_deparse(e$call)

      if (grepl(regex_select_helper, fn)) {
        accessed_via_envir <<- TRUE
        return(fn)
      }
      else
        return(x)
    }
  ))

  # if we use a select helper, check that its argument is a character. If it's
  # not, maybe its argument is a variable called in a loop or a function, so
  # need to grab its value.

  tmp <- gsub(regex_select_helper, "", tmp_orig)
  tmp <- gsub("^\\(", "", tmp)
  tmp <- gsub("\\)$", "", tmp)
  is_quoted <- grepl("^\\\"|^\\\'", tmp, perl = TRUE) &&
    grepl("\\\"$|\\\'$", tmp, perl = TRUE)
  if (!is_quoted) {
    tmp <- insight::safe_deparse(get(tmp, env = parent.frame(3L)))
  }
  out <- gsub("^\"", "", tmp)
  out <- gsub("\"$", "", out)
  return(
    list(
      out = out,
      accessed_via_envir = accessed_via_envir,
      deparsed = tmp_orig
    )
  )
  return(out)
}

# used for e.g select = as.numeric() (can't evaluate it because missing arg)
.is_function <- function(x) {
  if (is.null(x)) return(FALSE)
  out <- grepl("\\(\\)$", x)
  if (isFALSE(out)) {
    out <- try(is.function(eval(parse(text = x))), silent = TRUE)
    if (inherits(out, "try-error")) {
      out <- FALSE
    }
  }
  out
}

.is_negated <- function(x) {
  if (is.null(x)) return(FALSE)
  grepl("^-", x)
}

# returns TRUE if input is a range with characters, e.g abc:def, and returns
# FALSE if input is made only of numbers, e.g 1:4
.is_range_character <- function(x) {
  if (is.null(x)) return(FALSE)
  if (.is_numeric(x)) return(FALSE)
  # clean string to avoid (1:2) being considered as range of vars
  x <- gsub("^\\(", "", x)
  x <- gsub("\\)$", "", x)
  split <- unlist(strsplit(x, ":"))
  if (length(split) != 2) return(FALSE)
  !all(.numbers_only(split[1]), .numbers_only(split[2]))
}

# is a character vector made only of numbers ?
.numbers_only <- function(x) {
  !grepl("\\D", x)
}

# check if a character vector is numeric, e.g "1:10"
.is_numeric <- function(x) {
  if (is.null(x)) return(FALSE)
  out <- try(eval(parse(text = x)), silent = TRUE)
  if (inherits(out, "try-error")) return(FALSE)
  is.numeric(out)
}

# check if a string starts with a select helper
.is_select_helper <- function(x) {
  if (is.null(x)) return(FALSE)
  grepl(regex_select_helper, x)
}

# dynGet (depends on R >= 3.2) makes a dynamyc search of an object through all
# the call stack, so it finds an object no matter the number of parent.frame()
.is_object_in_env <- function(x) {
  suppressMessages(tryCatch(
    {
      dynGet(x, inherits = TRUE)
      TRUE
    },
    error = function(e) {
      FALSE
    }
  ))
}

regex_select_helper <- "^(starts\\_with|ends\\_with|col\\_ends\\_with|contains|regex)"

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
