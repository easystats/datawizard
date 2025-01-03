# Code adapted from {poorman} by Nathan Eastwood [License: MIT]
# https://github.com/nathaneastwood/poorman/blob/master/R/select_positions.R

.select_nse <- function(select, data, exclude, ignore_case, regex = FALSE,
                        remove_group_var = FALSE, allow_rename = FALSE,
                        verbose = FALSE, ifnotfound = "warn") {
  .check_data(data)
  columns <- colnames(data)

  # avoid conflicts
  conflicting_packages <- .conflicting_packages("poorman")
  on.exit(.attach_packages(conflicting_packages))

  expr_select <- substitute(select, env = parent.frame(1L))
  expr_exclude <- substitute(exclude, env = parent.frame(1L))

  # when exclude is not an argument called from the function (e.g data_to_long),
  # do not consider "exclude" as a symbol
  if (deparse(expr_exclude) == "exclude" && is.null(substitute(exclude))) {
    expr_exclude <- NULL
  }

  # for grouped data frames, we can decide to remove group variable from selection
  grp_vars <- setdiff(colnames(attr(data, "groups", exact = TRUE)), ".rows")

  # directly return all names if select == exclude == NULL
  if (is.null(expr_select) && is.null(expr_exclude)) {
    # don't include grouping variables
    if (remove_group_var) {
      columns <- setdiff(columns, grp_vars)
    }
    return(columns)
  }

  # get the position of columns that are selected or excluded
  selected <- .eval_expr(
    expr_select,
    data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  )
  excluded <- .eval_expr(
    expr_exclude,
    data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  )

  selected_has_mix_idx <- any(selected < 0L) && any(selected > 0L)
  excluded_has_mix_idx <- any(excluded < 0L) && any(excluded > 0L)

  if (selected_has_mix_idx || excluded_has_mix_idx) {
    insight::format_error(
      "You can't mix negative and positive indices in `select` or `exclude`."
    )
  }

  # variable positions -> variable names
  selected <- columns[selected]
  excluded <- columns[excluded]

  if (length(selected) == 0L) {
    if (length(excluded) == 0L) {
      out <- character(0L)
    } else {
      out <- setdiff(columns, excluded)
    }
  } else {
    out <- setdiff(selected, excluded)
  }

  # don't include grouping variables
  if (remove_group_var && length(out)) {
    out <- setdiff(out, grp_vars)
  }

  # for named character vectors, we offer the service to rename the columns
  if (allow_rename && typeof(expr_select) == "language") {
    # safe evaluation of the expression, to get the named vector from "select"
    new_names <- tryCatch(eval(expr_select), error = function(e) NULL)
    # check if we really have a named vector
    if (!is.null(new_names) && !is.null(names(new_names))) {
      # if so, copy names
      all_names <- names(new_names)
      # if some of the elements don't have a name, we set the value as name
      names(new_names)[!nzchar(all_names)] <- new_names[!nzchar(all_names)]
      # after inclusion and exclusion, the original values in "select"
      # may have changed, so we check that we only add names of valid values
      out <- stats::setNames(out, names(new_names)[new_names %in% out])
      # check if we have any duplicated names, and if so, give an error
      if (anyDuplicated(names(out)) > 0) {
        insight::format_error(paste0(
          "Following names are duplicated after renaming: ",
          text_concatenate(names(out)[duplicated(names(out))], enclose = "`"),
          ". Using duplicated names is no good practice and therefore discouraged. Please provide unique names."
        ))
      }
    }
  }

  out
}


# This is where we dispatch the expression to several helper functions.
# This function is called multiple times for expressions that are composed
# of several symbols/language.
#
# Ex:
# * "cyl" -> will go to .select_char() and will return directly
# * cyl:gear -> function (`:`) so find which function it is, then get the
#   position for each variable, then evaluate the function with the positions

.eval_expr <- function(x, data, ignore_case, regex, verbose, ifnotfound) {
  if (is.null(x)) {
    return(NULL)
  }

  type <- typeof(x)

  out <- switch(type,
    integer = x,
    double = as.integer(x),
    character = .select_char(
      data, x, ignore_case,
      regex = regex, verbose, ifnotfound
    ),
    symbol = .select_symbol(
      data, x, ignore_case,
      regex = regex, verbose, ifnotfound
    ),
    language = .eval_call(
      data, x, ignore_case,
      regex = regex, verbose, ifnotfound
    ),
    insight::format_error(paste0(
      "Expressions of type <", typeof(x),
      "> cannot be evaluated for use when subsetting."
    ))
  )

  out
}


# Possibilities:
# - quoted variable name
# - quoted variable name with ignore case
# - quoted variable name with colon, to indicate range
# - character that should be regex-ed on variable names
# - special word "all" to return all vars

.select_char <- function(data, x, ignore_case, regex, verbose, ifnotfound) {
  # use colnames because names() doesn't work for matrices
  columns <- colnames(data)
  if (isTRUE(regex)) {
    # string is a regular expression
    grep(x, columns)
  } else if (length(x) == 1L && x == "all") {
    # string is "all" - select all columns
    seq_along(data)
  } else if (any(grepl(":", x, fixed = TRUE))) {
    # special pattern, as string (e.g.select = c("cyl:hp", "am")). However,
    # this will first go into `.eval_call()` and thus only single elements
    # are passed in `x` - we have never a character *vector* here
    # check for valid names
    colon_vars <- unlist(strsplit(x, ":", fixed = TRUE))
    colon_match <- match(colon_vars, columns)
    if (anyNA(colon_match)) {
      .action_if_not_found(colon_vars, columns, colon_match, verbose, ifnotfound)
      matches <- NA
    } else {
      start_pos <- match(colon_vars[1], columns)
      end_pos <- match(colon_vars[2], columns)
      if (!is.na(start_pos) && !is.na(end_pos)) {
        matches <- start_pos:end_pos
      } else {
        matches <- NA
      }
    }
    matches[!is.na(matches)]
  } else if (isTRUE(ignore_case)) {
    # find columns, case insensitive
    matches <- match(toupper(x), toupper(columns))
    matches[!is.na(matches)]
  } else {
    # find columns, case sensitive
    matches <- match(x, columns)
    if (anyNA(matches)) {
      .action_if_not_found(x, columns, matches, verbose, ifnotfound)
    }
    matches[!is.na(matches)]
  }
}

# small helper, to avoid duplicated code

.action_if_not_found <- function(x,
                                 columns,
                                 matches,
                                 verbose,
                                 ifnotfound) {
  msg <- paste0(
    "Following variable(s) were not found: ",
    toString(x[is.na(matches)])
  )
  msg2 <- .misspelled_string(
    columns,
    x[is.na(matches)],
    default_message = "Possibly misspelled?"
  )
  if (ifnotfound == "error") {
    insight::format_error(msg, msg2)
  }
  if (ifnotfound == "warn" && verbose) {
    insight::format_warning(msg, msg2)
  }
}


# 3 types of symbols:
# - unquoted variables
# - objects that need to be evaluated, e.g data_find(iris, i) where
#   i is a function arg or is defined before. This can also be a
#   vector of names or positions.
# - functions (without parenthesis)

# The first case is easy to deal with.
# For the 2nd one, we try to get the value of the object at each environment
# (starting from the lower one) until the global environment. If we get its
# value but it errors because the function doesn't exist then it means that
# it is a select helper that we grab from the error message.

.select_symbol <- function(data, x, ignore_case, regex, verbose, ifnotfound) {
  try_eval <- try(eval(x), silent = TRUE)
  x_dep <- insight::safe_deparse(x)
  is_select_helper <- FALSE
  out <- NULL

  if (x_dep %in% colnames(data)) {
    matches <- match(x_dep, colnames(data))
    out <- matches[!is.na(matches)]
  } else if (isTRUE(ignore_case)) {
    matches <- match(toupper(x_dep), toupper(colnames(data)))
    out <- matches[!is.na(matches)]
  } else {
    new_expr <- tryCatch(
      .dynGet(x, inherits = FALSE, minframe = 0L),
      error = function(e) {
        # if starts_with() et al. don't exist
        fn <- insight::safe_deparse(e$call)

        # if starts_with() et al. come from tidyselect but need to be used in
        # a select environment, then the error doesn't have the same structure.
        if (is.null(fn) && grepl("must be used within a", e$message, fixed = TRUE)) {
          call_trace <- lapply(e$trace$call, function(x) {
            tmp <- insight::safe_deparse(x)
            if (grepl(paste0("^", .regex_select_helper()), tmp)) {
              tmp
            }
          })
          fn <- Filter(Negate(is.null), call_trace)[1]
        }
        # if we actually obtain the select helper call, return it, else return
        # what we already had
        if (length(fn) > 0L && grepl(.regex_select_helper(), fn)) {
          is_select_helper <<- TRUE
          return(fn)
        }
        NULL
      }
    )

    # when "x" is a function arg which is itself a function call to evaluate,
    # .dynGet can return "x" infinitely so we try to evaluate this arg
    # see #414
    if (!is.null(new_expr) && insight::safe_deparse(new_expr) == "x") {
      new_expr <- .dynEval(x, inherits = FALSE, minframe = 0L, remove_n_top_env = 4)
    }

    if (is_select_helper) {
      new_expr <- str2lang(unlist(new_expr, use.names = FALSE))
      out <- .eval_expr(
        new_expr,
        data = data,
        ignore_case = ignore_case,
        regex = regex,
        verbose = verbose,
        ifnotfound = ifnotfound
      )
    } else if (length(new_expr) == 1L && is.function(new_expr)) {
      out <- which(vapply(data, new_expr, FUN.VALUE = logical(1L)))
    } else {
      out <- unlist(lapply(
        new_expr,
        .eval_expr,
        data = data,
        ignore_case = ignore_case,
        regex = regex,
        verbose = verbose,
        ifnotfound = ifnotfound
      ), use.names = FALSE)
    }
  }

  # sometimes an object that needs to be evaluated has the same name as a
  # function (e.g `colnames`). Vector of names have the priority on functions
  # so function evaluation is delayed at the max.
  if (is.null(out) && is.function(try_eval)) {
    cols <- names(data)
    out <- which(vapply(data, x, FUN.VALUE = logical(1L)))
  }

  out
}

# Dispatch expressions to various select helpers according to the function call.

.eval_call <- function(data, x, ignore_case, regex, verbose, ifnotfound) {
  type <- insight::safe_deparse(x[[1]])
  switch(type,
    `:` = .select_seq(x, data, ignore_case, regex, verbose, ifnotfound),
    `-` = .select_minus(x, data, ignore_case, regex, verbose, ifnotfound),
    `c` = .select_c(x, data, ignore_case, regex, verbose, ifnotfound), # nolint
    `(` = .select_bracket(x, data, ignore_case, regex, verbose, ifnotfound),
    `[` = .select_square_bracket(
      x, data, ignore_case, regex, verbose, ifnotfound
    ),
    `$` = .select_dollar(x, data, ignore_case, regex, verbose, ifnotfound),
    `~` = .select_tilde(x, data, ignore_case, regex, verbose, ifnotfound),
    list = .select_list(x, data, ignore_case, regex, verbose, ifnotfound),
    names = .select_names(x, data, ignore_case, regex, verbose, ifnotfound),
    starts_with = ,
    ends_with = ,
    matches = ,
    contains = ,
    regex = .select_helper(x, data, ignore_case, regex, verbose, ifnotfound),
    .select_context(x, data, ignore_case, regex, verbose, ifnotfound)
  )
}

# e.g 1:3, or gear:cyl
.select_seq <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  x <- .eval_expr(
    expr[[2]],
    data = data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  )
  y <- .eval_expr(
    expr[[3]],
    data = data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  )
  x:y
}

# e.g -cyl
.select_minus <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  x <- .eval_expr(
    expr[[2]],
    data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  )
  if (length(x) == 0L) {
    seq_along(data)
  } else {
    x * -1L
  }
}

# e.g c("gear", "cyl")
.select_c <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  lst_expr <- as.list(expr)
  lst_expr[[1]] <- NULL
  unlist(lapply(
    lst_expr,
    .eval_expr,
    data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  ), use.names = FALSE)
}

# e.g -(gear:cyl)
.select_bracket <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  .eval_expr(
    expr[[2]],
    data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  )
}

# e.g myvector[3]
.select_square_bracket <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  first_obj <- .eval_expr(
    expr[[2]],
    data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  )
  .eval_expr(
    first_obj[eval(expr[[3]])],
    data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  )
}

.select_names <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  first_obj <- .dynEval(expr, inherits = FALSE, minframe = 0L)
  .eval_expr(
    first_obj,
    data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = FALSE,
    ifnotfound = ifnotfound
  )
}

# e.g starts_with("Sep")
.select_helper <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  lst_expr <- as.list(expr)

  # need this if condition to distinguish between starts_with("Sep") (that we
  # can use directly) and starts_with(i) (where we need to get i)
  if (length(lst_expr) == 2L && typeof(lst_expr[[2]]) == "symbol") {
    collapsed_patterns <- .dynGet(lst_expr[[2]], inherits = FALSE, minframe = 0L)
  } else {
    collapsed_patterns <- paste(unlist(lst_expr[2:length(lst_expr)]), collapse = "|")
  }

  helper <- insight::safe_deparse(lst_expr[[1]])

  rgx <- switch(helper,
    starts_with = paste0("^(", collapsed_patterns, ")"),
    ends_with = paste0("(", collapsed_patterns, ")$"),
    contains = paste0("(", collapsed_patterns, ")"),
    regex = collapsed_patterns,
    insight::format_error("There is no select helper called '", helper, "'.")
  )
  grep(rgx, colnames(data), ignore.case = ignore_case)
}

# e.g args$select (happens when we use grouped_data (see center.grouped_df()))
.select_dollar <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  first_obj <- .dynGet(expr[[2]], ifnotfound = NULL, inherits = FALSE, minframe = 0L)
  if (is.null(first_obj)) {
    first_obj <- .dynEval(expr[[2]], inherits = FALSE, minframe = 0L)
  }
  .eval_expr(
    first_obj[[deparse(expr[[3]])]],
    data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  )
}

# e.g ~ gear + cyl
.select_tilde <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  vars <- all.vars(expr)
  unlist(lapply(
    vars,
    .eval_expr,
    data = data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  ), use.names = FALSE)
}

# e.g list(gear = 4, cyl = 5)
.select_list <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  vars <- names(.dynEval(expr, inherits = FALSE, minframe = 0L))
  unlist(lapply(
    vars,
    .eval_expr,
    data = data,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose,
    ifnotfound = ifnotfound
  ), use.names = FALSE)
}

# e.g is.numeric()
.select_context <- function(expr, data, ignore_case, regex, verbose, ifnotfound) {
  x_dep <- insight::safe_deparse(expr)
  if (endsWith(x_dep, "()")) {
    new_expr <- gsub("\\(\\)$", "", x_dep)
    new_expr <- str2lang(new_expr)
    .eval_expr(
      new_expr,
      data = data,
      ignore_case = ignore_case,
      regex = regex,
      verbose = verbose,
      ifnotfound = ifnotfound
    )
  } else {
    out <- .dynEval(expr, inherits = FALSE, minframe = 0L)
    .eval_expr(
      out,
      data = data,
      ignore_case = ignore_case,
      regex = regex,
      verbose = verbose,
      ifnotfound = ifnotfound
    )
  }
}

# -------------------------------------

.check_data <- function(data) {
  if (is.null(data)) {
    insight::format_error("The `data` argument must be provided.")
  }
  .coerce_to_dataframe(data)
}

.regex_select_helper <- function() {
  "(starts\\_with|ends\\_with|col\\_ends\\_with|contains|regex)"
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
        suppressPackageStartupMessages(
          suppressWarnings(
            require(pkg[i], quietly = TRUE, character.only = TRUE)
          )
        )
      }
    }
  }
}

# Almost identical to dynGet(). The difference is that we deparse the expression
# because get0() allows symbol only since R 4.1.0
.dynGet <- function(x,
                    ifnotfound = stop(gettextf("%s not found", sQuote(x)), domain = NA, call. = FALSE),
                    minframe = 1L,
                    inherits = FALSE) {
  x <- insight::safe_deparse(x)
  n <- sys.nframe()
  myObj <- structure(list(.b = as.raw(7)), foo = 47L)
  while (n > minframe) {
    n <- n - 1L
    env <- sys.frame(n)
    r <- get0(x, envir = env, inherits = inherits, ifnotfound = myObj)
    if (!identical(r, myObj)) {
      return(r)
    }
  }
  ifnotfound
}

# Similar to .dynGet() but instead of getting an object from the environment,
# we try to evaluate an expression. It stops as soon as the evaluation doesn't
# error. Returns NULL if can never be evaluated.
#
# Custom arg "remove_n_top_env" to remove the first environments which are
# ".select_nse()" and the other custom functions
.dynEval <- function(x,
                     ifnotfound = stop(gettextf("%s not found", sQuote(x)), domain = NA, call. = FALSE),
                     minframe = 1L,
                     inherits = FALSE,
                     remove_n_top_env = 0) {
  n <- sys.nframe() - remove_n_top_env
  x <- insight::safe_deparse(x)
  while (n > minframe) {
    n <- n - 1L
    env <- sys.frame(n)
    r <- try(eval(str2lang(x), envir = env), silent = TRUE)
    if (!inherits(r, "try-error") && !is.null(r)) {
      return(r)
    }
  }
  ifnotfound
}
