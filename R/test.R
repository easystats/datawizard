# this function evaluates the select-expression and allows non-standard evaluation

.select_nse <- function(select, data, exclude, ignore_case, regex = FALSE, verbose = FALSE) {

  .check_data(data)
  columns <- colnames(data)

  expr_select <- substitute(select, env = parent.frame(1L))
  expr_exclude <- substitute(exclude, env = parent.frame(1L))

  # directly return all names if select == exclude == NULL
  if (is.null(expr_select) && is.null(expr_exclude)) {
    return(columns)
  }

  selected <- .eval_expr(expr_select, data, ignore_case = ignore_case, regex = regex)
  excluded <- .eval_expr(expr_exclude, data, ignore_case = ignore_case, regex = regex)

  if ((any(selected < 0) && any(selected > 0)) ||
      (any(excluded < 0) && any(excluded > 0))) {
    insight::format_error(
      paste0("You can't mix negative and positive numeric indices in `select` or `exclude`.")
    )
  }

  selected <- columns[selected]
  excluded <- columns[excluded]

  if (length(selected) == 0L) {
    if (length(excluded) == 0L) {
      out <- NULL
    } else {
      out <- setdiff(columns, excluded)
    }
  } else {
    out <- setdiff(selected, excluded)
  }

  if (is.null(out)) out <- character(0L)

  out
}


.eval_expr <- function(x, data, ignore_case, regex) {

  if (is.null(x)) return(NULL)

  type <- typeof(x)

  out <- switch(
    type,
    "integer" = x,
    "double" = as.integer(x),
    "character" = .select_char(data, x, ignore_case, regex = regex),
    "symbol" = .select_symbol(data, x, ignore_case, regex = regex),
    "language" = .eval_call(data, x, ignore_case, regex = regex),
    stop("Expressions of type <", typeof(x), "> cannot be evaluated for use when subsetting.")
  )

  out
}

.select_char <- function(data, x, ignore_case, regex) {
  if (isTRUE(regex)) {
    grep(x, colnames(data))
  } else if (length(x) > 0L && x == "all") {
    return(seq_along(iris))
  } else {
    which(x == colnames(data)) # colnames because names() doesn't work for matrices
  }
}

.select_symbol <- function(data, x, ignore_case, regex) {
  try_eval <- try(eval(x), silent = TRUE)
  x_dep <- deparse(x)
  is_select_helper <- FALSE

  if (is.function(try_eval)) {
    cols <- names(data)
    which(vapply(data, x, FUN.VALUE = logical(1L)))
  } else if (x_dep %in% colnames(data)) {
    which(x_dep == colnames(data))
  } else {
    new_expr <- tryCatch(
      dynGet(x, inherits = FALSE, minframe = 0L),
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
          is_select_helper <<- TRUE
          return(fn)
        } else {
          NULL
        }
      }
    )

    if (is_select_helper) {
      new_expr <- str2lang(new_expr)
      .eval_expr(new_expr, data = data, ignore_case = ignore_case, regex = regex)
    } else {
      unlist(lapply(new_expr, .eval_expr, data = data, ignore_case = ignore_case, regex = regex))
    }
  }
}

.eval_call <- function(data, x, ignore_case = ignore_case, regex) {
  type <- as.character(x[[1]])
  if (length(type) > 1L) {
    # This helps when pkg::fn is used in a select helper
    type <- "context"
  }

  switch(
    type,
    `:` = .select_seq(x, data, ignore_case, regex),
    `!` = .select_negate(x, data, ignore_case, regex),
    `-` = .select_minus(x, data, ignore_case, regex),
    `c` = .select_c(x, data, ignore_case, regex),
    `(` = .select_bracket(x, data, ignore_case, regex),
    `&` = .select_and(x, data, ignore_case, regex),
    `$` = .select_dollar(x, data, ignore_case, regex),
    `~` = .select_tilde(x, data, ignore_case, regex),
    "starts_with" = ,
    "ends_with" = ,
    "matches" = ,
    "contains" = ,
    "regex" = .select_helper(x, data, ignore_case, regex),
    .select_context(x, data)
  )
}

.select_seq <- function(expr, data, ignore_case, regex) {
  x <- .eval_expr(expr[[2]])
  y <- .eval_expr(expr[[3]])
  x:y
}

.select_negate <- function(expr, data, ignore_case, regex) {
  x <- if (.is_negated_colon(expr, data)) {
    expr <- call(":", expr[[2]][[2]], expr[[2]][[3]][[2]])
    .eval_expr(expr, data)
  } else {
    .eval_expr(expr[[2]], data, ignore_case = ignore_case, regex = regex)
  }
  x * -1L
}

.is_negated_colon <- function(expr, data, ignore_case, regex) {
  expr[[1]] == "!" && length(expr[[2]]) > 1L && expr[[2]][[1]] == ":" && expr[[2]][[3]][[1]] == "!"
}

.select_minus <- function(expr, data, ignore_case, regex) {
  x <- .eval_expr(expr[[2]], data, ignore_case = ignore_case, regex = regex)
  x * -1L
}

.select_c <- function(expr, data, ignore_case, regex) {
  lst_expr <- as.list(expr)
  lst_expr[[1]] <- NULL
  unlist(lapply(lst_expr, .eval_expr, data = data, ignore_case = ignore_case, regex = regex))
}

.select_bracket <- function(expr, data, ignore_case, regex) {
  .eval_expr(expr[[2]], ignore_case = ignore_case, regex = regex)
}

.select_helper <- function(expr, data, ignore_case, regex) {
  lst_expr <- as.list(expr)

  if (length(lst_expr) == 2L && typeof(lst_expr[[2]]) == "symbol") {
    collapsed_patterns <- dynGet(lst_expr[[2]], inherits = FALSE, minframe = 0L)
  } else {
    collapsed_patterns <- paste(unlist(lst_expr[2:length(lst_expr)]), collapse = "|")
  }

  rgx <- if (lst_expr[[1]] == "starts_with") {
    paste0("^(", collapsed_patterns ,")")
  } else if (lst_expr[[1]] == "ends_with") {
    paste0("(", collapsed_patterns ,")$")
  } else if (lst_expr[[1]] == "contains") {
    paste0("(", collapsed_patterns,")")
  } else if (lst_expr[[1]] == "regex") {
    collapsed_patterns
  }
  grep(rgx, colnames(data), ignore.case = ignore_case)
}

.select_dollar <- function(expr, data, ignore_case, regex) {
  first_obj <- dynGet(expr[[2]], inherits = FALSE, minframe = 0L)
  .eval_expr(first_obj[[deparse(expr[[3]])]], data, ignore_case = ignore_case, regex = regex)
}

.select_tilde <- function(expr, data, ignore_case, regex) {
  vars <- all.vars(expr)
  unlist(lapply(vars, .eval_expr, data = data, ignore_case = ignore_case, regex = regex))
}

.select_context <- function(expr, data) {
  eval(expr, envir = data)
}


.check_data <- function(data) {
  if (is.null(data)) {
    insight::format_error("The `data` argument must be provided.")
  }
  # check data frame input
  if (!is.null(data)) {
    data <- .coerce_to_dataframe(data)
  }
}

.regex_select_helper <- function() "(starts\\_with|ends\\_with|col\\_ends\\_with|contains|regex)"

