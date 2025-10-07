#' Describe a distribution
#'
#' This function describes a distribution by a set of indices (e.g., measures of
#' centrality, dispersion, range, skewness, kurtosis).
#'
#' @param x A numeric vector, a character vector, a data frame, or a list. See
#' `Details`.
#' @param by Column names indicating how to split the data in various groups
#' before describing the distribution. `by` groups will be added to potentially
#' existing groups created by `data_group()`.
#' @param range Return the range (min and max).
#' @param quartiles Return the first and third quartiles (25th and 75th
#'   percentiles).
#' @param include_factors Logical, if `TRUE`, factors are included in the
#'   output, however, only columns for range (first and last factor levels) as
#'   well as n and missing will contain information.
#' @param ci Confidence Interval (CI) level. Default is `NULL`, i.e. no
#'   confidence intervals are computed. If not `NULL`, confidence intervals are
#'   based on bootstrap replicates (see `iterations`).
#' @param iterations The number of bootstrap replicates for computing confidence
#'   intervals. Only applies when `ci` is not `NULL`. Defaults to `100`. For
#'   more stable results, increase the number of `iterations`, but note that this
#'   can also increase the computation time significantly.
#' @param iqr Logical, if `TRUE`, the interquartile range is calculated (based
#'   on [stats::IQR()], using `type = 6`).
#' @param verbose Show or silence warnings and messages.
#' @inheritParams bayestestR::point_estimate
#' @inheritParams extract_column_names
#'
#' @details If `x` is a data frame, only numeric variables are kept and will be
#' displayed in the summary by default.
#'
#' If `x` is a list, the behavior is different whether `x` is a stored list. If
#' `x` is stored (for example, `describe_distribution(mylist)` where `mylist`
#' was created before), artificial variable names are used in the summary
#' (`Var_1`, `Var_2`, etc.). If `x` is an unstored list (for example,
#' `describe_distribution(list(mtcars$mpg))`), then `"mtcars$mpg"` is used as
#' variable name.
#'
#' @note There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
#'   implemented in the [**see**-package](https://easystats.github.io/see/).
#'
#' @return A data frame with columns that describe the properties of the variables.
#'
#' @examplesIf require("bayestestR", quietly = TRUE)
#' describe_distribution(rnorm(100))
#'
#' data(iris)
#' describe_distribution(iris)
#' describe_distribution(iris, include_factors = TRUE, quartiles = TRUE)
#' describe_distribution(list(mtcars$mpg, mtcars$cyl))
#' @export
describe_distribution <- function(x, ...) {
  UseMethod("describe_distribution")
}


#' @export
describe_distribution.default <- function(x, verbose = TRUE, ...) {
  if (verbose) {
    insight::format_warning(
      paste0("Can't describe variables of class `", class(x)[1], "`.")
    )
  }
  NULL
}


#' @export
describe_distribution.list <- function(
  x,
  centrality = "mean",
  dispersion = TRUE,
  iqr = TRUE,
  range = TRUE,
  quartiles = FALSE,
  ci = NULL,
  include_factors = FALSE,
  iterations = 100,
  threshold = 0.1,
  verbose = TRUE,
  ...
) {
  factor_el <- which(vapply(x, is.factor, FUN.VALUE = logical(1L)))
  num_el <- which(vapply(x, is.numeric, FUN.VALUE = logical(1L)))

  # get elements names as is
  # ex: `list(mtcars$mpg, mtcars$cyl) -> c("mtcars$mpg", "mtcars$cyl")`
  nm <- vapply(
    sys.call()[[2]],
    insight::safe_deparse,
    FUN.VALUE = character(1L)
  )[-1]

  if (isTRUE(include_factors)) {
    x <- x[c(num_el, factor_el)]
    if (length(nm) != 0) {
      nm <- nm[c(num_el, factor_el)]
    }
  } else {
    x <- x[num_el]
    if (length(nm) != 0) {
      nm <- nm[num_el]
    }
  }

  # Not possible to obtain elements names if they are stored in
  # an object
  if (length(nm) == 0) {
    nm <- paste0("Var_", seq_along(x))
  }

  # The function currently doesn't support descriptive summaries for character
  # or factor types.
  out <- do.call(
    rbind,
    lapply(x, function(i) {
      if (
        (include_factors && is.factor(i)) || (!is.character(i) && !is.factor(i))
      ) {
        describe_distribution(
          i,
          centrality = centrality,
          dispersion = dispersion,
          iqr = iqr,
          range = range,
          quartiles = quartiles,
          ci = ci,
          iterations = iterations,
          threshold = threshold,
          verbose = verbose
        )
      }
    })
  )

  if (is.null(names(x))) {
    new_names <- nm
  } else {
    empty_names <- which(!nzchar(names(x), keepNA = TRUE))
    new_names <- names(x)
    new_names[empty_names] <- nm[empty_names]
  }

  out$Variable <- new_names
  row.names(out) <- NULL
  out <- out[c("Variable", setdiff(colnames(out), "Variable"))]

  class(out) <- unique(c(
    "parameters_distribution",
    "see_parameters_distribution",
    class(out)
  ))
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  attr(out, "ci") <- ci
  attr(out, "centrality") <- centrality
  attr(out, "threshold") <- threshold
  out
}


#' @rdname describe_distribution
#' @export
describe_distribution.numeric <- function(
  x,
  centrality = "mean",
  dispersion = TRUE,
  iqr = TRUE,
  range = TRUE,
  quartiles = FALSE,
  ci = NULL,
  iterations = 100,
  threshold = 0.1,
  verbose = TRUE,
  ...
) {
  insight::check_if_installed("bayestestR")
  out <- data.frame(.temp = 0)

  # Missing
  n_missing <- sum(is.na(x))
  x <- stats::na.omit(x)

  # Point estimates
  out <- cbind(
    out,
    bayestestR::point_estimate(
      x,
      centrality = centrality,
      dispersion = dispersion,
      threshold = threshold,
      verbose = verbose,
      ...
    )
  )

  # interquartile range, type same as minitab and SPSS
  if (iqr) {
    out$IQR <- stats::IQR(x, na.rm = TRUE, type = 6)
  }

  # Confidence Intervals
  if (!is.null(ci)) {
    insight::check_if_installed("boot")
    # tell user about bootstrapping and appropriate number of iterations.
    # "show_iterations_msg" is an undocumented argument that is only passed
    # internally to this function to avoid multiple repeated messages
    if (!isFALSE(list(...)$show_iterations_msg)) {
      .show_iterations_warning(verbose, iterations, ci)
    }
    # calculate CI for each centrality
    for (cntr in .centrality_options(centrality)) {
      results <- tryCatch(
        {
          boot::boot(
            data = x,
            statistic = .boot_distribution,
            R = iterations,
            centrality = cntr
          )
        },
        error = function(e) {
          msg <- conditionMessage(e)
          if (!is.null(msg) && msg == "sample is too sparse to find TD") {
            insight::format_warning(
              "When bootstrapping CIs, sample was too sparse to find TD. Returning NA for CIs."
            )
            list(t = c(NA_real_, NA_real_))
          }
        }
      )
      out_ci <- bayestestR::ci(results$t, ci = ci, verbose = FALSE)
      ci_data <- data.frame(out_ci$CI_low[1], out_ci$CI_high[1])
      colnames(ci_data) <- c(paste0("CI_low_", cntr), paste0("CI_high_", cntr))
      out <- cbind(out, ci_data)
    }
  }

  # Range
  if (range) {
    out <- cbind(
      out,
      data.frame(
        Min = min(x, na.rm = TRUE),
        Max = max(x, na.rm = TRUE)
      )
    )
  }

  # Quartiles
  if (quartiles) {
    out <- cbind(
      out,
      data.frame(
        Q1 = stats::quantile(x, probs = 0.25, na.rm = TRUE),
        Q3 = stats::quantile(x, probs = 0.75, na.rm = TRUE)
      )
    )
  }

  # Skewness
  out <- cbind(
    out,
    data.frame(
      Skewness = as.numeric(skewness(x, verbose = verbose)),
      Kurtosis = as.numeric(kurtosis(x, verbose = verbose))
    )
  )

  out$n <- length(x)
  out$n_Missing <- n_missing
  out$.temp <- NULL

  class(out) <- unique(c(
    "parameters_distribution",
    "see_parameters_distribution",
    class(out)
  ))
  attr(out, "data") <- x
  attr(out, "ci") <- ci
  attr(out, "centrality") <- centrality
  attr(out, "threshold") <- threshold
  out
}


#' @rdname describe_distribution
#' @export
describe_distribution.factor <- function(
  x,
  dispersion = TRUE,
  range = TRUE,
  verbose = TRUE,
  ...
) {
  # Missing
  n_missing <- sum(is.na(x))
  x <- stats::na.omit(x)

  out <- data.frame(
    Mean = NA,
    SD = NA,
    CI_low = NA,
    CI_high = NA,
    IQR = NA,
    Min = levels(x)[1],
    Max = levels(x)[nlevels(x)],
    Q1 = NA,
    Q3 = NA,
    Skewness = as.numeric(skewness(x, verbose = verbose)),
    Kurtosis = as.numeric(kurtosis(x, verbose = verbose)),
    n = length(x),
    n_Missing = n_missing,
    stringsAsFactors = FALSE
  )

  if (!dispersion) {
    out$SD <- NULL
  }

  dot_args <- list(...)

  if (is.null(dot_args[["ci"]])) {
    out$CI_low <- NULL
    out$CI_high <- NULL
  }

  if (is.null(dot_args[["iqr"]]) || isFALSE(dot_args[["iqr"]])) {
    out$IQR <- NULL
  }

  if (is.null(dot_args[["quartiles"]]) || isFALSE(dot_args[["quartiles"]])) {
    out$Q1 <- NULL
    out$Q3 <- NULL
  }

  if (!range) {
    out$Min <- NULL
    out$Max <- NULL
  }

  class(out) <- unique(c(
    "parameters_distribution",
    "see_parameters_distribution",
    class(out)
  ))
  attr(out, "data") <- x
  out
}


#' @export
describe_distribution.character <- function(
  x,
  dispersion = TRUE,
  range = TRUE,
  verbose = TRUE,
  ...
) {
  # Missing
  n_missing <- sum(is.na(x))
  x <- stats::na.omit(x)
  values <- unique(x)

  out <- data.frame(
    Mean = NA,
    SD = NA,
    IQR = NA,
    CI_low = NA,
    CI_high = NA,
    Min = values[1],
    Max = values[length(values)],
    Q1 = NA,
    Q3 = NA,
    Skewness = as.numeric(skewness(x, verbose = verbose)),
    Kurtosis = as.numeric(kurtosis(x, verbose = verbose)),
    n = length(x),
    n_Missing = n_missing,
    stringsAsFactors = FALSE
  )

  if (!dispersion) {
    out$SD <- NULL
  }

  dot_args <- list(...)
  if (is.null(dot_args[["ci"]])) {
    out$CI_low <- NULL
    out$CI_high <- NULL
  }

  if (is.null(dot_args[["iqr"]]) || isFALSE(dot_args[["iqr"]])) {
    out$IQR <- NULL
  }

  if (is.null(dot_args[["quartiles"]]) || isFALSE(dot_args[["quartiles"]])) {
    out$Q1 <- NULL
    out$Q3 <- NULL
  }

  if (!range) {
    out$Min <- NULL
    out$Max <- NULL
  }

  class(out) <- unique(c(
    "parameters_distribution",
    "see_parameters_distribution",
    class(out)
  ))
  attr(out, "data") <- x
  out
}


#' @rdname describe_distribution
#' @export
describe_distribution.data.frame <- function(
  x,
  select = NULL,
  exclude = NULL,
  centrality = "mean",
  dispersion = TRUE,
  iqr = TRUE,
  range = TRUE,
  quartiles = FALSE,
  include_factors = FALSE,
  ci = NULL,
  iterations = 100,
  threshold = 0.1,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  by = NULL,
  ...
) {
  select <- .select_nse(
    select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  # check for reserved variable names
  .check_for_reserved_names(select)

  # tell user about bootstrapping and appropriate number of iterations
  .show_iterations_warning(verbose, iterations, ci)

  if (!is.null(by)) {
    if (!is.character(by)) {
      insight::format_error("`by` must be a character vector.")
    }
    x <- data_group(x, by)
    out <- describe_distribution(
      x,
      select = select,
      exclude = exclude,
      centrality = centrality,
      dispersion = dispersion,
      iqr = iqr,
      range = range,
      quartiles = quartiles,
      include_factors = include_factors,
      ci = ci,
      iterations = iterations,
      threshold = threshold,
      ignore_case = ignore_case,
      regex = regex,
      verbose = verbose
    )
    out <- data_ungroup(out)
    return(out)
  }

  # The function currently doesn't support descriptive summaries for character
  # or factor types.
  out <- do.call(
    rbind,
    lapply(x[select], function(i) {
      if (
        (include_factors && is.factor(i)) || (!is.character(i) && !is.factor(i))
      ) {
        describe_distribution(
          i,
          centrality = centrality,
          dispersion = dispersion,
          iqr = iqr,
          range = range,
          quartiles = quartiles,
          ci = ci,
          iterations = iterations,
          threshold = threshold,
          verbose = verbose,
          show_iterations_msg = FALSE
        )
      }
    })
  )

  if (is.null(out)) {
    return(NULL)
  }

  out$Variable <- row.names(out)
  row.names(out) <- NULL
  out <- out[c("Variable", setdiff(colnames(out), "Variable"))]

  class(out) <- unique(c(
    "parameters_distribution",
    "see_parameters_distribution",
    class(out)
  ))
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  attr(out, "ci") <- ci
  attr(out, "centrality") <- centrality
  attr(out, "threshold") <- threshold
  out
}


#' @export
describe_distribution.grouped_df <- function(
  x,
  select = NULL,
  exclude = NULL,
  centrality = "mean",
  dispersion = TRUE,
  iqr = TRUE,
  range = TRUE,
  quartiles = FALSE,
  include_factors = FALSE,
  ci = NULL,
  iterations = 100,
  threshold = 0.1,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  by = NULL,
  ...
) {
  if (!is.null(by)) {
    if (!is.character(by)) {
      insight::format_error("`by` must be a character vector.")
    }
    existing_grps <- setdiff(colnames(attributes(x)$groups), ".rows")
    x <- data_group(x, c(existing_grps, by))
  }
  group_vars <- setdiff(colnames(attributes(x)$groups), ".rows")
  group_data <- expand.grid(lapply(x[group_vars], function(i) unique(sort(i))))
  groups <- split(x, x[group_vars])
  groups <- Filter(function(x) nrow(x) > 0, groups)

  # check for reserved variable names
  .check_for_reserved_names(group_vars, type = "group_vars")

  select <- .select_nse(
    select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  # tell user about bootstrapping and appropriate number of iterations
  .show_iterations_warning(verbose, iterations, ci)

  out <- do.call(
    rbind,
    lapply(seq_along(groups), function(i) {
      d <- describe_distribution.data.frame(
        groups[[i]][select],
        centrality = centrality,
        dispersion = dispersion,
        iqr = iqr,
        range = range,
        quartiles = quartiles,
        include_factors = include_factors,
        ci = ci,
        iterations = iterations,
        threshold = threshold,
        verbose = verbose,
        show_iterations_msg = FALSE,
        ...
      )

      for (grp in seq_along(group_vars)) {
        d[[group_vars[grp]]] <- group_data[i, grp]
      }
      d <- data_relocate(d, group_vars, before = 1)

      d
    })
  )

  class(out) <- unique(c(
    "parameters_distribution",
    "see_parameters_distribution",
    class(out)
  ))
  attr(out, "object_name") <- deparse(substitute(x), width.cutoff = 500)
  attr(out, "ci") <- ci
  attr(out, "centrality") <- centrality
  attr(out, "threshold") <- threshold
  out
}


# methods ------------------

#' @export
print.parameters_distribution <- function(x, digits = 2, ...) {
  formatted_table <- format(
    x,
    digits = digits,
    format = "text",
    ci_width = NULL,
    ci_brackets = TRUE,
    ...
  )
  cat(insight::export_table(
    formatted_table,
    format = "text",
    digits = digits,
    ...
  ))
  invisible(x)
}


#' @export
print_md.parameters_distribution <- function(
  x,
  digits = 2,
  ci_brackets = c("(", ")"),
  ...
) {
  formatted_table <- format(
    x = x,
    digits = digits,
    format = "markdown",
    ci_width = NULL,
    ci_brackets = ci_brackets,
    ...
  )

  insight::export_table(
    formatted_table,
    format = "markdown",
    align = "firstleft",
    ...
  )
}


#' @export
print_html.parameters_distribution <- function(
  x,
  digits = 2,
  ci_brackets = c("(", ")"),
  ...
) {
  formatted_table <- format(
    x = x,
    digits = digits,
    format = "html",
    ci_width = NULL,
    ci_brackets = ci_brackets,
    ...
  )

  # determine backend
  backend <- .check_format_backend(...)

  # pass arguments to export_table
  fun_args <- list(
    formatted_table,
    format = backend,
    ...
  )

  # no "align" for format "tt" - this currently gives an error. Not sure
  # if related to insight::export_table or tinytable
  if (identical(backend, "html")) {
    fun_args$align <- "firstleft"
  }

  do.call(insight::export_table, fun_args)
}


#' @export
display.parameters_distribution <- function(
  object,
  format = "markdown",
  digits = 2,
  ...
) {
  format <- .display_default_format(format)

  fun_args <- list(
    x = object,
    digits = digits,
    ...
  )

  # print table in HTML or markdown format
  if (format %in% c("html", "tt")) {
    fun_args$backend <- format
    do.call(print_html, fun_args)
  } else {
    do.call(print_md, fun_args)
  }
}


#' @export
plot.parameters_distribution <- function(x, ...) {
  insight::check_if_installed("see")
  NextMethod()
}


# bootstrapping CIs ----------------------------------

.boot_distribution <- function(data, indices, centrality) {
  out <- datawizard::describe_distribution(
    data[indices],
    centrality = centrality,
    dispersion = FALSE,
    iqr = FALSE,
    range = FALSE,
    quartiles = FALSE,
    ci = NULL,
    verbose = FALSE
  )
  out[[1]]
}


# check centrality options ----------------------------------------

.centrality_options <- function(centrality) {
  if (identical(centrality, "all")) {
    c("mean", "median", "MAP")
  } else {
    centrality
  }
}


# sanity check ----------------------------------------

.check_for_reserved_names <- function(x, type = "select") {
  reserved_names <- c(
    "Variable",
    "CI_low",
    "CI_high",
    "n_Missing",
    "Q1",
    "Q3",
    "Quartiles",
    "Min",
    "Max",
    "Range",
    "Trimmed_Mean",
    "Trimmed",
    "Mean",
    "SD",
    "IQR",
    "Skewness",
    "Kurtosis",
    "n",
    "Median",
    "MAD",
    "MAP",
    "IQR",
    "n_Missing"
  )
  invalid_names <- intersect(reserved_names, x)

  if (length(invalid_names) > 0) {
    # adapt message to show user whether wrong variables appear in grouping or select
    msg <- switch(
      type,
      select = "with `describe_distribution()`: ",
      "as grouping variables in `describe_distribution()`: "
    )
    insight::format_error(paste0(
      "Following variable names are reserved and cannot be used ",
      msg,
      text_concatenate(invalid_names, enclose = "`"),
      ". Please rename these variables in your data."
    ))
  }
}


.show_iterations_warning <- function(verbose, iterations = 100, ci = NULL) {
  if (verbose && !is.null(ci)) {
    msg <- paste(
      "Bootstrapping confidence intervals using",
      iterations,
      "iterations, please be patient..."
    )
    if (iterations < 200) {
      msg <- c(
        msg,
        "For more stable intervals, increase the number of `iterations`, but note that this can also increase the computation time significantly."
      ) # nolint
    }
    insight::format_alert(msg)
  }
}
