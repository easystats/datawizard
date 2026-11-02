#' @title Collapse sparse levels of categorical variables
#' @name collapse_levels
#'
#' @description
#' Merges sparse levels of factors and character vectors. A level is sparse if
#' it has fewer than `min_n` values or less than `min_prop` of the non-missing
#' values.
#'
#' @details
#' Nominal variables (unordered factors and character vectors): all sparse
#' levels merge into one last level. If that level is still sparse, the smallest
#' remaining level joins it until it is not, so a large level can lose its own
#' name. If all levels would merge into one, `x` is returned unchanged with a
#' warning.
#'
#' Ordered factors: the smallest level merges with the adjacent level that has
#' fewer values, until no level is sparse or two levels are left, with a warning
#' if a level is still sparse. On equal counts, it merges toward the middle of
#' the scale, and from the exact middle to the lower level.
#'
#' If `x` changes, levels unused in all selected columns are dropped, and a
#' factor loses the attribute `labels` and a contrast matrix. `NA` values and an
#' `NA` level (see [addNA()]) are not counted or merged. A grouped data frame is
#' counted as a whole, and its grouping columns stay unchanged.
#'
#' @param x A factor, a character vector, or a data frame.
#' @param min_prop A level with a smaller share of the non-missing values is
#'   sparse. A number between 0 and 1.
#' @param min_n A level with fewer values is sparse. A number of 0 or more.
#' @param other Name of the merged level of a nominal variable. If `NULL`, the
#'   merged levels are joined with `sep`. Ignored for ordered factors. If levels
#'   merge, it is an error if the name is already a used level.
#' @param sep String that joins the labels of merged levels.
#' @param pool Logical. If `TRUE`, the counts are summed over the selected
#'   columns, and all of them get the same merges and levels. Use this for the
#'   items of a scale. Ordered factors must have the same levels in the same
#'   order and cannot be mixed with other factors or character vectors.
#' @param verbose Toggle warnings and messages.
#' @param ... Not used.
#' @inheritParams extract_column_names
#' @inheritParams categorize
#'
#' @return `x`, with sparse levels merged. In data frames, only factors and
#'   character vectors change.
#'
#' @inheritSection center Selection of variables - the `select` argument
#'
#' @seealso [recode_values()] and [recode_into()] to recode by hand, and
#'   [winsorize()] to limit extreme values of numeric variables.
#'
#' @examples
#' x <- factor(rep(c("a", "b", "c", "d"), c(80, 14, 3, 3)))
#' table(collapse_levels(x))
#' table(collapse_levels(x, other = "Other"))
#'
#' # ordered factors merge with a neighbor
#' x <- factor(
#'   rep(c("low", "mid", "high"), c(3, 50, 47)),
#'   levels = c("low", "mid", "high"),
#'   ordered = TRUE
#' )
#' table(collapse_levels(x))
#'
#' # the items of a scale get the same merges
#' d <- data.frame(
#'   item1 = ordered(rep(1:3, c(2, 8, 90))),
#'   item2 = ordered(rep(1:3, c(6, 88, 6)))
#' )
#' table(collapse_levels(d, pool = TRUE)$item2)
#' @export
collapse_levels <- function(x, ...) {
  UseMethod("collapse_levels")
}


#' @export
collapse_levels.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_alert(
      "Only factors and character vectors have levels to collapse."
    )
  }
  x
}


#' @rdname collapse_levels
#' @export
collapse_levels.factor <- function(
  x,
  min_prop = 0.05,
  min_n = 0,
  other = NULL,
  sep = "/",
  verbose = TRUE,
  ...
) {
  .check_collapse_args(min_prop, min_n, other, sep)
  plan <- .collapse_plan(list(x), min_prop, min_n, other, sep, verbose)
  .collapse_apply(x, plan)
}


#' @rdname collapse_levels
#' @export
collapse_levels.character <- collapse_levels.factor


#' @rdname collapse_levels
#' @export
collapse_levels.data.frame <- function(
  x,
  select = NULL,
  exclude = NULL,
  min_prop = 0.05,
  min_n = 0,
  other = NULL,
  sep = "/",
  pool = FALSE,
  append = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
) {
  .check_collapse_args(min_prop, min_n, other, sep)
  if (!isTRUE(pool) && !isFALSE(pool)) {
    insight::format_error("`pool` must be `TRUE` or `FALSE`.")
  }

  # evaluate arguments
  select <- .select_nse(
    select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    remove_group_var = TRUE,
    verbose = verbose
  )
  # only factors and character vectors have levels
  select <- select[vapply(
    x[select],
    function(i) is.factor(i) || is.character(i),
    logical(1)
  )]

  # ".process_append()" creates the new variables and updates "select"
  if (!isFALSE(append) && length(select) > 0L) {
    my_args <- .process_append(x, select, append, append_suffix = "_r")
    x <- my_args$x
    select <- my_args$select
  }

  # with pool, all selected columns share one plan
  batches <- if (pool && length(select) > 0L) list(select) else as.list(select)
  for (i in batches) {
    where <- if (pool) "the selected columns" else paste0("column `", i, "`")
    where <- paste("in", where)
    plan <- .collapse_plan(x[i], min_prop, min_n, other, sep, verbose, where)
    x[i] <- lapply(x[i], .collapse_apply, plan = plan)
  }
  x
}


# which label each level gets, from the counts over all columns. NULL if the
# columns stay as they are.
.collapse_plan <- function(
  columns,
  min_prop,
  min_n,
  other,
  sep,
  verbose,
  where = "in `x`"
) {
  columns <- lapply(columns, function(i) {
    if (is.character(i)) factor(i) else i
  })
  own_levels <- lapply(columns, function(i) levels(i)[!is.na(levels(i))])
  is_ordered <- vapply(columns, is.ordered, logical(1))
  if (
    any(is_ordered) && (!all(is_ordered) || length(unique(own_levels)) > 1L)
  ) {
    insight::format_error(paste(
      "With `pool = TRUE`, ordered factors must have the same levels in the",
      "same order and can't be mixed with other factors or character vectors."
    ))
  }

  lvls <- unique(unlist(own_levels))
  counts <- Reduce(
    `+`,
    lapply(columns, function(i) {
      tabulate(match(levels(i), lvls)[unclass(i)], length(lvls))
    })
  )
  names(counts) <- lvls
  n <- sum(counts)
  is_sparse <- function(k) k < min_n | k / n < min_prop
  warn <- function(...) {
    if (isTRUE(verbose)) insight::format_warning(paste0(...))
  }
  if (n == 0L || !any(is_sparse(counts))) {
    return(NULL)
  }

  # a level is sparse, so min_n or min_prop is above 0, and unused levels are
  counts <- counts[counts > 0L]
  groups <- if (all(is_ordered)) {
    .collapse_ordered(counts, is_sparse)
  } else {
    .collapse_nominal(counts, is_sparse)
  }

  if (length(groups) == 1L && any(is_sparse(counts))) {
    warn(
      "Merging sparse levels ",
      where,
      " would leave only one level, so nothing was merged."
    )
    return(NULL)
  }

  new_names <- vapply(groups, paste, character(1), collapse = sep)
  merged <- lengths(groups) > 1L
  if (!all(is_ordered) && !is.null(other)) {
    new_names[merged] <- other
  }
  taken <- c(
    intersect(new_names[merged], names(counts)),
    new_names[duplicated(new_names)]
  )
  if (length(taken) > 0L) {
    insight::format_error(paste0(
      "The name of a merged level, `",
      taken[1],
      "`, is already taken. Use `sep`, or `other` for nominal variables, ",
      "to change it."
    ))
  }

  still_sparse <- is_sparse(vapply(
    groups,
    function(i) sum(counts[match(i, names(counts))]),
    numeric(1)
  ))
  if (any(still_sparse)) {
    warn(
      "Only two levels are left ",
      where,
      ", and still sparse: ",
      toString(paste0("`", new_names[still_sparse], "`")),
      "."
    )
  }

  plan <- stats::setNames(rep(new_names, lengths(groups)), unlist(groups))
  if (identical(unname(plan), lvls)) {
    return(NULL)
  }
  plan
}


.collapse_nominal <- function(counts, is_sparse) {
  sparse <- is_sparse(counts)
  # the merged level must not be sparse either
  while (any(sparse) && !all(sparse) && is_sparse(sum(counts[sparse]))) {
    sparse[which.min(replace(counts, sparse, Inf))] <- TRUE
  }
  c(
    as.list(names(counts)[!sparse]),
    if (any(sparse)) list(names(counts)[sparse])
  )
}


.collapse_ordered <- function(counts, is_sparse) {
  groups <- as.list(names(counts))
  sizes <- unname(counts)
  while (length(groups) > 2L && any(is_sparse(sizes))) {
    i <- which.min(sizes)
    k <- length(sizes)
    lower <- if (i > 1L) sizes[i - 1L] else Inf
    upper <- if (i < k) sizes[i + 1L] else Inf
    # merge with the smaller neighbor; a tie goes toward the middle position
    down <- lower < upper || (lower == upper && i >= (k + 1) / 2)
    j <- if (down) i - 1L else i
    groups[[j]] <- c(groups[[j]], groups[[j + 1L]])
    groups[[j + 1L]] <- NULL
    sizes[j] <- sizes[j] + sizes[j + 1L]
    sizes <- sizes[-(j + 1L)]
  }
  groups
}


.collapse_apply <- function(x, plan) {
  if (is.null(plan)) {
    return(x)
  }
  if (is.character(x)) {
    x[] <- plan[match(x, names(plan))]
    return(x)
  }

  new_levels <- unique(unname(plan))
  if (anyNA(levels(x))) {
    new_levels <- c(new_levels, NA)
  }
  out <- match(plan[match(levels(x), names(plan))], new_levels)[unclass(x)]

  # keep class and attributes, but drop labels and contrasts of the old levels
  attrs <- attributes(x)
  attrs$levels <- new_levels
  attrs$labels <- NULL
  if (is.matrix(attrs$contrasts)) {
    attrs$contrasts <- NULL
  }
  attributes(out) <- attrs
  out
}


.check_collapse_args <- function(min_prop, min_n, other, sep) {
  is_scalar <- function(i, mode) {
    is.vector(i, mode = mode) && length(i) == 1L && !is.na(i)
  }
  if (!is_scalar(min_prop, "numeric") || min_prop < 0 || min_prop > 1) {
    insight::format_error("`min_prop` must be a number between 0 and 1.")
  }
  if (!is_scalar(min_n, "numeric") || min_n < 0) {
    insight::format_error("`min_n` must be a number of 0 or more.")
  }
  if (!is.null(other) && !is_scalar(other, "character")) {
    insight::format_error("`other` must be `NULL` or a string.")
  }
}
