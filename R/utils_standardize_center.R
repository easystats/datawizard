# preparation for standardize and center ----
#
# Performs some preparation when standardizing or centering variables,
# like finding the center or scale, also in relation to some reference values.
# This function is applied to *vectors*.
#
#' @keywords internal
.process_std_center <- function(
  x,
  weights,
  robust,
  verbose = TRUE,
  reference = NULL,
  center = NULL,
  scale = NULL,
  cluster = NULL
) {
  cluster_vars <- NULL
  if (!is.null(cluster)) {
    cluster_vars <- .cluster_list(cluster)
    if (any(lengths(cluster_vars) != length(x))) {
      insight::format_error(
        "`cluster` must have the same length as the variable to transform."
      )
    }
    cluster <- .cluster_id(cluster_vars)
    if (verbose) {
      .check_cluster_levels(x, cluster_vars)
    }
  }

  # Warning if all NaNs
  if (all(is.na(x) | is.infinite(x))) {
    return(NULL)
  }

  if (!is.null(cluster)) {
    valid_x <- !is.na(x) & !is.infinite(x) & !is.na(cluster)
    na_values <- is.na(x) | is.na(cluster)
    inf_values <- is.infinite(x)
    vals <- x[valid_x]
  } else if (.are_weights(weights)) {
    valid_x <- !is.na(x) &
      !is.na(weights) &
      !is.infinite(x) &
      !is.infinite(weights)
    na_values <- is.na(x) | is.na(weights)
    inf_values <- is.infinite(x) | is.infinite(weights)
    vals <- x[valid_x]
    weights <- weights[valid_x]
  } else {
    valid_x <- !is.na(x) & !is.infinite(x)
    na_values <- is.na(x)
    inf_values <- is.infinite(x)
    vals <- x[valid_x]
  }

  # validation checks. With `cluster`, count only the values that center and
  # scale are computed from.
  check <- .check_standardize_numeric(
    if (is.null(cluster)) x else x[valid_x],
    name = NULL,
    verbose = verbose,
    reference = reference,
    center = center
  )

  if (is.factor(vals) || is.character(vals)) {
    vals <- .factor_to_numeric(vals)
  }

  # with clusters, center and scale are computed from one value per cluster.
  # Like missing values, infinite values are left out. `center()` passes
  # `scale = NULL` and needs one cluster; `standardize()` needs two for the SD.
  ref_vals <- vals
  if (!is.null(cluster)) {
    ref_vals <- .cluster_values(
      vals,
      cluster[valid_x],
      min_clusters = if (is.null(scale)) 1L else 2L
    )
  }

  # Get center and scale
  ref <- .get_center_scale(
    ref_vals,
    robust,
    weights,
    reference,
    .center = center,
    .scale = scale,
    verbose = verbose
  )

  list(
    vals = vals,
    valid_x = valid_x,
    center = ref$center,
    scale = ref$scale,
    check = check,
    na_values = na_values,
    inf_values = inf_values
  )
}


# cluster-level values ----
#
# Reduces `x` to one value per cluster. `x` and `cluster` must not contain
# missing or infinite values: `.process_std_center()` passes only the rows in
# `valid_x`. Gives an error if there are fewer than `min_clusters` clusters.
#
#' @keywords internal
.cluster_values <- function(x, cluster, min_clusters = 2L) {
  cluster_pairs <- unique(data.frame(cluster = cluster, x = x))
  if (anyDuplicated(cluster_pairs$cluster) > 0) {
    insight::format_error(
      "The variable takes more than one value within a cluster. With `cluster`, it must have one value per cluster."
    )
  }
  if (nrow(cluster_pairs) < min_clusters) {
    insight::format_error(paste0(
      "`cluster` must contain at least ",
      if (min_clusters == 1L) "one cluster" else "two clusters",
      " with finite, non-missing values."
    ))
  }
  cluster_pairs$x
}


#' @keywords internal
.check_cluster_args <- function(
  cluster,
  weights = NULL,
  reference = NULL,
  center = NULL,
  scale = NULL
) {
  if (is.null(cluster)) {
    return(invisible(NULL))
  }
  if (!is.null(weights)) {
    insight::format_error("`cluster` cannot be combined with `weights`.")
  }
  # data frame methods pass `NA` when no center or scale was given
  has_value <- function(i) is.numeric(i) && !all(is.na(i))
  if (!is.null(reference) || has_value(center) || has_value(scale)) {
    insight::format_error(
      "`cluster` cannot be combined with `reference`, or with values for `center` or `scale`."
    )
  }
  invisible(NULL)
}


#' @keywords internal
.check_cluster_column <- function(x, cluster) {
  missing_columns <- setdiff(cluster, colnames(x))
  if (length(missing_columns) == 1L) {
    insight::format_error(
      paste0("Could not find the `cluster` column `", missing_columns, "`.")
    )
  } else if (length(missing_columns) > 1L) {
    insight::format_error(paste0(
      "Could not find the `cluster` columns ",
      text_concatenate(missing_columns, enclose = "`"),
      "."
    ))
  }
}


# several cluster variables ----
#
# `.cluster_list()` turns `cluster` into a named list of id vectors: a list or
# data frame keeps its elements (unnamed ones are named by position), any
# other vector becomes a list of one. `.cluster_id()` combines the vectors
# into one id per row, so that rows with equal ids on every vector form one
# cluster. It builds the id from per-vector codes, because `interaction()`
# merges ids such as "a.b" with "c" and "a" with "b.c". The id is `NA` where
# any vector is `NA`.
#
#' @keywords internal
.cluster_list <- function(cluster) {
  if (is.list(cluster)) {
    cluster <- as.list(cluster)
  } else {
    cluster <- list(cluster)
  }
  if (is.null(names(cluster))) {
    names(cluster) <- rep("", length(cluster))
  }
  unnamed <- !nzchar(names(cluster))
  names(cluster)[unnamed] <- as.character(which(unnamed))
  cluster
}


#' @keywords internal
.cluster_id <- function(cluster_vars) {
  if (length(cluster_vars) == 1L) {
    return(cluster_vars[[1]])
  }
  codes <- lapply(cluster_vars, function(i) match(i, unique(i)))
  key <- do.call(paste, c(codes, sep = "_"))
  id <- match(key, unique(key))
  id[Reduce(`|`, lapply(cluster_vars, is.na))] <- NA
  id
}


# cluster variables that are not needed ----
#
# With two or more cluster variables, a cluster variable is not needed when
# dropping it leaves fewer clusters and `x` still takes one value per cluster
# of the others. It then only splits clusters of the others, and each split
# cluster counts once per part. Clusters are counted among the values with
# finite, non-missing `x` and no missing id. `rows` holds the row indices of
# each group of a grouped data frame, so that one warning names every cluster
# variable that is not needed in any group.
#
#' @keywords internal
.check_cluster_levels <- function(
  x,
  cluster_vars,
  name = NULL,
  rows = list(seq_along(x))
) {
  if (length(cluster_vars) < 2L) {
    return(invisible(NULL))
  }
  not_needed <- character(0)
  for (r in rows) {
    x_rows <- x[r]
    vars_rows <- lapply(cluster_vars, `[`, r)
    valid <- !is.na(x_rows) & !Reduce(`|`, lapply(vars_rows, is.na))
    if (is.numeric(x_rows)) {
      valid <- valid & is.finite(x_rows)
    }
    not_needed <- union(
      not_needed,
      .cluster_not_needed(x_rows[valid], lapply(vars_rows, `[`, valid))
    )
  }
  if (length(not_needed)) {
    labels <- ifelse(
      grepl("^[0-9]+$", not_needed),
      paste("cluster variable", not_needed),
      paste0("`", not_needed, "`")
    )
    subject <- if (is.null(name)) "The variable" else paste0("`", name, "`")
    insight::format_warning(paste0(
      subject,
      " takes one value per cluster even without ",
      text_concatenate(labels, last = " or "),
      " in `cluster`. Such a cluster variable only splits clusters, and each split cluster then counts once per part. Remove it from `cluster`, or set `verbose = FALSE` to silence this warning." # nolint
    ))
  }
  invisible(NULL)
}


#' @keywords internal
.cluster_not_needed <- function(x, cluster_vars) {
  n_clusters <- length(unique(.cluster_id(cluster_vars)))
  out <- character(0)
  for (i in seq_along(cluster_vars)) {
    pairs <- unique(data.frame(
      cluster = .cluster_id(cluster_vars[-i]),
      x = x
    ))
    if (nrow(pairs) < n_clusters && anyDuplicated(pairs$cluster) == 0L) {
      out <- c(out, names(cluster_vars)[i])
    }
  }
  out
}


# processing and checking of arguments ----
#
# Performs some preparation when standardizing or centering variables,
# like finding the center or scale, also in relation to some reference values.
# This function is applied to the *data frame methods*.
#
#' @keywords internal
.process_std_args <- function(
  x,
  select,
  exclude,
  weights,
  append,
  append_suffix = "_z",
  keep_factors,
  remove_na = "none",
  reference = NULL,
  .center = NULL,
  .scale = NULL,
  keep_character = FALSE,
  preserve_value_labels = FALSE,
  cluster = NULL
) {
  # check append argument, and set default
  if (isFALSE(append)) {
    append <- NULL
  } else if (isTRUE(append)) {
    append <- append_suffix
  }

  # a character vector of column names names the cluster columns, which are
  # never transformed. A character vector with one id per row that is not all
  # column names holds id values.
  cluster_name <- NULL
  if (!is.null(cluster)) {
    # check here, because `.scale` is dropped below when no `.center` is given
    .check_cluster_args(cluster, weights, reference, .center, .scale)
    is_names <- is.character(cluster) &&
      (all(cluster %in% colnames(x)) ||
        nrow(x) < 2L ||
        length(cluster) != nrow(x))
    if (is_names) {
      .check_cluster_column(x, cluster)
      cluster_name <- cluster
      exclude <- c(exclude, cluster)
    } else if (any(lengths(.cluster_list(cluster)) != nrow(x))) {
      insight::format_error(
        "`cluster` must have the same length as the number of rows in the data."
      )
    }
  }

  if (!is.null(weights) && is.character(weights)) {
    if (weights %in% colnames(x)) {
      exclude <- c(exclude, weights)
    } else {
      insight::format_warning(
        paste0(
          "Could not find weighting column `",
          weights,
          "`. Weighting not carried out."
        )
      )
      weights <- NULL
    }
  }

  select <- .select_variables(x, select, exclude, keep_factors, keep_character)

  # check if selected variables are in reference
  if (!is.null(reference) && !all(select %in% names(reference))) {
    insight::format_error(
      "The `reference` must include all variables from `select`."
    )
  }

  # copy label attributes
  variable_labels <- insight::compact_list(lapply(
    x,
    attr,
    "label",
    exact = TRUE
  ))
  value_labels <- NULL
  if (preserve_value_labels) {
    value_labels <- insight::compact_list(lapply(
      x,
      attr,
      "labels",
      exact = TRUE
    ))
  }

  # drop NAs
  remove_na <- match.arg(remove_na, c("none", "selected", "all"))

  omit <- switch(
    remove_na,
    none = logical(nrow(x)),
    selected = rowSums(vapply(x[select], is.na, FUN.VALUE = logical(nrow(x)))) >
      0,
    all = rowSums(vapply(x, is.na, FUN.VALUE = logical(nrow(x)))) > 0
  )
  x <- x[!omit, , drop = FALSE]

  if (!is.null(weights) && is.character(weights)) {
    weights <- x[[weights]]
  }

  if (!is.null(cluster_name)) {
    cluster <- as.list(x[cluster_name])
  } else if (!is.null(cluster)) {
    cluster <- lapply(.cluster_list(cluster), function(i) i[!omit])
  }

  # append standardized variables
  if (!is.null(append) && append != "") {
    new_variables <- x[select]
    colnames(new_variables) <- paste0(colnames(new_variables), append)
    if (length(variable_labels)) {
      variable_labels <- c(
        variable_labels,
        stats::setNames(variable_labels[select], colnames(new_variables))
      )
    }
    if (length(value_labels)) {
      value_labels <- c(
        value_labels,
        stats::setNames(value_labels[select], colnames(new_variables))
      )
    }
    x <- cbind(x, new_variables)
    select <- colnames(new_variables)
  }

  # check for reference center and scale. A single TRUE or FALSE is passed on
  # to each column as it is, as the numeric methods take it.
  is_flag <- function(i) is.logical(i) && length(i) == 1L && !is.na(i)
  if (
    (is_flag(.center) || is.null(.center)) &&
      (is_flag(.scale) || is.null(.scale)) &&
      (is_flag(.center) || is_flag(.scale))
  ) {
    .center <- stats::setNames(
      rep(if (is_flag(.center)) .center else NA, length(select)),
      select
    )
    .scale <- stats::setNames(
      rep(if (is_flag(.scale)) .scale else NA, length(select)),
      select
    )
  } else if (!is.null(.center)) {
    # for center(), we have no scale - set it to default value
    if (is.null(.scale)) {
      .scale <- rep(1, length(.center))
    }

    # center and scale must have same length
    if (length(.center) != length(.scale)) {
      insight::format_error("`center` and `scale` must be of same length.")
    }

    # center and scale must either be of length 1 or of same length as selected variables
    if (length(.center) > 1 && length(.center) != length(select)) {
      insight::format_error(
        "`center` and `scale` must have the same length as the selected variables for standardization or centering."
      )
    }

    # if of length 1, recycle
    if (length(.center) == 1) {
      .center <- rep(.center, length(select))
      .scale <- rep(.scale, length(select))
    }

    # set names
    if (is.null(names(.center))) {
      .center <- stats::setNames(.center, select)
    }
    if (is.null(names(.scale))) {
      .scale <- stats::setNames(.scale, select)
    }
  } else {
    # use NA if missing, so we can index these as vectors
    .center <- stats::setNames(rep(NA, length(select)), select)
    .scale <- stats::setNames(rep(NA, length(select)), select)
  }

  # add back variable labels
  if (length(variable_labels)) {
    for (i in names(variable_labels)) {
      attr(x[[i]], "label") <- variable_labels[[i]]
    }
  }

  if (preserve_value_labels && length(value_labels)) {
    for (i in names(value_labels)) {
      attr(x[[i]], "labels") <- value_labels[[i]]
    }
  }

  list(
    x = x,
    select = select,
    exclude = exclude,
    weights = weights,
    append = append,
    center = .center,
    scale = .scale,
    cluster = cluster
  )
}


# retrieve center and scale information ----
#' @keywords internal
.get_center_scale <- function(
  x,
  robust = FALSE,
  weights = NULL,
  reference = NULL,
  .center = NULL,
  .scale = NULL,
  verbose = TRUE
) {
  if (is.null(reference)) {
    reference <- x
  }

  # for center(), we have no scale. default to 1
  if (is.null(.scale) || is.na(.scale) || isFALSE(.scale)) {
    scale <- 1
  } else if (isTRUE(.scale)) {
    if (robust) {
      scale <- weighted_mad(reference, weights)
    } else {
      scale <- weighted_sd(reference, weights)
    }
  } else {
    # we must have a numeric value here
    scale <- .scale
  }

  # process center
  if (is.null(.center) || is.na(.center) || isFALSE(.center)) {
    center <- 0
  } else if (isTRUE(.center)) {
    if (robust) {
      center <- weighted_median(reference, weights)
    } else {
      center <- weighted_mean(reference, weights)
    }
  } else {
    # we must have a numeric value here
    center <- .center
  }

  if (scale == 0) {
    scale <- 1
    if (verbose) {
      insight::format_warning(sprintf(
        "%s is 0 - variable not standardized (only scaled).",
        if (robust) "MAD" else "SD"
      ))
    }
  }

  list(center = center, scale = scale)
}


# check range of input variables ----
#' @keywords internal
.check_standardize_numeric <- function(
  x,
  name = NULL,
  verbose = TRUE,
  reference = NULL,
  center
) {
  # Warning if only one value
  if (
    insight::has_single_value(x) &&
      is.null(reference) &&
      (is.null(center) || isTRUE(center))
  ) {
    if (verbose) {
      if (is.null(name)) {
        insight::format_alert(
          "The variable contains only one unique value and will be set to 0."
        )
      } else {
        insight::format_alert(
          paste0(
            "The variable `",
            name,
            "` contains only one unique value and will be set to 0."
          )
        )
      }
    }
    return(NULL)
  }

  # Warning if logical vector
  if (
    verbose && insight::n_unique(x) == 2 && !is.factor(x) && !is.character(x)
  ) {
    if (is.null(name)) {
      insight::format_alert(
        "The variable contains only two different values. Consider converting it to a factor."
      )
    } else {
      insight::format_alert(
        paste0(
          "Variable `",
          name,
          "` contains only two different values. Consider converting it to a factor."
        )
      )
    }
  }
  x
}


# process append argument ----
#' @keywords internal
.process_append <- function(
  x,
  select,
  append,
  append_suffix = "_z",
  preserve_value_labels = FALSE,
  keep_factors = TRUE,
  keep_character = FALSE
) {
  # check append argument, and set default
  if (isFALSE(append)) {
    append <- NULL
  } else if (isTRUE(append)) {
    append <- append_suffix
  }

  # append recoded variables
  if (!is.null(append) && append != "") {
    # keep or drop factors and characters
    select <- .select_variables(
      x,
      select,
      exclude = NULL,
      keep_factors = keep_factors,
      keep_character = keep_character
    )

    # copy label attributes
    variable_labels <- insight::compact_list(lapply(
      x,
      attr,
      "label",
      exact = TRUE
    ))
    value_labels <- NULL
    if (preserve_value_labels) {
      value_labels <- insight::compact_list(lapply(
        x,
        attr,
        "labels",
        exact = TRUE
      ))
    }

    # add new variables that sould be appended
    new_variables <- x[select]
    colnames(new_variables) <- paste0(colnames(new_variables), append)
    if (length(variable_labels)) {
      variable_labels <- c(
        variable_labels,
        stats::setNames(variable_labels[select], colnames(new_variables))
      )
    }
    if (length(value_labels)) {
      value_labels <- c(
        value_labels,
        stats::setNames(value_labels[select], colnames(new_variables))
      )
    }
    x <- cbind(x, new_variables)
    select <- colnames(new_variables)

    # add back variable labels
    if (length(variable_labels)) {
      for (i in names(variable_labels)) {
        attr(x[[i]], "label") <- variable_labels[[i]]
      }
    }

    if (preserve_value_labels && length(value_labels)) {
      for (i in names(value_labels)) {
        attr(x[[i]], "labels") <- value_labels[[i]]
      }
    }
  }
  list(x = x, select = select)
}


# variables to standardize and center ----
#
# This function mainly serves the purpose to keep or drop factors and
# character vectors from transformation functions.
#
#' @keywords internal
.select_variables <- function(
  x,
  select,
  exclude,
  keep_factors,
  keep_character = FALSE
) {
  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  if (!keep_factors) {
    if (!keep_character) {
      factors <- vapply(
        x[select],
        function(i) is.factor(i) | is.character(i),
        FUN.VALUE = logical(1L)
      )
    } else {
      factors <- vapply(x[select], is.factor, FUN.VALUE = logical(1L))
    }
    select <- select[!factors]
  }

  select
}


# for grouped df ---------------------------
#' @keywords internal
.process_grouped_df <- function(
  x,
  select,
  exclude,
  append,
  append_suffix = "_z",
  reference,
  weights,
  keep_factors,
  cluster = NULL
) {
  if (!is.null(cluster)) {
    if (!is.character(cluster)) {
      insight::format_error(
        "For grouped data frames, `cluster` must be a column name or a character vector of column names."
      )
    }
    .check_cluster_args(cluster, weights, reference)
    .check_cluster_column(x, cluster)
    exclude <- c(exclude, cluster)
  }

  if (!is.null(reference)) {
    insight::format_error(
      "The `reference` argument cannot be used with grouped standardization for now."
    )
  }

  # check append argument, and set default
  if (isFALSE(append)) {
    append <- NULL
  } else if (isTRUE(append)) {
    append <- append_suffix
  }

  info <- attributes(x)

  grps <- attr(x, "groups", exact = TRUE)[[".rows"]]

  # for grouped data frames, we can decide to remove group variable from selection
  grp_vars <- setdiff(colnames(attr(x, "groups", exact = TRUE)), ".rows")

  if (is.numeric(weights)) {
    insight::format_warning(
      "For grouped data frames, `weights` must be a character, not a numeric vector.",
      "Ignoring weightings."
    )
    weights <- NULL
  }

  x <- as.data.frame(x)
  select <- .select_variables(x, select, exclude, keep_factors)
  select <- setdiff(select, grp_vars)

  # append standardized variables
  if (!is.null(append) && append != "") {
    new_variables <- x[select]
    colnames(new_variables) <- paste0(colnames(new_variables), append)
    x <- cbind(x, new_variables)
    select <- colnames(new_variables)
    info$names <- c(info$names, select)
  }

  list(x = x, info = info, select = select, grps = grps, weights = weights)
}
