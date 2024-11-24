#' @rdname standardize
#' @export
unstandardize <- function(x, ...) {
  UseMethod("unstandardize")
}

#' @rdname standardize
#' @export
unstandardise <- unstandardize

#' @rdname standardize
#' @export
unstandardize.numeric <- function(x,
                                  center = NULL,
                                  scale = NULL,
                                  reference = NULL,
                                  robust = FALSE,
                                  two_sd = FALSE,
                                  ...) {
  if (!is.null(reference)) {
    if (robust) {
      center <- stats::median(reference, na.rm = TRUE)
      scale <- stats::mad(reference, na.rm = TRUE)
    } else {
      center <- mean(reference, na.rm = TRUE)
      scale <- stats::sd(reference, na.rm = TRUE)
    }
  } else if (is.null(center) || is.null(scale)) {
    if (all(c("center", "scale") %in% names(attributes(x)))) {
      center <- attr(x, "center", exact = TRUE)
      scale <- attr(x, "scale", exact = TRUE)
      attr(x, "scale") <- attr(x, "center") <- NULL
    } else if (all(c("scaled:center", "scaled:scale") %in% names(attributes(x)))) {
      center <- attr(x, "scaled:center", exact = TRUE)
      scale <- attr(x, "scaled:scale", exact = TRUE)
      attr(x, "scaled:scale") <- attr(x, "scaled:center") <- NULL
    } else {
      insight::format_error("You must provide the arguments `center`, `scale` or `reference`.")
    }
  }

  if (two_sd) {
    scale <- 2 * scale
  }

  x * scale + center
}

#' @rdname standardize
#' @export
unstandardize.data.frame <- function(x,
                                     center = NULL,
                                     scale = NULL,
                                     reference = NULL,
                                     robust = FALSE,
                                     two_sd = FALSE,
                                     select = NULL,
                                     exclude = NULL,
                                     ignore_case = FALSE,
                                     regex = FALSE,
                                     verbose = TRUE,
                                     ...) {
  # Select and deselect
  cols <- .select_nse(
    select,
    x,
    exclude = exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  dots <- match.call(expand.dots = FALSE)[["..."]]

  if (!is.null(dots$grp_attr_dw)) {
    grp_attr_dw <- eval(dots$grp_attr_dw, envir = parent.frame(1L))
  } else {
    grp_attr_dw <- NULL
  }

  if (!is.null(grp_attr_dw)) {
    center <- vapply(cols, function(x) {
      grp_attr_dw[grep(paste0("^attr\\_", x, "\\.center"), names(grp_attr_dw))]
    }, FUN.VALUE = numeric(1L))
    scale <- vapply(cols, function(x) {
      grp_attr_dw[grep(paste0("^attr\\_", x, "\\.scale"), names(grp_attr_dw))]
    }, FUN.VALUE = numeric(1L))
    i <- vapply(x[, cols, drop = FALSE], is.numeric, FUN.VALUE = logical(1L))
  } else if (!is.null(reference)) {
    i <- vapply(x[, cols, drop = FALSE], is.numeric, FUN.VALUE = logical(1L))
    i <- i[i]
    reference <- reference[names(i)]
    if (robust) {
      center <- vapply(reference, FUN.VALUE = numeric(1L), stats::median, na.rm = TRUE)
      scale <- vapply(reference, FUN.VALUE = numeric(1L), stats::mad, na.rm = TRUE)
    } else {
      center <- vapply(reference, FUN.VALUE = numeric(1L), mean, na.rm = TRUE)
      scale <- vapply(reference, FUN.VALUE = numeric(1L), stats::sd, na.rm = TRUE)
    }
  } else if (is.null(center) || is.null(scale)) {
    i <- vapply(x[, cols, drop = FALSE], function(k) {
      a <- attributes(k)
      is.numeric(k) && !is.null(a) && all(c("scale", "center") %in% names(a))
    }, FUN.VALUE = logical(1L))

    if (any(i)) {
      i <- i[i]
      center <- vapply(x[names(i)], FUN.VALUE = numeric(1L), attr, "center", exact = TRUE)
      scale <- vapply(x[names(i)], FUN.VALUE = numeric(1L), attr, "scale", exact = TRUE)
    } else if (all(c("center", "scale") %in% names(attributes(x)))) {
      center <- attr(x, "center", exact = TRUE)
      scale <- attr(x, "scale", exact = TRUE)
      attr(x, "center") <- attr(x, "scale") <- NULL
      i <- names(x) %in% names(scale)
      i <- i[i]
    } else {
      insight::format_error("You must provide the arguments `center`, `scale` or `reference`.")
    }
  } else {
    if (is.null(names(center))) {
      i <- vapply(x, is.numeric, FUN.VALUE = logical(1L))
      names(center) <- names(scale) <- names(x[i])
    }

    i <- names(x) %in% names(center)
    names(i) <- names(x)
    i <- i[i]
  }

  if (two_sd) {
    scale <- 2 * scale
  }

  cols <- names(i)

  # Apply unstandardization to cols
  for (col in cols) {
    x[col] <- unstandardize(x[[col]], center = center[[col]], scale = scale[[col]])
  }
  x
}

#' @export
unstandardize.factor <- function(x, ...) {
  x
}

#' @export
unstandardize.character <- function(x, ...) {
  x
}



#' @export
unstandardize.grouped_df <- function(x,
                                     center = NULL,
                                     scale = NULL,
                                     reference = NULL,
                                     robust = FALSE,
                                     two_sd = FALSE,
                                     select = NULL,
                                     exclude = NULL,
                                     ignore_case = FALSE,
                                     regex = FALSE,
                                     verbose = TRUE,
                                     ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    remove_group_var = TRUE,
    verbose = verbose
  )

  info <- attributes(x)


  grps <- attr(x, "groups", exact = TRUE)[[".rows"]]

  x <- as.data.frame(x)

  for (i in select) {
    if (is.null(info$groups[[paste0("attr_", i)]])) {
      insight::format_error(
        paste(
          "Couldn't retrieve the necessary information to unstandardize",
          text_concatenate(i, enclose = "`")
        )
      )
    }
  }

  for (rows in seq_along(grps)) {
    # get the dw_transformer attributes for this group
    raw_attrs <- unlist(info$groups[rows, startsWith(names(info$groups), "attr")])
    if (length(select) == 1L) {
      names(raw_attrs) <- paste0("attr_", select, ".", names(raw_attrs))
    }

    tmp <- unstandardise(
      x[grps[[rows]], , drop = FALSE],
      center = center,
      scale = scale,
      reference = reference,
      robust = robust,
      two_sd = two_sd,
      select = select,
      exclude = exclude,
      ignore_case = ignore_case,
      regex = regex,
      verbose = verbose,
      grp_attr_dw = raw_attrs
    )
    x[grps[[rows]], ] <- tmp
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- utils::modifyList(info, attributes(x))
  class(x) <- c("grouped_df", class(x))
  x
}

#' @export
unstandardize.matrix <- function(x,
                                 center = NULL,
                                 scale = NULL,
                                 reference = NULL,
                                 robust = FALSE,
                                 two_sd = FALSE,
                                 ...) {
  if (all(c("scaled:center", "scaled:scale") %in% names(attributes(x)))) {
    center <- attr(x, "scaled:center", exact = TRUE)
    scale <- attr(x, "scaled:scale", exact = TRUE)
    attr(x, "scaled:center") <- attr(x, "scaled:scale") <- NULL

    for (col in seq_len(ncol(x))) {
      x[, col] <- unstandardize.numeric(x[, col], center = center[col], scale = scale[col])
    }
  } else {
    scales <- attr(x, "scale")
    centers <- attr(x, "center")

    xl <- lapply(seq_len(ncol(x)), function(i) {
      tmp <- x[, i]
      attributes(tmp) <- list(center = centers[i], scale = scales[i])
      tmp
    })

    xz <- lapply(xl, datawizard::unstandardize, ...)
    x_out <- do.call(cbind, xz)
    dimnames(x_out) <- dimnames(x)

    x <- x_out
  }
  x
}

#' @export
unstandardize.array <- unstandardize.matrix



# Datagrid ----------------------------------------------------------------

#' @export
unstandardize.datagrid <- function(x, ...) {
  x[names(x)] <- unstandardize(as.data.frame(x), reference = attributes(x)$data, ...)
  x
}

#' @export
unstandardize.visualisation_matrix <- unstandardize.datagrid
