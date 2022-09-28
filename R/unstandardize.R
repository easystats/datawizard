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
                                     ...) {
  if (!is.null(reference)) {
    i <- sapply(x, is.numeric)
    i <- i[i]
    reference <- reference[names(i)]
    if (robust) {
      center <- sapply(reference, stats::median, na.rm = TRUE)
      scale <- sapply(reference, stats::mad, na.rm = TRUE)
    } else {
      center <- sapply(reference, mean, na.rm = TRUE)
      scale <- sapply(reference, stats::sd, na.rm = TRUE)
    }
  } else if (is.null(center) || is.null(scale)) {
    i <- sapply(x, function(k) {
      is.numeric(k) && !is.null(a <- attributes(k)) && all(c("scale", "center") %in% names(a))
    })

    if (any(i)) {
      i <- i[i]
      center <- sapply(x[names(i)], attr, "center", exact = TRUE)
      scale <- sapply(x[names(i)], attr, "scale", exact = TRUE)
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
      i <- sapply(x, is.numeric)
      names(center) <- names(scale) <- names(x[i])
    }

    i <- names(x) %in% names(center)
    names(i) <- names(x)
    i <- i[i]
  }

  if (two_sd) {
    scale <- 2 * scale
  }

  # Select and deselect
  cols <- names(i)
  if (!is.null(select)) cols <- cols[cols %in% select]
  if (!is.null(exclude)) cols <- cols[!cols %in% exclude]

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
                                     ...) {
  insight::format_error("Cannot (yet) unstandardize a `grouped_df`.")
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
