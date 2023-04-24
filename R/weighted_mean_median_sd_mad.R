#' Weighted Mean, Median, SD, and MAD
#'
#' @inheritParams stats::weighted.mean
#' @inheritParams stats::mad
#' @param weights A numerical vector of weights the same length as `x` giving
#'   the weights to use for elements of `x`. If `weights = NULL`, `x` is passed
#' to the non-weighted function.
#' @param verbose Show warning when `weights` are negative?
#' @inheritParams skewness
#'
#' @note Missing and infinite values are silently removed from `x` and `weights`.
#'
#' @examples
#' ## GPA from Siegel 1994
#' x <- c(3.7, 3.3, 3.5, 2.8)
#' wt <- c(5, 5, 4, 1) / 15
#'
#' weighted_mean(x, wt)
#' weighted_median(x, wt)
#'
#' weighted_sd(x, wt)
#' weighted_mad(x, wt)
#'
#' @export
weighted_mean <- function(x, weights = NULL, na.rm = TRUE, verbose = TRUE, ...) {
  if (!.are_weights(weights) || !.validate_weights(weights, verbose)) {
    return(mean(x, na.rm = na.rm))
  }

  # remove missings
  complete <- .clean_missings(x, weights, na.rm)
  stats::weighted.mean(complete$x, complete$weights, na.rm = na.rm)
}


#' @export
#' @rdname weighted_mean
weighted_median <- function(x, weights = NULL, na.rm = TRUE, verbose = TRUE, ...) {
  if (!.are_weights(weights) || !.validate_weights(weights, verbose)) {
    return(stats::median(x, na.rm = na.rm))
  }

  p <- 0.5 # split probability

  # remove missings
  complete <- .clean_missings(x, weights, na.rm)

  order <- order(complete$x)
  x <- complete$x[order]
  weights <- complete$weights[order]

  rw <- cumsum(weights) / sum(weights)
  # sanity check
  if (all(is.na(rw))) {
    return(NA_real_)
  }

  md.values <- min(which(rw >= p))

  if (rw[md.values] == p) {
    q <- mean(x[md.values:(md.values + 1)])
  } else {
    q <- x[md.values]
  }

  q
}


#' @export
#' @rdname weighted_mean
weighted_sd <- function(x, weights = NULL, na.rm = TRUE, verbose = TRUE, ...) {
  # from cov.wt
  if (!.are_weights(weights) || !.validate_weights(weights, verbose)) {
    return(stats::sd(x, na.rm = na.rm))
  }

  # remove missings
  complete <- .clean_missings(x, weights, na.rm)

  weights1 <- complete$weights / sum(complete$weights)
  center <- sum(weights1 * complete$x)
  xc <- sqrt(weights1) * (complete$x - center)
  var <- (t(xc) %*% xc) / (1 - sum(weights1^2))
  sqrt(as.vector(var))
}

#' @export
#' @rdname weighted_mean
weighted_mad <- function(x, weights = NULL, constant = 1.4826, na.rm = TRUE, verbose = TRUE, ...) {
  # From matrixStats
  if (!.are_weights(weights) || !.validate_weights(weights, verbose)) {
    return(stats::mad(x, na.rm = na.rm))
  }

  center <- weighted_median(x, weights = weights, na.rm = na.rm)
  x <- abs(x - center)
  constant * weighted_median(x, weights = weights, na.rm = na.rm)
}


# Utils -------------------------------------------------------------------

.validate_weights <- function(weights, verbose = TRUE) {
  pos <- all(weights > 0, na.rm = TRUE)

  if (isTRUE(!pos) && isTRUE(verbose)) {
    insight::format_warning("Some `weights` were negative. Weighting not carried out.")
  }

  pos
}

.clean_missings <- function(x, weights, na.rm) {
  if (isTRUE(na.rm)) {
    flag <- FALSE
    if (any(is.infinite(x)) || any(is.infinite(weights))) {
      # remove Inf
      x[is.infinite(x)] <- NA
      weights[is.infinite(weights)] <- NA
      flag <- TRUE
    }

    if (anyNA(x) || anyNA(weights)) {
      # remove missings
      x[is.na(weights)] <- NA
      weights[is.na(x)] <- NA
      flag <- TRUE
    }

    if (flag) {
      weights <- stats::na.omit(weights)
      x <- stats::na.omit(x)
    }
  }

  list(x = x, weights = weights)
}
