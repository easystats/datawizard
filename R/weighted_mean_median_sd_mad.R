#' Weighted Mean, Median, SD, and MAD
#'
#' @inheritParams stats::weighted.mean
#' @inheritParams stats::mad
#' @param weights A numerical vector of weights the same length as `x` giving
#'   the weights to use for elements of `x`.
#' @param verbose Show warning when `weights` are negative?
#'
#' If `weights = NULL`, `x` is passed to the non-weighted function.
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
weighted_mean <- function(x, weights = NULL, verbose = TRUE, ...) {
  if (!.are_weights(weights) || !.validate_weights(weights, verbose)) {
    return(mean(x, na.rm = TRUE))
  }

  stats::weighted.mean(x, weights, na.rm = TRUE)
}


#' @export
#' @rdname weighted_mean
weighted_median <- function(x, weights = NULL, verbose = TRUE, ...) {
  # From spatstat + wiki
  if (!.are_weights(weights) || !.validate_weights(weights, verbose)) {
    return(stats::median(x, na.rm = TRUE))
  }

  oo <- order(x)
  x <- x[oo]
  weights <- weights[oo]
  Fx <- cumsum(weights) / sum(weights)

  lefties <- which(Fx <= 0.5)
  left <- max(lefties)

  if (length(lefties) == 0) {
    result <- x[1]
  } else if (left == length(x)) {
    result <- x[length(x)]
  } else {
    result <- x[left]

    if (!(Fx[left - 1] < 0.5 && 1 - Fx[left] < 0.5)) {
      right <- left + 1
      y <- x[left] * Fx[left] + x[right] * Fx[right]
      if (is.finite(y)) result <- y
    }
  }

  result
}

#' @export
#' @rdname weighted_mean
weighted_sd <- function(x, weights = NULL, verbose = TRUE, ...) {
  # from cov.wt
  if (!.are_weights(weights) || !.validate_weights(weights, verbose)) {
    return(stats::sd(x, na.rm = TRUE))
  }

  weights1 <- weights / sum(weights)
  center <- sum(weights1 * x)
  xc <- sqrt(weights1) * (x - center)
  var <- (t(xc) %*% xc) / (1 - sum(weights1^2))
  sqrt(as.vector(var))
}

#' @export
#' @rdname weighted_mean
weighted_mad <- function(x, weights = NULL, constant = 1.4826, verbose = TRUE, ...) {
  # From matrixStats
  if (!.are_weights(weights) || !.validate_weights(weights, verbose)) {
    return(stats::mad(x, na.rm = TRUE))
  }

  center <- weighted_median(x, weights = weights)
  x <- abs(x - center)
  constant * weighted_median(x, weights = weights)
}


# Utils -------------------------------------------------------------------

.validate_weights <- function(weights, verbode = TRUE) {
  pos <- all(weights > 0, na.rm = TRUE)

  if (isTRUE(!pos) && isTRUE(verbose)) {
    warning("Some `weights` were negative. Weighting not carried out.", call. = FALSE)
  }

  pos
}
