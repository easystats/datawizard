#' Summary Helpers
#'
#' @param x A numeric vector (or one that can be coerced to one via
#'   `as.numveric()`) to be summarized.
#' @param ... Not used.
#' @inheritParams stats::mad
#'
#' @return A vector of length 3: one SD below the mean, the mean, and one SD
#'   above the mean (or median and MAD).
#'
#' @examples
#' mean_sd(mtcars$mpg)
#'
#' median_mad(mtcars$mpg)
#'
#' @export
mean_sd <- function(x, na.rm = TRUE, named = TRUE, ...) {
  x <- as.numeric(x)

  M <- mean(x, na.rm = na.rm)
  S <- stats::sd(x, na.rm = na.rm)

  v <- M + c(-1, 0, 1) * S
  if (isTRUE(named)) {
    names(v) <- c("-SD", "Mean", "+SD")
  }
  v
}

#' @export
#' @rdname mean_sd
median_mad <- function(x, na.rm = TRUE, constant = 1.4826, named = TRUE, ...) {
  x <- as.numeric(x)

  M <- stats::median(x, na.rm = na.rm)
  S <- stats::mad(x, na.rm = na.rm, constant = constant)

  v <- M + c(-1, 0, 1) * S
  if (isTRUE(named)) {
    names(v) <- c("-MAD", "Median", "+MAD")
  }
  v
}
