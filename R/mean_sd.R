#' Summary Helpers
#'
#' @param x A numeric vector (or one that can be coerced to one via
#'   `as.numeric()`) to be summarized.
#' @param named Should the vector be named?
#'   (E.g., `c("-SD" = -1, Mean = 1, "+SD" = 2)`.)
#' @param times How many SDs above and below the Mean (or MADs around the Median)
#' @param ... Not used.
#' @inheritParams coef_var
#' @inheritParams stats::mad
#'
#' @return A (possibly named) numeric vector of length `2*times + 1` of SDs
#'   below the mean, the mean, and SDs above the mean (or median and MAD).
#'
#' @examples
#' mean_sd(mtcars$mpg)
#'
#' mean_sd(mtcars$mpg, times = 2L)
#'
#' median_mad(mtcars$mpg)
#'
#' @export
mean_sd <- function(x, times = 1L, remove_na = TRUE, named = TRUE, ...) {
  .centrality_dispersion(x, type = "mean", times = times, remove_na = remove_na, named = named)
}

#' @export
#' @rdname mean_sd
median_mad <- function(x, times = 1L, remove_na = TRUE, constant = 1.4826, named = TRUE, ...) {
  .centrality_dispersion(x, type = "median", times = times, remove_na = remove_na, constant = constant, named = named)
}

#' @keywords Internal
.centrality_dispersion <- function(x,
                                   type = "mean",
                                   remove_na = TRUE,
                                   times = 1L,
                                   constant = 1.4826,
                                   named = TRUE,
                                   ...) {
  x <- as.numeric(x)
  times <- as.integer(times)
  type <- match.arg(type, choices = c("mean", "median"))

  # centrality
  M <- switch(type,
    median = stats::median(x, na.rm = remove_na),
    mean(x, na.rm = remove_na)
  )

  S <- switch(type,
    median = stats::mad(x, na.rm = remove_na, constant = constant),
    stats::sd(x, na.rm = remove_na)
  )

  v <- M + c(-rev(seq_len(times)), 0, seq_len(times)) * S

  if (isTRUE(named)) {
    string_cs <- switch(type,
      median = c("Median", "MAD"),
      c("Mean", "SD")
    )
    if (times == 1L) {
      times <- ""
    } else {
      times <- paste0(seq_len(times), " ")
    }
    names(v) <- c(
      paste0("-", rev(times), string_cs[2]),
      string_cs[1],
      paste0("+", times, string_cs[2])
    )
  }
  v
}
