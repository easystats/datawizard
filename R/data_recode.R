#' @title Recode data
#' @name data_recode
#'
#' @description
#' Recode data.
#'
#' @param x A data frame or vector.
#' @param split Name of a function, or numeric values, indicating cutoffs
#' @param n_groups If `split = "quantile"`.
#'
#' @export
data_recode <- function(x, ...) {
  UseMethod("data_recode")
}

#' @rdname data_recode
#' @export
data_recode.numeric <- function(x, split = "median", n_groups = NULL) {
  # evaluate split-function
  split <- substitute(split)

  if (is.numeric(eval(split))) {
    split <- eval(split)
  } else if (!is.character(split)) {
    split <- deparse(split)
  }

  # no missings
  x <- stats::na.omit(x)

  # stop if all NA
  if (!length(x)) {

  }

  if (is.numeric(split)) {
    cutoffs <- unique(c(min(x), split, max(x)))
  } else {
    cutoffs <- switch(
      split,
      "median" = stats::median(x),
      "mean" = mean(x),
      "quantile" = stats::quantile(x, probs = length(x) / (rev(seq(1:n_groups)) * length(x))),
    )
  }

  # complete ranges, including minimum and maximum
  cutoffs <- c(min(x), cutoffs, max(x))

  out <- cut(x, breaks = cutoffs)
  levels(out) <- 1:nlevels(out)

  out
}
