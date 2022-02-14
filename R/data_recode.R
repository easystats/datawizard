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
data_recode.numeric <- function(x, split = "median", n_groups = NULL, lowest = 1, ...) {
  # evaluate split-function
  split <- substitute(split)

  if (is.numeric(eval(split))) {
    split <- eval(split)
  } else if (!is.character(split)) {
    split <- deparse(split)
  }

  # save
  original_x <- x

  # no missings
  x <- stats::na.omit(x)

  # stop if all NA
  if (!length(x)) {
    warning(insight::format_message("Variable contains only missing values. No recoding carried out."), call. = FALSE)
    return(original_x)
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

  # recode into groups
  out <- cut(x, breaks = cutoffs)
  levels(out) <- 1:nlevels(out)

  # fix lowest value, add back into original vector
  out <- as.numeric(out) - (min(out) - lowest)
  original_x[!is.na(original_x)] <- out

  original_x
}
