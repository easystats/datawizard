data_recode <- function(x, split = "median", n_groups = NULL) {
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
    split <- unique(c(min(x), split, max(x)))
    out <- cut(x, breaks = split)
    levels(out) <- 1:nlevels(out)
  } else {
    cutoffs <- switch(
      split,
      "median" = stats::median(x),
      "mean" = mean(x),
      "quantile" = stats::quantile(x, probs = length(x) / (rev(seq(1:n_groups)) * length(x))),
    )

  }

}

a <- rev(seq(1:3))

