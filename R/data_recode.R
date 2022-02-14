#' @title Recode data
#' @name data_recode
#'
#' @description
#' Recode data.
#'
#' @param x A data frame or vector.
#' @param split Name of a function, or numeric values, indicating cutoffs
#' @param n_groups If `split = "quantile"`.
#' @param lowest Minimum value if numeric variables are recoded.
#' @param ... not used.
#'
#' @examples
#' set.seed(123)
#' x <- sample(1:10, size = 50, replace = TRUE)
#'
#' table(x)
#'
#' # by default, at median
#' table(data_recode(x))
#'
#' # into 3 groups, based on distribution (quantiles)
#' table(data_recode(x, split = "quantile", n_groups = 3))
#'
#' # into 3 groups, try to return similar group sizes
#' # (i.e. similar count for each value/level)
#' table(data_recode(x, split = "quantile", n_groups = 3))
#'
#' # into 3 groups, manual cut offs
#' table(data_recode(x, split = c(3, 5)))
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

  # handle aliases
  if (identical(split, "equal_size")) {
    split <- "equal"
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
    cutoffs <- split
  } else {
    cutoffs <- switch(
      split,
      "median" = stats::median(x),
      "mean" = mean(x),
      "quantile" = stats::quantile(x, probs = length(x) / (rev(seq(1:n_groups)) * length(x))),
      "equal" = .equal_groups(x, n_groups),
      NULL
    )
  }

  # complete ranges, including minimum and maximum
  cutoffs <- unique(c(min(x), cutoffs, max(x)))

  # recode into groups
  out <- droplevels(cut(x, breaks = cutoffs, include.lowest = TRUE, right = !identical(split, "equal")))
  levels(out) <- 1:nlevels(out)

  # fix lowest value, add back into original vector
  out <- as.numeric(out)
  out <- out - (min(out) - lowest)
  original_x[!is.na(original_x)] <- out

  original_x
}




# tools --------------------

.equal_groups <- function(x, n_groups) {
  nominator <- seq_len(n_groups - 1)
  denominator <- rep(n_groups, length(nominator))
  qu_prob <- nominator / denominator
  stats::quantile(x, probs = qu_prob)
}
