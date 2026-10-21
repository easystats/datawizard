#' Winsorize data
#'
#' @details
#'
#' Winsorizing or winsorization is the transformation of statistics by limiting
#' extreme values in the statistical data to reduce the effect of possibly
#' spurious outliers. The distribution of many statistics can be heavily
#' influenced by outliers. A typical strategy is to set all outliers (values
#' beyond a certain threshold) to a specified percentile of the data; for
#' example, a `90%` winsorization would see all data below the 5th percentile set
#' to the 5th percentile, and data above the 95th percentile set to the 95th
#' percentile. Winsorized estimators are usually more robust to outliers than
#' their more standard forms.
#'
#' Factors are returned unchanged unless `include_factors = TRUE`. In that
#' case, rare levels are merged into one level, `".other"`, which becomes the
#' last level. In an ordered factor, `".other"` is therefore the highest level.
#' A level is rare when its share of the non-missing values is less than
#' `threshold`. Values at an `NA` level, such as one added by [addNA()], are
#' missing values, and an `NA` level is never rare. Levels without values are
#' not rare. If fewer than two levels are rare, the factor is returned
#' unchanged. A level `".other"` that already exists is never rare, but when
#' levels are merged, its values join them. With `include_factors = TRUE`,
#' `method` must be `"percentile"`, so a data frame with a factor column gives
#' an error for any other method. Character and logical vectors are always
#' returned unchanged.
#'
#' @return
#'
#' A data frame with winsorized columns or a winsorized vector.
#'
#' @param data data frame or vector.
#' @param threshold The amount of winsorization, depends on the value of `method`:
#' - For `method = "percentile"`: the amount to winsorize from *each* tail.
#'   The value of `threshold` must be between 0 and 0.5 and of length 1. For
#'   factors with `include_factors = TRUE`, it is the share of non-missing
#'   values below which a level is rare.
#' - For `method = "zscore"`: the number of *SD*/*MAD*-deviations from the
#'   *mean*/*median* (see `robust`). The value of `threshold` must be greater
#'   than 0 and of length 1.
#' - For `method = "raw"`: a vector of length 2 with the lower and upper bound
#'   for winsorization.
#' @param method One of "percentile" (default), "zscore", or "raw". For factors
#'   with `include_factors = TRUE`, only "percentile" can be used.
#' @param robust Logical, if TRUE, winsorizing through the "zscore" method is
#'   done via the median and the median absolute deviation (MAD); if FALSE, via
#'   the mean and the standard deviation.
#' @param ... Currently not used.
#' @param verbose Not used anymore since `datawizard` 0.6.6.
#' @param include_factors Logical, if `TRUE`, rare levels of factors are merged
#'   into a level `".other"` (see 'Details'). Defaults to `FALSE`, which returns
#'   factors unchanged. Because it comes after `...`, it must be named in full,
#'   as in `include_factors = TRUE`. An unnamed or shortened argument is ignored.
#'
#' @examples
#' hist(iris$Sepal.Length, main = "Original data")
#'
#' hist(winsorize(iris$Sepal.Length, threshold = 0.2),
#'   xlim = c(4, 8), main = "Percentile Winsorization"
#' )
#'
#' hist(winsorize(iris$Sepal.Length, threshold = 1.5, method = "zscore"),
#'   xlim = c(4, 8), main = "Mean (+/- SD) Winsorization"
#' )
#'
#' hist(winsorize(iris$Sepal.Length, threshold = 1.5, method = "zscore", robust = TRUE),
#'   xlim = c(4, 8), main = "Median (+/- MAD) Winsorization"
#' )
#'
#' hist(winsorize(iris$Sepal.Length, threshold = c(5, 7.5), method = "raw"),
#'   xlim = c(4, 8), main = "Raw Thresholds"
#' )
#'
#' # Also works on a data frame:
#' winsorize(iris, threshold = 0.2)
#'
#' # Merge levels with less than 10% of the values:
#' x <- factor(c(rep("a", 10), rep("b", 8), "c", "d"))
#' winsorize(x, threshold = 0.1, include_factors = TRUE)
#'
#' @inherit data_rename seealso
#' @export
winsorize <- function(data, ...) {
  UseMethod("winsorize")
}


#' @rdname winsorize
#' @export
winsorize.factor <- function(
  data,
  threshold = 0.2,
  method = "percentile",
  ...,
  include_factors = FALSE
) {
  .check_include_factors(include_factors)
  if (!include_factors) {
    return(data)
  }

  .check_factor_args(threshold, method)

  # values at an NA level (see addNA()) are missing, too
  values <- as.character(data)
  n <- sum(!is.na(values))
  if (n == 0L) {
    return(data)
  }

  # levels other than NA and ".other" with at least one value and a share
  # below threshold
  lvls <- levels(data)
  candidates <- setdiff(lvls[!is.na(lvls)], ".other")
  counts <- tabulate(match(values, candidates), nbins = length(candidates))
  rare <- candidates[counts > 0 & counts / n < threshold]
  if (length(rare) < 2L) {
    return(data)
  }

  merged <- c(rare, ".other")
  new_levels <- c(setdiff(lvls, merged), ".other")
  values[values %in% merged] <- ".other"

  # rebuild the codes so that the class, an NA level, and other attributes
  # are kept
  out <- match(values, new_levels)
  attrs <- attributes(data)
  attrs$levels <- new_levels
  attributes(out) <- attrs
  out
}

#' @export
winsorize.character <- function(data, ...) {
  data
}

#' @export
winsorize.logical <- winsorize.character

#' @rdname winsorize
#' @export
winsorize.data.frame <- function(
  data,
  threshold = 0.2,
  method = "percentile",
  robust = FALSE,
  verbose = TRUE,
  ...,
  include_factors = FALSE
) {
  .check_include_factors(include_factors)
  data[] <- lapply(
    data,
    winsorize,
    threshold = threshold,
    method = method,
    robust = robust,
    verbose = verbose,
    include_factors = include_factors
  )
  data
}

#' @rdname winsorize
#' @export
winsorize.numeric <- function(
  data,
  threshold = 0.2,
  method = "percentile",
  robust = FALSE,
  verbose = TRUE,
  ...
) {
  method <- match.arg(method, choices = c("percentile", "zscore", "raw"))

  if (method == "raw" && length(threshold) != 2L) {
    insight::format_error(
      "`threshold` must be of length 2 for lower and upper bound."
    )
  }

  if (method == "percentile") {
    if (threshold < 0 || threshold > 0.5) {
      insight::format_error(
        "`threshold` for winsorization must be a scalar between 0 and 0.5."
      )
    }

    y <- sort(data)
    n <- length(data)
    ibot <- floor(threshold * n) + 1
    itop <- length(data) - ibot + 1

    threshold <- c(y[ibot], y[itop])
  }

  if (method == "zscore") {
    if (threshold <= 0) {
      insight::format_error(
        "'threshold' for winsorization must be a scalar greater than 0."
      )
    }

    if (isTRUE(robust)) {
      centeral <- stats::median(data, na.rm = TRUE)
      deviation <- stats::mad(data, center = centeral, na.rm = TRUE)
    } else {
      centeral <- mean(data, na.rm = TRUE)
      deviation <- stats::sd(data, na.rm = TRUE)
    }

    threshold <- centeral + c(-1, 1) * deviation * threshold
  }

  data[data < threshold[1]] <- threshold[1]
  data[data > threshold[2]] <- threshold[2]
  return(data)
}


.check_include_factors <- function(include_factors) {
  if (
    !is.logical(include_factors) ||
      length(include_factors) != 1L ||
      is.na(include_factors)
  ) {
    insight::format_error("`include_factors` must be `TRUE` or `FALSE`.")
  }
}


.check_factor_args <- function(threshold, method) {
  method <- match.arg(method, choices = c("percentile", "zscore", "raw"))
  if (method != "percentile") {
    insight::format_error(
      "Factors can only be winsorized with `method = \"percentile\"`."
    )
  }
  .check_factor_threshold(threshold)
}


.check_factor_threshold <- function(threshold) {
  if (
    !is.numeric(threshold) ||
      length(threshold) != 1L ||
      is.na(threshold) ||
      threshold < 0 ||
      threshold > 0.5
  ) {
    insight::format_error(
      "`threshold` for winsorization must be a scalar between 0 and 0.5."
    )
  }
}
