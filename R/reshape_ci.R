#' Reshape CI between wide/long formats
#'
#' Reshape CI between wide/long formats.
#'
#' @param x A data frame containing columns named `CI_low` and `CI_high` (or
#'   similar, see `ci_type`).
#' @param ci_type String indicating the "type" (i.e. prefix) of the interval
#'   columns. Per *easystats* convention, confidence or credible intervals are
#'   named `CI_low` and `CI_high`, and the related `ci_type` would be `"CI"`.
#'   If column names for other intervals differ, `ci_type` can be used to
#'   indicate the name, e.g. `ci_type = "SI"` can be used for support intervals,
#'   where the column names in the data frame would be `SI_low` and `SI_high`.
#'
#' @return
#'
#' A data frame with columns corresponding to confidence intervals reshaped
#' either to wide or long format.
#'
#' @examples
#' x <- data.frame(
#'   Parameter = c("Term 1", "Term 2", "Term 1", "Term 2"),
#'   CI = c(.8, .8, .9, .9),
#'   CI_low = c(.2, .3, .1, .15),
#'   CI_high = c(.5, .6, .8, .85),
#'   stringsAsFactors = FALSE
#' )
#'
#' reshape_ci(x)
#' reshape_ci(reshape_ci(x))
#' @export

reshape_ci <- function(x, ci_type = "CI") {
  # define interval type
  ci_type <- match.arg(ci_type, choices = c("CI", "SI", "HDI", "ETI"))

  ci_low <- paste0(ci_type, "_low")
  ci_high <- paste0(ci_type, "_high")

  # Long to wide ----------------
  if (ci_low %in% names(x) && ci_high %in% names(x) && "CI" %in% names(x)) {
    ci_position <- which(names(x) == "CI")

    # Reshape
    if (length(unique(x$CI)) > 1) {
      if (!"Parameter" %in% names(x)) {
        x$Parameter <- NA
        remove_parameter <- TRUE
      } else {
        remove_parameter <- FALSE
      }

      x <- stats::reshape(
        x,
        idvar = "Parameter",
        timevar = "CI",
        direction = "wide",
        v.names = c(ci_low, ci_high),
        sep = "_"
      )
      row.names(x) <- NULL
      if (remove_parameter) x$Parameter <- NULL
    }

    # Replace at the right place
    ci_colname <- names(x)[grepl(paste0(ci_low, "_*"), names(x)) | grepl(paste0(ci_high, "_*"), names(x))]
    colnames_1 <- names(x)[0:(ci_position - 1)][!names(x)[0:(ci_position - 1)] %in% ci_colname]
    colnames_2 <- names(x)[!names(x) %in% c(ci_colname, colnames_1)]
    x <- x[c(colnames_1, ci_colname, colnames_2)]


    # Wide to long --------------
  } else {
    if (!"Parameter" %in% names(x)) {
      x$Parameter <- seq_len(nrow(x))
      remove_parameter <- TRUE
    } else {
      remove_parameter <- FALSE
    }

    lows <- grepl(paste0(ci_low, "_*"), names(x))
    highs <- grepl(paste0(ci_high, "_*"), names(x))
    ci <- as.numeric(gsub(paste0(ci_low, "_"), "", names(x)[lows]))
    if (paste0(ci, collapse = "-") != paste0(gsub(paste0(ci_high, "_"), "", names(x)[highs]), collapse = "-")) {
      insight::format_error("Something went wrong in the CIs reshaping.")
      return(x)
    }
    if (sum(lows) > 1 && sum(highs) > 1) {
      low <- stats::reshape(
        x[!highs],
        direction = "long",
        varying = list(names(x)[lows]),
        sep = "_",
        timevar = "CI",
        v.names = ci_low,
        times = ci
      )
      high <- stats::reshape(
        x[!lows],
        direction = "long",
        varying = list(names(x)[highs]),
        sep = "_",
        timevar = "CI",
        v.names = ci_high,
        times = ci
      )
      x <- merge(low, high)
      x$id <- NULL
      x <- x[order(x$Parameter), ]
      row.names(x) <- NULL
      if (remove_parameter) x$Parameter <- NULL
    }

    # Replace at the right place
    ci_position <- which(lows)[1]
    ci_colname <- c("CI", ci_low, ci_high)
    colnames_1 <- names(x)[0:(ci_position - 1)][!names(x)[0:(ci_position - 1)] %in% ci_colname]
    colnames_2 <- names(x)[!names(x) %in% c(ci_colname, colnames_1)]
    x <- x[c(colnames_1, ci_colname, colnames_2)]
  }

  class(x) <- intersect(c("data.frame", "numeric"), class(x))
  x
}
