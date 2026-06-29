# distribution ---------------------------------

#' @export
format.parameters_distribution <- function(
  x,
  digits = 2,
  format = NULL,
  ci_width = "auto",
  ci_brackets = TRUE,
  ...
) {
  # save information
  att <- attributes(x)

  if (all(c("Min", "Max") %in% names(x))) {
    x$Min <- insight::format_ci(
      x$Min,
      x$Max,
      ci = NULL,
      digits = digits,
      width = ci_width,
      brackets = ci_brackets
    )
    x$Max <- NULL
    colnames(x)[which(colnames(x) == "Min")] <- "Range"
  }

  if (all(c("Q1", "Q3") %in% names(x))) {
    x$Q1 <- insight::format_ci(
      x$Q1,
      x$Q3,
      ci = NULL,
      digits = digits,
      width = ci_width,
      brackets = FALSE
    )
    x$Q3 <- NULL
    colnames(x)[which(colnames(x) == "Q1")] <- "Quartiles"
  }

  # find CI columns. We might have multiple columns for different centralities
  ci_columns <- grepl("^(CI_low|CI_high)", colnames(x))
  # make sure we have matches
  if (any(ci_columns)) {
    # iterate all centrality options
    centrality <- .centrality_options(att$centrality)
    for (ce in centrality) {
      # this is the original column name
      ci_columns <- c(paste0("CI_low_", ce), paste0("CI_high_", ce))
      # we format CI column, merge it into one column
      x[[ci_columns[1]]] <- insight::format_ci(
        x[[ci_columns[1]]],
        x[[ci_columns[2]]],
        ci = NULL,
        digits = digits,
        width = ci_width,
        brackets = ci_brackets
      )
      # ... and remove the no longer needed CI_high column
      x[[ci_columns[2]]] <- NULL
      ci_lvl <- attributes(x)$ci

      # find position of CI column
      ci_columm_pos <- which(colnames(x) == ci_columns[1])

      # rename
      if (is.null(ci_lvl)) {
        colnames(x)[ci_columm_pos] <- sprintf(
          "CI (%s)",
          insight::format_capitalize(ce)
        )
      } else {
        colnames(x)[ci_columm_pos] <- sprintf(
          "%i%% CI (%s)",
          round(100 * ci_lvl),
          insight::format_capitalize(ce)
        )
      }

      # make sure we have the correct column name of the centrality
      centr_name <- switch(
        tolower(ce),
        mean = "Mean",
        median = "Median",
        map = "MAP"
      )

      # reorder CI column, move it to related centrality index
      centr_pos <- which(colnames(x) == centr_name)
      if (length(centr_pos)) {
        x <- data_relocate(x, select = ci_columm_pos, after = centr_pos)
      }
    }
  }

  if ("Trimmed_Mean" %in% colnames(x)) {
    threshold <- attributes(x)$threshold
    if (is.null(threshold)) {
      trim_name <- "Trimmed"
    } else {
      trim_name <- sprintf("Trimmed (%g%%)", round(100 * threshold))
    }
    colnames(x)[which(colnames(x) == "Trimmed_Mean")] <- trim_name
  }

  x
}
