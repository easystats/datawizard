# distribution ---------------------------------

#' @export
format.parameters_distribution <- function(x, digits = 2, format = NULL, ci_width = "auto", ci_brackets = TRUE, ...) {
  if (all(c("Min", "Max") %in% names(x))) {
    x$Min <- insight::format_ci(x$Min, x$Max, ci = NULL, digits = digits, width = ci_width, brackets = ci_brackets)
    x$Max <- NULL
    colnames(x)[which(colnames(x) == "Min")] <- "Range"
  }

  if (all(c("Q1", "Q3") %in% names(x))) {
    x$Q1 <- insight::format_ci(x$Q1, x$Q3, ci = NULL, digits = digits, width = ci_width, brackets = FALSE)
    x$Q3 <- NULL
    colnames(x)[which(colnames(x) == "Q1")] <- "Quartiles"
  }

  if (all(c("CI_low", "CI_high") %in% names(x))) {
    x$CI_low <- insight::format_ci(x$CI_low, x$CI_high, ci = NULL, digits = digits, width = ci_width, brackets = ci_brackets)
    x$CI_high <- NULL
    ci_lvl <- attributes(x)$ci
    centrality_ci <- attributes(x)$first_centrality

    if (is.null(centrality_ci)) {
      ci_suffix <- ""
    } else {
      ci_suffix <- paste0(" (", centrality_ci, ")")
    }

    if (is.null(ci_lvl)) {
      colnames(x)[which(colnames(x) == "CI_low")] <- sprintf("CI%s", ci_suffix)
    } else {
      colnames(x)[which(colnames(x) == "CI_low")] <- sprintf("%i%% CI%s", round(100 * ci_lvl), ci_suffix)
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

  if (".group" %in% colnames(x)) {
    final_table <- list()
    grps <- split(x, x[[".group"]])
    for (i in names(grps)) {
      grps[[i]][[".group"]] <- NULL
      table_caption <- NULL
      if (is.null(format) || format == "text") {
        table_caption <- c(sprintf("# %s", i), "blue")
      } else if (format == "markdown") {
        table_caption <- sprintf("%s", i)
      }
      attr(grps[[i]], "table_caption") <- table_caption
      final_table <- c(final_table, list(grps[[i]]))
    }
  } else {
    final_table <- x
  }

  final_table
}
