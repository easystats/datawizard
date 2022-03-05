.evaluate_pattern <- function(x) {
  if (grepl("^starts_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("^", gsub("starts_with\\(\"(.*)\"\\)", "\\1", x))
  } else if (grepl("^ends_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0(gsub("ends_with\\(\"(.*)\"\\)", "\\1", x), "$")
  } else if (grepl("^contains\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("\\Q", gsub("contains\\(\"(.*)\"\\)", "\\1", x), "\\E")
  } else if (grepl("^matches\\(\"(.*)\"\\)", x)) {
    pattern <- gsub("matches\\(\"(.*)\"\\)", "\\1", x)
  } else if (grepl("^col_starts_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("^", gsub("col_starts_with\\(\"(.*)\"\\)", "\\1", x))
  } else if (grepl("^col_ends_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0(gsub("^ends_with\\(\"(.*)\"\\)", "\\1", x), "$")
  } else if (grepl("^col_contains\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("\\Q", gsub("col_contains\\(\"(.*)\"\\)", "\\1", x), "\\E")
  } else if (grepl("^col_matches\\(\"(.*)\"\\)", x)) {
    pattern <- gsub("col_matches\\(\"(.*)\"\\)", "\\1", x)
  } else {
    pattern <- x
  }
  gsub("\\\\", "\\", pattern, fixed = TRUE)
}
