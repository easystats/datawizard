.evaluate_pattern <- function(x) {
  if (grepl("starts_with\\(\"(.*)\"\\)", x) || grepl("col_starts_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("^", gsub("starts_with\\(\"(.*)\"\\)", "\\1", x))
  } else if (grepl("ends_with\\(\"(.*)\"\\)", x) || grepl("col_ends_with\\(\"(.*)\"\\)", x)) {
    pattern <- paste0(gsub("ends_with\\(\"(.*)\"\\)", "\\1", x), "$")
  } else if (grepl("contains\\(\"(.*)\"\\)", x) || grepl("col_contains\\(\"(.*)\"\\)", x)) {
    pattern <- paste0("\\Q", gsub("contains\\(\"(.*)\"\\)", "\\1", x), "\\E")
  } else {
    pattern <- x
  }
  gsub("\\\\", "\\", pattern, fixed = TRUE)
}
