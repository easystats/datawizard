.evaluate_pattern <- function(x, data = NULL, ignore_case = FALSE) {
  fixed <- FALSE
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
  } else if (!is.null(data) && grepl(":", x, fixed = TRUE)) {
    from_to <- unlist(strsplit(x, ":", fixed = TRUE))
    cn <- colnames(data)
    if (isTRUE(ignore_case)) {
      from_to <- tolower(from_to)
      cn <- tolower(cn)
    }
    from <- which(cn == from_to[1])
    to <- which(cn == from_to[2])
    if (!length(from)) {
      stop("Could not find variable '", from_to[1], "' in data.", call. = FALSE)
    }
    if (!length(to)) {
      stop("Could not find variable '", from_to[2], "' in data.", call. = FALSE)
    }
    pattern <- colnames(data)[from:to]
    fixed <- TRUE
  } else {
    pattern <- x
  }
  list(pattern = gsub("\\\\", "\\", pattern, fixed = TRUE), fixed = fixed)
}
