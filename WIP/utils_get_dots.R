.is_num_chr <- function(x) {
  is.character(x) && !anyNA(suppressWarnings(as.numeric(stats::na.omit(x))))
}

.is_num_fac <- function(x) {
  is.factor(x) && !anyNA(suppressWarnings(as.numeric(levels(x))))
}



.get_num_range <- function(i) {
  r1 <- trimws(unlist(strsplit(gsub("num_range\\((.*)\\)", "\\1", i), ",")))
  r2 <- gsub("\"", "", trimws(gsub("(.*)(=)(.*)", "\\3", r1)), fixed = TRUE)
  es <- grepl("=", r1)
  if (any(es)) {
    names(r2)[es] <- trimws(gsub("(.*)(=)(.*)", "\\1", r1[es]))
  }

  args <- c("prefix", "range", "width")
  if (is.null(names(r2))) {
    names(r2) <- args[1:length(r2)]
  }

  na_names <- which(is.na(names(r2)))
  if (length(na_names)) {
    names(r2)[na_names] <- args[na_names]
  }

  if (length(r2) > 3) {
    r2 <- r2[1:3]
  }

  from <- as.numeric(gsub("(\\d):(.*)", "\\1", r2["range"]))
  to <- as.numeric(gsub("(.*):(\\d)", "\\2", r2["range"]))
  width <- as.numeric(r2["width"])

  if (is.na(width)) {
    sprintf("%s%i", r2["prefix"], from:to)
  } else {
    sprintf("%s%.*i", r2["prefix"], width, from:to)
  }
}
