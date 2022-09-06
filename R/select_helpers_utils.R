.starts_with <- function(match, data, ignore_case = TRUE) {
  .check_select_helper(match, data)
  match <- paste0("^", match)
  cols <- names(data)
  out <- cols[which(grepl(match, cols, ignore.case = ignore_case))]
  out
}

.ends_with <- function(match, data, ignore_case = TRUE) {
  .check_select_helper(match, data)
  match <- paste0(match, "$")
  cols <- names(data)
  out <- cols[which(grepl(match, cols, ignore.case = ignore_case))]
  out
}

.contains <- function(match, data, ignore_case = TRUE) {
  .check_select_helper(match, data)
  cols <- names(data)
  out <- cols[which(grepl(match, cols, ignore.case = ignore_case))]
  out
}


.check_select_helper <- function(match, data) {
  if (is.null(match)) {
    stop(insight::format_message("Argument `match` is missing."), call. = FALSE)
  }
  if (!is.character(match)) {
    stop(insight::format_message("Argument `match` must be of type character."), call. = FALSE)
  }
}

.check_data <- function(data) {
  if (is.null(data)) {
    stop(insight::format_message("The `data` argument must be provided."), call. = FALSE)
  }
  # check data frame input
  if (!is.null(data) && !is.data.frame(data)) {
    data <- try(as.data.frame(data), silent = TRUE)
    if (inherits(data, c("try-error", "simpleError"))) {
      stop(insight::format_message(
        "The `data` argument must be a data frame, or an object that can be coerced to a data frame."
      ), call. = FALSE)
    }
  }
}
