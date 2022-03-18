#' @export
data_addsuffix_test <- function(data, pattern, select = NULL, exclude = NULL, ignore_case = FALSE, ...) {

  select <- test_fun(select, data, exclude, ignore_case)

  selected_columns <- colnames(data) %in% select
  colnames(data)[selected_columns] <- paste0(colnames(data)[selected_columns], pattern)
  data
}


test_fun <- function(select, data, exclude, ignore_case) {
  fixed <- TRUE
  # avoid conflicts
  conflicting_packages <- .conflicting_packages("poorman")

  # in case pattern is a variable from another function call...
  p <- try(eval(select), silent = TRUE)
  if (inherits(p, c("try-error", "simpleError"))) {
    p <- substitute(select, env = parent.frame())
  }

  # check if pattern is a function like "starts_with()"
  select <- tryCatch(eval(p), error = function(e) NULL)

  # if select could not be evaluated (because expression "makes no sense")
  # try to evaluate and find select-helpers. In this case, set fixed = FALSE,
  # so we can use grepl()
  if (is.null(select)) {
    evaluated_pattern <- .evaluate_pattern(insight::safe_deparse(p), data, ignore_case = ignore_case)
    select <- evaluated_pattern$pattern
    fixed <- evaluated_pattern$fixed
  }

  # seems to be no valid column name or index, so try to grep
  if (isFALSE(fixed)) {
    select <- colnames(data)[grepl(select, colnames(data), ignore.case = ignore_case)]
  }

  # load again
  .attach_packages(conflicting_packages)

  # return valid column names, based on pattern
  .evaluated_pattern_to_colnames(select, data, ignore_case, verbose = FALSE, exclude)
}
