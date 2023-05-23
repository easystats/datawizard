#' Create new variables in a data frame
#'
#' Create new variables in a data frame. Unlike `base::transform()`, `data_transform`
#' can be used on grouped data frames, and newly created variables can be directly
#' used.
#'
#' @param data The data frame, for which new variables should be created.
#' @param ... A sequence of named expressions, where the left-hand side refers
#' to the name of the new variable, while the right-hand side represent the
#' values of the new variable. See 'Examples'.
#'
#' @note This function is still experimental and well tested for interactive
#' use. There might be corner cases, e.g. when called from inside functions
#' or similar, where `data_transform()` does not yet work. Please carefully
#' check your results before using this function, say, in package code.
#'
#' @examples
#' data(efc)
#' new_efc <- data_transform(
#'   efc,
#'   c12hour_c = center(c12hour),
#'   c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE),
#'   c12hour_z2 = standardize(c12hour)
#' )
#' head(new_efc)
#'
#' # attributes - in this case, value and variable labels - are preserved
#' str(new_efc)
#'
#' # works on grouped data
#' grouped_efc <- data_group(efc, "c172code")
#' new_efc <- data_transform(
#'   grouped_efc,
#'   c12hour_c = center(c12hour),
#'   c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE),
#'   c12hour_z2 = standardize(c12hour)
#' )
#' head(new_efc)
#' @export
data_transform <- function(data, ...) {
  UseMethod("data_transform")
}

#' @export
data_transform.default <- function(data, ...) {
  insight::format_error("`data` must be a data frame.")
}

#' @export
data_transform.data.frame <- function(data, ...) {
  dots <- match.call(expand.dots = FALSE)$`...`
  for (i in seq_along(dots)) {
    new_variable <- with(data, eval(dots[[i]]))
    if (length(new_variable) != nrow(data)) {
      insight::format_error("New variable has not the same length as the other variables in the data frame.")
    }
    data[[names(dots)[i]]] <- new_variable
  }
  data
}

#' @export
data_transform.grouped_df <- function(data, ...) {
  # we need to evaluate dots here, and pass them with "do.call" to
  # the data.frame method later...
  dots <- match.call(expand.dots = FALSE)$`...`
  # save attributes, convert to regular data frame
  grps <- attr(data, "groups", exact = TRUE)[[".rows"]]
  attr_data <- attributes(data)
  data <- as.data.frame(data)
  # create new variables now - else we cannot replace rows later
  for (i in names(dots)) {
    data[[i]] <- NA
  }
  # perform transform on groups
  for (rows in grps) {
    data[rows, ] <- do.call("data_transform", c(list(data[rows, , drop = FALSE]), dots))
  }
  # set back class, so data frame still works with dplyr
  data <- .replace_attrs(data, attr_data)
  data
}
