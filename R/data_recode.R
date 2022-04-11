#' @title Recode old values of variables into new values
#' @name data_recode
#'
#' @description
#' This functions recodes old values into new values and can be used to to
#' recode numeric or character vectors, or factors.
#'
#' @param x A data frame, numeric or character vector, or factor.
#' @param recodes A list of named vectors, which indicate the recode pairs.
#'   The _names_ of the list-elements (i.e. the left-hand side) represent the
#'   old values, while the values of the list-elements indicate the values.
#'   When recoding numeric vectors, element names have to be surrounded in
#'   backticks. For example, `recodes = list(\`1\` = 0)` would recode all
#'   `1` into `0` in a numeric vector.
#' @param ... not used.
#' @inheritParams find_columns
#'
#' @return `x`, recoded into groups. By default `x` is numeric, unless `labels`
#'   is specified. In this case, a factor is returned, where the factor levels
#'   (i.e. recoded groups are labelled accordingly.
#'
#' @examples
#' set.seed(123)
#' x <- sample(c(1:4, NA), 20, TRUE)
#' table(x, useNA = "always")
#'
#' out <- data_recode(x, list(`1` = 0, `2:3` = 1, `4` = 2))
#' out
#' table(out, useNA = "always")
#'
#' out <- data_recode(x, list(`1` = 0, `2:3` = 1, `4` = 2, `NA` = 9))
#' out
#' table(out, useNA = "always")
#'
#' out <- data_recode(x, list(`1` = 0, `2:3` = 1, `else` = 77))
#' out
#' table(out, useNA = "always")
#' @export
data_recode <- function(x, ...) {
  UseMethod("data_recode")
}


#' @export
data_recode.default <- function(x, ...) {
  return(x)
}


#' @rdname data_recode
#' @export
data_recode.numeric <- function(x, recodes = NULL, verbose = TRUE, ...) {

  # save
  original_x <- x

  # no missings
  valid <- stats::na.omit(x)

  # stop if all NA
  if (!length(valid)) {
    if (isTRUE(verbose)) {
      warning(insight::format_message("Variable contains only missing values. No recoding carried out."), call. = FALSE)
    }
    return(original_x)
  }

  # check for "else" token
  if ("else" %in% names(recodes)) {
    else_token <- recodes[["else"]]
    recodes["else"] <- NULL

    # set the default value for all values that have no match
    # (i.e. that should not be recoded)
    if (else_token == "copy") {
      x <- original_x
    } else {
      x <- rep(as.numeric(else_token), length = length(x))
    }
  }

  for (i in names(recodes)) {
    # name of list element is old value
    value_string <- i

    # replace placeholder
    value_string <- gsub("min", min(x, na.rm = TRUE), value_string)
    value_string <- gsub("max", max(x, na.rm = TRUE), value_string)

    # mimic vector
    if (!grepl("c(", value_string, fixed = TRUE)) {
      value_string <- paste0("c(", value_string, ")")
    }

    # parse old values, which are strings (names of element), but which should
    # contain values, like "1:10" or "1, 2, 3, 4". These should now be in the
    # format "c(1, 2, 3, 4)" or "c(1:10)", and it should be possible to parse
    # and evaluate these strings into a numeric vector
    old_values <- tryCatch(eval(parse(text = value_string)), error = function(e) NULL)

    if (!is.null(old_values)) {
      x[which(original_x %in% old_values)] <- as.numeric(recodes[[i]])
    }
  }

  x
}


#' @export
data_recode.factor <- function(x, recodes = NULL, verbose = TRUE, ...) {
  original_x <- x
  levels(x) <- 1:nlevels(x)
  out <- as.factor(data_recode(as.numeric(x), ...))
  .set_back_labels(out, original_x, include_values = FALSE)
}


#' @rdname data_recode
#' @export
data_recode.data.frame <- function(x,
                                   recodes = NULL,
                                   force = FALSE,
                                   append = FALSE,
                                   select = NULL,
                                   exclude = NULL,
                                   ignore_case = FALSE,
                                   verbose = TRUE,
                                   ...) {
  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)

  # process arguments
  args <- .process_std_args(x, select, exclude, weights = NULL, append, append_suffix = "_r", force)

  # update processed arguments
  x <- args$x
  select <- args$select

  x[select] <- lapply(x[select], data_recode, recodes = recodes, verbose = verbose, ...)
  x
}

