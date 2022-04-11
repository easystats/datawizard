#' @title Recode old values of variables into new values
#' @name data_recode
#'
#' @description
#' This functions divides the range of variables into intervals and recodes
#' the values inside these intervals according to their related interval.
#' It is basically a wrapper around base R's `cut()`, providing a simplified
#' and more accessible way to define the interval breaks (cut-off values).
#'
#' @param x A (grouped) data frame, numeric vector or factor.
#' @param recodes A named vector, or a list of named vectors, which indicate
#'   the recode pairs.
#' @param ... not used.
#' @inheritParams find_columns
#'
#' @return `x`, recoded into groups. By default `x` is numeric, unless `labels`
#'   is specified. In this case, a factor is returned, where the factor levels
#'   (i.e. recoded groups are labelled accordingly.
#'
#' @examples
#' set.seed(123)
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
data_recode.factor <- function(x, ...) {
  original_x <- x
  levels(x) <- 1:nlevels(x)
  out <- as.factor(data_recode(as.numeric(x), ...))
  .set_back_labels(out, original_x, include_values = FALSE)
}


#' @rdname data_recode
#' @export
data_recode.data.frame <- function(x,
                                split = "median",
                                n_groups = NULL,
                                range = NULL,
                                lowest = 1,
                                labels = NULL,
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

  x[select] <- lapply(x[select], data_recode, split = split, n_groups = n_groups, range = range, lowest = lowest, labels = labels, verbose = verbose, ...)
  x
}

