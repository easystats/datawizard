#' Convert data to numeric
#'
#' Convert data to numeric by converting characters to factors and factors to
#' either numeric levels or dummy variables. The "counterpart" to convert
#' variables into factors is `to_factor()`.
#'
#' @param x A data frame, factor or vector.
#' @param dummy_factors Transform factors to dummy factors (all factor levels as
#'   different columns filled with a binary 0-1 value).
#' @param preserve_levels Logical, only applies if `x` is a factor. If `TRUE`,
#' and `x` has numeric factor levels, these will be converted into the related
#' numeric values. If this is not possible, the converted numeric values will
#' start from 1 to number of levels.
#' @param lowest Numeric, indicating the lowest (minimum) value when converting
#' factors or character vectors to numeric values.
#' @param ... Arguments passed to or from other methods.
#' @inheritParams find_columns
#' @inheritParams categorize
#'
#' @section Selection of variables - `select` argument:
#' For most functions that have a `select` argument the complete input data
#' frame is returned, even when `select` only selects a range of variables.
#' However, for `to_numeric()`, factors might be converted into dummies,
#' thus, the number of variables of the returned data frame no longer match
#' the input data frame. Hence, when `select` is used, *only* those variables
#' (or their dummies) specified in `select` will be returned. Use `append=TRUE`
#' to also include the original variables in the returned data frame.
#'
#' @examples
#' to_numeric(head(ToothGrowth))
#' to_numeric(head(ToothGrowth), dummy_factors = FALSE)
#'
#' # factors
#' x <- as.factor(mtcars$gear)
#' to_numeric(x, dummy_factors = FALSE)
#' to_numeric(x, dummy_factors = FALSE, preserve_levels = TRUE)
#'
#' @return A data frame of numeric variables.
#'
#' @export
to_numeric <- function(x, ...) {
  UseMethod("to_numeric")
}


## TODO Deprecate and remove alias later

#' @rdname to_numeric
#' @export
data_to_numeric <- to_numeric


#' @export
to_numeric.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    message(insight::format_message(sprintf("Converting into numeric values currently not possible for variables of class '%s'.", class(x)[1])))
  }
  x
}


#' @rdname to_numeric
#' @export
to_numeric.data.frame <- function(x,
                                  select = NULL,
                                  exclude = NULL,
                                  dummy_factors = TRUE,
                                  preserve_levels = FALSE,
                                  lowest = NULL,
                                  append = FALSE,
                                  ignore_case = FALSE,
                                  verbose = TRUE,
                                  ...) {
  # sanity check, return as is for complete numeric
  if (all(sapply(x, is.numeric))) {
    return(x)
  }

  # evaluate arguments
  select <- .select_nse(select, x, exclude, ignore_case)

  # drop numerics, when append is not FALSE
  if (!isFALSE(append)) {
    select <- colnames(x[select])[!sapply(x[select], is.numeric)]
  }

  # process arguments
  args <- .process_std_args(
    x,
    select,
    exclude,
    weights = NULL,
    append,
    append_suffix = "_n",
    force = TRUE
  )

  # update processed arguments
  x <- args$x
  select <- args$select

  out <- sapply(
    x[select],
    to_numeric,
    dummy_factors = dummy_factors,
    preserve_levels = preserve_levels,
    lowest = lowest,
    verbose = verbose,
    simplify = FALSE
  )

  # save variable attributes
  attr_vars <- lapply(out, attributes)
  # "out" is currently a list, bind columns and to data frame
  out <- as.data.frame(do.call(cbind, out))
  # set back attributes
  for (i in colnames(out)) {
    if (is.list(attr_vars[[i]])) {
      if (is.list(attributes(out[[i]]))) {
        attributes(out[[i]]) <- utils::modifyList(attr_vars[[i]], attributes(out[[i]]))
      } else {
        attributes(out[[i]]) <- attr_vars[[i]]
      }
    }
  }

  # due to the special handling of dummy factors, we need to take care
  # of appending the data here again. usually, "args$x" includes the appended
  # data, which does not work here...

  if (!isFALSE(append)) {
    common_columns <- intersect(colnames(x), colnames(out))
    if (length(common_columns)) {
      x[common_columns] <- NULL
    }
    out <- cbind(x, out)
  }

  out
}


#' @export
to_numeric.numeric <- function(x, verbose = TRUE, ...) {
  .set_back_labels(as.numeric(x), x)
}

#' @export
to_numeric.double <- to_numeric.numeric

#' @export
to_numeric.logical <- to_numeric.numeric

#' @export
to_numeric.Date <- function(x, verbose = TRUE, ...) {
  if (verbose) {
    warning(insight::format_message(
      "Converting a date-time variable into numeric.",
      "Please note that this conversion probably not returns meaningful results."
    ), call. = FALSE)
  }
  as.numeric(x)
}

#' @export
to_numeric.POSIXt <- to_numeric.Date


#' @export
to_numeric.factor <- function(x,
                              dummy_factors = TRUE,
                              preserve_levels = FALSE,
                              lowest = NULL,
                              verbose = TRUE,
                              ...) {
  # preserving levels only works when factor levels are numeric
  if (isTRUE(preserve_levels) && anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    preserve_levels <- FALSE
  }

  if (dummy_factors) {
    out <- as.data.frame(stats::model.matrix(~x, contrasts.arg = list(x = "contr.treatment")))
    out[1] <- as.numeric(rowSums(out[2:ncol(out)]) == 0)

    # insert back NA rows. if "x" had missing values, model.matrix() creates an
    # array with only non-missing values, so some rows are missing. First, we
    # need to now which rows are missing (na_values) and the length of the
    # original vector (which will be the number of rows in the final data frame)

    na_values <- which(is.na(x))
    rows_x <- length(x)

    if (any(na_values)) {
      # iterate all missing values that have
      for (i in seq_along(na_values)) {
        # if the first observation was missing, add NA row and bind data frame
        if (i == 1 && na_values[i] == 1) {
          out <- rbind(NA, out)
        } else {
          # if the last observation was NA, add NA row to data frame
          if (na_values[i] == rows_x) {
            out <- rbind(out, NA)
          } else {
            # else, pick rows from beginning to current NA value, add NA,
            # and rbind the remaining rows
            out <- rbind(out[1:(na_values[i] - 1), ], NA, out[na_values[i]:nrow(out), ])
          }
        }
      }
      rownames(out) <- NULL
    }
    names(out) <- levels(x)
  } else if (preserve_levels) {
    out <- .set_back_labels(as.numeric(as.character(x)), x)
  } else {
    out <- .set_back_labels(as.numeric(x), x)
  }

  # shift to requested starting value
  if (!is.null(lowest)) {
    difference <- min(out) - lowest
    out <- out - difference
  }

  out
}


#' @export
to_numeric.character <- function(x,
                                 dummy_factors = FALSE,
                                 lowest = NULL,
                                 verbose = TRUE,
                                 ...) {
  numbers <- sapply(x, function(i) {
    element <- tryCatch(.str2lang(i), error = function(e) NULL)
    !is.null(element) && is.numeric(element)
  })
  if (all(numbers)) {
    out <- as.numeric(sapply(x, .str2lang))
  } else {
    out <- to_numeric(as.factor(x), dummy_factors = dummy_factors)
  }

  # shift to requested starting value
  if (!is.null(lowest)) {
    difference <- min(out) - lowest
    out <- out - difference
  }

  out
}



#' Convert to Numeric (if possible)
#'
#' Tries to convert vector to numeric if possible (if no warnings or errors).
#' Otherwise, leaves it as is.
#'
#' @param x A vector to be converted.
#'
#' @examples
#' coerce_to_numeric(c("1", "2"))
#' coerce_to_numeric(c("1", "2", "A"))
#' @return Numeric vector (if possible)
#' @export
coerce_to_numeric <- function(x) {
  tryCatch(as.numeric(as.character(x)),
    error = function(e) x,
    warning = function(w) x
  )
}
