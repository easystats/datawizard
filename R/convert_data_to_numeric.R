#' Convert data to numeric
#'
#' Convert data to numeric by converting characters to factors and factors to
#' either numeric levels or dummy variables.
#'
#' @param x A data frame or a vector.
#' @param dummy_factors Transform factors to dummy factors (all factor levels as
#'   different columns filled with a binary 0-1 value).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' convert_data_to_numeric(head(ToothGrowth))
#' convert_data_to_numeric(head(ToothGrowth), dummy_factors = FALSE)
#'
#' @return A data frame of numeric variables.
#'
#' @export
convert_data_to_numeric <- function(x, ...) {
  UseMethod("convert_data_to_numeric")
}

#' @rdname convert_data_to_numeric
#' @export
data_to_numeric <- convert_data_to_numeric


#' @export
convert_data_to_numeric.default <- function(x, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    message(insight::format_message(sprintf("Converting into numeric values currently not possible for variables of class '%s'.", class(x)[1])))
  }
  x
}


#' @rdname convert_data_to_numeric
#' @export
convert_data_to_numeric.data.frame <- function(x, dummy_factors = TRUE, ...) {
  out <- sapply(x, convert_data_to_numeric, dummy_factors = dummy_factors, simplify = FALSE)
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
  out
}


#' @export
convert_data_to_numeric.numeric <- function(x, ...) {
  .set_back_labels(as.numeric(x), x)
}

#' @export
convert_data_to_numeric.double <- convert_data_to_numeric.numeric

#' @export
convert_data_to_numeric.logical <- convert_data_to_numeric.numeric


#' @export
convert_data_to_numeric.factor <- function(x, dummy_factors = TRUE, ...) {
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
      for (i in 1:length(na_values)) {
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
  } else {
    out <- .set_back_labels(as.numeric(x), x)
  }
  out
}


#' @export
convert_data_to_numeric.character <- function(x, dummy_factors = FALSE, ...) {
  numbers <- sapply(x, function(i) {
    element <- tryCatch(.str2lang(i), error = function(e) NULL)
    !is.null(element) && is.numeric(element)
  })
  if (all(numbers)) {
    out <- as.numeric(sapply(x, .str2lang))
  } else {
    out <- convert_data_to_numeric(as.factor(x), dummy_factors = dummy_factors)
  }
  out
}
