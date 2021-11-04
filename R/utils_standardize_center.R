
# helper -----------------------------


.get_center_scale <- function(x, robust = FALSE, weights = NULL, reference = NULL) {
  if (is.null(reference)) reference <- x

  if (robust) {
    center <- .median(reference, weights)
    scale <- .mad(reference, weights)
  } else {
    center <- .mean(reference, weights)
    scale <- .sd(reference, weights)
  }
  list(center = center, scale = scale)
}



#' @keywords internal
.check_standardize_numeric <- function(x,
                                       name = NULL,
                                       verbose = TRUE,
                                       reference = NULL) {
  # Warning if only one value
  if (length(unique(x)) == 1 && is.null(reference)) {
    if (verbose) {
      if (is.null(name)) {
        message("The variable contains only one unique value and will be set to 0.")
      } else {
        message(paste0("The variable `", name, "` contains only one unique value and will be set to 0."))
      }
    }
    return(NULL)
  }

  # Warning if logical vector
  if (length(unique(x)) == 2 && !is.factor(x) && !is.character(x)) {
    if (verbose) {
      if (is.null(name)) {
        message("The variable contains only two different values. Consider converting it to a factor.")
      } else {
        message(paste0("Variable `", name, "` contains only two different values. Consider converting it to a factor."))
      }
    }
  }
  x
}





.select_variables <- function(x, select, exclude, force) {
  if (is.null(select)) {
    select <- names(x)
  }

  if (!is.null(exclude)) {
    select <- setdiff(select, exclude)
  }

  if (!force) {
    factors <- sapply(x[select], function(i) is.factor(i) | is.character(i))
    select <- select[!factors]
  }

  select
}




#' @keywords internal
.check_center_numeric <- function(x, name = NULL, verbose = TRUE) {
  # Warning if only one value
  if (length(unique(x)) == 1) {
    if (verbose) {
      if (is.null(name)) {
        message("The variable contains only one unique value and will not be standardized.")
      } else {
        message(paste0("The variable `", name, "` contains only one unique value and will not be standardized."))
      }
    }
    return(NULL)
  }

  # Warning if logical vector
  if (length(unique(x)) == 2 && !is.factor(x) && !is.character(x)) {
    if (verbose) {
      if (is.null(name)) {
        message(insight::format_message("The variable contains only two different values. Consider converting it to a factor."))
      } else {
        message(insight::format_message(paste0("Variable `", name, "` contains only two different values. Consider converting it to a factor.")))
      }
    }
  }
  x
}



.are_weights <- function(w) {
  !is.null(w) && length(w) && !all(w == 1) && !all(w == w[1])
}



.factor_to_numeric <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }

  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    levels(x) <- 1:nlevels(x)
  }

  as.numeric(as.character(x))
}



.mean <- function(x, weights = NULL, verbose = TRUE, ...) {
  if (!.are_weights(weights)) {
    return(mean(x, na.rm = TRUE))
  }

  if (!all(weights > 0, na.rm = TRUE)) {
    if (isTRUE(verbose)) {
      warning("Some weights were negative. Weighting not carried out.", call. = FALSE)
    }
    return(mean(x, na.rm = TRUE))
  }

  stats::weighted.mean(x, weights, na.rm = TRUE)
}



.median <- function(x, weights = NULL, verbose = TRUE, ...) {
  # From spatstat + wiki
  if (!.are_weights(weights)) {
    return(stats::median(x, na.rm = TRUE))
  }

  if (!all(weights > 0, na.rm = TRUE)) {
    if (isTRUE(verbose)) {
      warning("Some weights were negative. Weighting not carried out.", call. = FALSE)
    }
    return(stats::median(x, na.rm = TRUE))
  }

  oo <- order(x)
  x <- x[oo]
  weights <- weights[oo]
  Fx <- cumsum(weights) / sum(weights)

  lefties <- which(Fx <= 0.5)
  left <- max(lefties)
  if (length(lefties) == 0) {
    result <- x[1]
  } else if (left == length(x)) {
    result <- x[length(x)]
  } else {
    result <- x[left]

    if (!(Fx[left - 1] < 0.5 && 1 - Fx[left] < 0.5)) {
      right <- left + 1
      y <- x[left] * Fx[left] + x[right] * Fx[right]
      if (is.finite(y)) result <- y
    }
  }

  result
}



.process_std_args <- function(x, select, exclude, weights, append, append_suffix = "_z", force, remove_na = "none") {
  # check for formula notation, convert to character vector
  if (inherits(select, "formula")) {
    select <- all.vars(select)
  }
  if (inherits(exclude, "formula")) {
    exclude <- all.vars(exclude)
  }

  # check append argument, and set default
  if (isFALSE(append)) {
    append <- NULL
  } else if (isTRUE(append)) {
    append <- append_suffix
  }

  if (!is.null(weights) && is.character(weights)) {
    if (weights %in% colnames(x)) {
      exclude <- c(exclude, weights)
    } else {
      warning("Could not find weighting column '", weights, "'. Weighting not carried out.")
      weights <- NULL
    }
  }

  select <- .select_variables(x, select, exclude, force)

  # drop NAs
  remove_na <- match.arg(remove_na, c("none", "selected", "all"))

  omit <- switch(remove_na,
                 none = logical(nrow(x)),
                 selected = rowSums(sapply(x[select], is.na)) > 0,
                 all = rowSums(sapply(x, is.na)) > 0
  )
  x <- x[!omit, , drop = FALSE]

  if (!is.null(weights) && is.character(weights)) weights <- x[[weights]]

  # append standardized variables
  if (!is.null(append) && append != "") {
    new_variables <- x[select]
    colnames(new_variables) <- paste0(colnames(new_variables), append)
    x <- cbind(x, new_variables)
    select <- colnames(new_variables)
  }

  list(
    x = x,
    select = select,
    exclude = exclude,
    weights = weights,
    append = append
  )
}
