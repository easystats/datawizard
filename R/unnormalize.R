#' @rdname normalize
#' @export
unnormalize <- function(x, ...) {
  UseMethod("unnormalize")
}


#' @export
unnormalize.default <- function(x, ...) {
  insight::format_error(
    "Variables of class '", class(x)[1], "' can't be unnormalized."
  )
}


#' @rdname normalize
#' @export
unnormalize.numeric <- function(x, verbose = TRUE, ...) {
  ## TODO implement algorithm include_bounds = FALSE

  # if function called from the "grouped_df" method, we use the dw_transformer
  # attributes that were recovered in the "grouped_df" method

  dots <- match.call(expand.dots = FALSE)[["..."]]
  grp_attr_dw <- eval(dots$grp_attr_dw, envir = parent.frame(1L))

  if (is.null(grp_attr_dw)) {
    include_bounds <- attr(x, "include_bounds")
    min_value <- attr(x, "min_value")
    range_difference <- attr(x, "range_difference")
    to_range <- attr(x, "to_range")
  } else {
    names(grp_attr_dw) <- gsub(".*\\.", "", names(grp_attr_dw))
    include_bounds <- grp_attr_dw["include_bounds"]
    min_value <- grp_attr_dw["min_value"]
    range_difference <- grp_attr_dw["range_difference"]
    to_range <- grp_attr_dw["to_range"]
    if (is.na(to_range)) {
      to_range <- NULL
    }
  }

  if (is.null(min_value) || is.null(range_difference)) {
    if (verbose) {
      insight::format_error("Can't unnormalize variable. Information about range and/or minimum value is missing.")
    }
    return(x)
  }

  if (is.null(to_range)) {
    x * range_difference + min_value
  } else {
    (x - to_range[1]) * (range_difference / diff(to_range)) + min_value
  }
}


#' @rdname normalize
#' @export
unnormalize.data.frame <- function(x,
                                   select = NULL,
                                   exclude = NULL,
                                   ignore_case = FALSE,
                                   regex = FALSE,
                                   verbose = TRUE,
                                   ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    verbose = verbose
  )

  # if function called from the "grouped_df" method, we use the dw_transformer
  # attributes that were recovered in the "grouped_df" method

  dots <- match.call(expand.dots = FALSE)[["..."]]

  if (is.null(dots$grp_attr_dw)) {
    grp_attr_dw <- NULL
  } else {
    grp_attr_dw <- eval(dots$grp_attr_dw, envir = parent.frame(1L))
  }

  for (i in select) {
    var_attr <- grep(paste0("^attr\\_", i, "\\."), names(grp_attr_dw))
    attrs <- grp_attr_dw[var_attr]
    x[[i]] <- unnormalize(x[[i]], verbose = verbose, grp_attr_dw = attrs)
  }

  x
}

#' @rdname normalize
#' @export
unnormalize.grouped_df <- function(x,
                                   select = NULL,
                                   exclude = NULL,
                                   ignore_case = FALSE,
                                   regex = FALSE,
                                   verbose = TRUE,
                                   ...) {
  # evaluate select/exclude, may be select-helpers
  select <- .select_nse(select,
    x,
    exclude,
    ignore_case,
    regex = regex,
    remove_group_var = TRUE,
    verbose = verbose
  )

  info <- attributes(x)

  grps <- attr(x, "groups", exact = TRUE)[[".rows"]]

  x <- as.data.frame(x)

  for (i in select) {
    if (is.null(info$groups[[paste0("attr_", i)]])) {
      insight::format_error(
        paste(
          "Couldn't retrieve the necessary information to unnormalize",
          text_concatenate(i, enclose = "`")
        )
      )
    }
  }
  for (rows in seq_along(grps)) {
    # get the dw_transformer attributes for this group
    raw_attrs <- unlist(info$groups[rows, startsWith(names(info$groups), "attr")])
    if (length(select) == 1L) {
      names(raw_attrs) <- paste0("attr_", select, ".", names(raw_attrs))
    }

    tmp <- unnormalize(
      x[grps[[rows]], , drop = FALSE],
      select = select,
      exclude = exclude,
      ignore_case = ignore_case,
      regex = regex,
      verbose = verbose,
      grp_attr_dw = raw_attrs
    )
    x[grps[[rows]], ] <- tmp
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- utils::modifyList(info, attributes(x))
  class(x) <- c("grouped_df", class(x))
  x
}
