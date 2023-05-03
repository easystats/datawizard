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

  # if function called from the "grouped_df" method, we use the dw_transformer
  # attributes that were recovered in the "grouped_df" method

  mc <- match.call(expand.dots = FALSE)

  if ("raw_attributes" %in% names(mc[["..."]])) {
    raw_attributes <- eval(mc[["..."]]$raw_attributes, envir = parent.frame(1L))
  } else {
    raw_attributes <- NULL
  }

  if (!is.null(raw_attributes)) {

    include_bounds <- unname(raw_attributes[which(grepl("include_bounds", names(raw_attributes)))])
    min_value <- unname(raw_attributes[which(grepl("min_value", names(raw_attributes)))])
    range_difference <- unname(raw_attributes[which(grepl("range_difference", names(raw_attributes)))])
    to_range <- unname(raw_attributes[which(grepl("to_range", names(raw_attributes)))])

    if (length(to_range) == 0L) to_range <- NULL

  } else {

    ## TODO implement algorithm include_bounds = FALSE
    include_bounds <- attr(x, "include_bounds")
    min_value <- attr(x, "min_value")
    range_difference <- attr(x, "range_difference")
    to_range <- attr(x, "to_range")

  }

  if (is.null(min_value) || is.null(range_difference)) {
    if (verbose) {
      insight::format_warning("Can't unnormalize variable. Information about range and/or minimum value is missing.")
    }
    return(x)
  }

  if (is.null(to_range)) {
    x * range_difference + min_value
  } else {
    out <- (x - to_range[1]) * (range_difference / diff(to_range)) + min_value
    out
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

  mc <- match.call(expand.dots = FALSE)

  if ("raw_attributes" %in% names(mc[["..."]])) {
    raw_attributes <- eval(mc[["..."]]$raw_attributes, envir = parent.frame(1L))
  } else {
    raw_attributes <- NULL
  }

  for (i in select) {
    attrs <- raw_attributes[
      which(
        grepl(paste0("^attr\\_", i, "\\."),
              names(raw_attributes))
      )
    ]
    x[[i]] <- unnormalize(x[[i]], verbose = verbose, raw_attributes = attrs)
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
  # works only for dplyr >= 0.8.0
  grps <- attr(x, "groups", exact = TRUE)[[".rows"]]

  x <- as.data.frame(x)

  for (i in select) {
    if (is.null(info$groups[[paste0("attr_", i)]])) {
      insight::format_error("Couldn't retrieve the necessary information to unnormalize", i)
    }
  }
  for (rows in seq_along(grps)) {

    # get the dw_transformer attributes for this group
    raw_attrs <- unlist(info$groups[rows, grepl("^attr", names(info$groups))])
    if (length(select) == 1L) {
      names(raw_attrs) <- paste0("attr_", select, ".", names(raw_attrs))
    }

    tmp <- unnormalize(
      x[grps[[rows]], , drop = FALSE],
      select = select,
      exclude = exclude,
      include_bounds = include_bounds,
      verbose = verbose,
      append = FALSE, # need to set to FALSE here, else variable will be doubled
      add_transform_class = FALSE,
      raw_attributes = raw_attrs
    )
    x[grps[[rows]], ] <- tmp
  }
  # set back class, so data frame still works with dplyr
  attributes(x) <- utils::modifyList(info, attributes(x))
  x
}
