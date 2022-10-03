new_data_to_long <- function(
    data,
    select = "all",
    names_to = "name",
    names_prefix = NULL,
    names_sep = NULL,
    names_pattern = NULL,
    values_to = "value",
    values_drop_na = FALSE,
    rows_to = NULL,
    ignore_case = FALSE,
    regex = FALSE,
    ...,
    cols,
    colnames_to
  ){

  # Check args
  if (!missing(colnames_to)) {
    .is_deprecated("colnames_to", "names_to")
    if (is.null(names_to)) {
      names_to <- colnames_to
    }
  }

  # Prefer "cols" over "select" for compat with tidyr::pivot_longer
  if (!missing(cols)) {
    select <- substitute(cols)
    cols <- .select_nse(
      select,
      data,
      exclude = NULL,
      ignore_case = ignore_case,
      regex = regex,
      verbose = FALSE
    )
  } else {
    if (!missing(select) || !is.null(select)) {
      cols <- .select_nse(
        select,
        data,
        exclude = NULL,
        ignore_case = ignore_case,
        regex = regex,
        verbose = FALSE
      )
    } else {
      insight::format_error(
        "You need to specify columns to pivot, either with `select` or `cols`."
      )
    }
  }

  # Remove tidyverse attributes, will add them back at the end
  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  } else {
    tbl_input <- FALSE
  }


  if (any(names_to %in% setdiff(names(data), cols))) {
    insight::format_error(
      "Some values of the columns specified in 'names_to' are already present as column names.",
      paste0(
        "Either use another value in `names_to` or rename the following columns: ",
        text_concatenate(names_to[which(names_to %in% setdiff(names(data), cols))])
      )
    )
  }

  # nothing to select?
  if (!length(cols)) {
    stop("No columns found for reshaping data.", call. = FALSE)
  }


  not_selected <- setdiff(names(data), cols)

  # create a temp id so that we know how to rearrange the rows once the data is
  # stacked
  not_stacked <- data[, not_selected, drop = FALSE]
  not_stacked[["_Rows"]] <- 1:nrow(not_stacked)

  if (length(not_selected) == 0) {
    stacked_data <- .stack(data[, cols], rearrange = TRUE)[, 2:1]
    out <- setNames(stacked_data, c(names_to, values_to))
  } else  {
    stacked_data <- .stack(data[, cols])[, 2:1]
    out <- cbind(not_stacked, setNames(stacked_data, c(names_to, values_to)))
  }

  if (!is.null(names_prefix)) {
    out[[names_to]] <- gsub(paste0("^", names_prefix), "", out[[names_to]])
  }

  rownames(out) <- 1:nrow(out)

  # rearrange the rows with the temp id and remove it
  out <- data_arrange(out, "_Rows")
  out <- data_remove(out, "_Rows")

  if (values_drop_na) {
    out <- out[!is.na(out[, values_to]), ]
  }

  # add back tidyverse attributes
  if (isTRUE(tbl_input)) {
    class(out) <- c("tbl_df", "tbl", "data.frame")
  }

  row.names(out) <- NULL

  out
}






# code taken from utils::stack
# Added an argument `rearrange` to reorder the rows to have a repeated sequence
# when all vars are selected to pivot
#
# See with first example in the docs of data_to_long

.stack <- function(x, select, drop = FALSE, rearrange = FALSE, ...) {
  if (!missing(select)) {
    nl <- as.list(1L:ncol(x))
    names(nl) <- names(x)
    vars <- eval(substitute(select), nl, parent.frame())
    x <- x[, vars, drop = FALSE]
  }
  keep <- vapply(x, is.vector, NA)
  if (!any(keep))
    stop("no vector columns were selected")
  if (!all(keep))
    warning("non-vector columns will be ignored")
  x <- x[, keep, drop = FALSE]
  ind <- rep(names(x), times = lengths(x))
  if (drop) {
    ind <- droplevels(ind)
  }
  out <- data.frame(values = unlist(unname(x)), ind, stringsAsFactors = FALSE)

  if (isTRUE(rearrange)) {

    split_data <- split(out, ~ ind)

    list_data <- lapply(seq_along(split_data), function(x) {
      new_id <- x + (1:nrow(split_data[[x]]) - 1) * length(split_data)
      split_data[[x]]$id2 <- new_id
      return(split_data[[x]])
    })

    out <- do.call(rbind, list_data)
    out <- data_arrange(out, "id2")
    out <- data_remove(out, "id2")
  }

  out
}
