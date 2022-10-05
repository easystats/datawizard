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
  not_stacked[["_Rows"]] <- coerce_to_numeric(row.names(data))
  if (insight::is_empty_object(not_stacked)) {
    not_stacked[["_Rows"]] <- coerce_to_numeric(row.names(data))
  }

  if (length(not_selected) == 0) {
    stacked_data <- .stack(data[, cols], rearrange = is.null(rows_to))[, 2:1]
  } else  {
    stacked_data <- .stack(data[, cols, drop = FALSE])[, 2:1]
  }

  stacked_data <- data_rename(stacked_data, "values", values_to)

  # split columns if several names in names_to or names_pattern is specified
  if (length(names_to) > 1) {

    if (is.null(names_pattern)) {
      tmp <- read.csv(
        text = stacked_data$ind,
        sep = "_",
        stringsAsFactors = FALSE,
        header = FALSE
      )
      names(tmp) <- paste0("V", seq_len(ncol(tmp)))
      tmp[tmp == ""] <- NA

      stacked_data$ind <- NULL
      stacked_data <- cbind(tmp, stacked_data)

    } else {
      tmp <- regmatches(
        unique(stacked_data$ind),
        regexec(names_pattern, unique(stacked_data$ind))
      )
      tmp <- as.data.frame(do.call(rbind, tmp), stringsAsFactors = FALSE)
      names(tmp) <- c("ind", names_to)
      stacked_data <- data_merge(stacked_data, tmp, by = "ind")
      stacked_data$ind <- NULL

    }

  }

  stacked_data <- data_relocate(stacked_data, select = values_to, after = -1)

  out <- cbind(
    not_stacked, setNames(stacked_data, c(names_to, values_to)),
    row.names = NULL
  )


  if (!is.null(names_prefix)) {
    if (length(names_to) > 1) {
      insight::format_error(
        "`names_prefix` only works when `names_to` is of length 1."
      )
    }
    out[[names_to]] <- gsub(paste0("^", names_prefix), "", out[[names_to]])
  }

  # rearrange the rows with the temp id
  if (length(not_selected) > 0) {
    out <- data_arrange(out, "_Rows")
  }

  # Remove or rename the row index
  if (is.null(rows_to)) {
    out[["_Rows"]] <- NULL
  } else {
    out <- data_rename(out, "_Rows", rows_to)
  }

  if (values_drop_na) {
    out <- out[!is.na(out[, values_to]), ]
  }

  # add back tidyverse attributes
  if (isTRUE(tbl_input)) {
    class(out) <- c("tbl_df", "tbl", "data.frame")
  }

  if (.has_numeric_rownames(data)) {
    row.names(out) <- NULL
  }

  out
}






#' Code taken from utils::stack
#' Added an argument `rearrange` to reorder the rows to have a repeated sequence
#' when all vars are selected to pivot
#
#' See with following example:
#'
#' wide_data <- data.frame(replicate(5, rnorm(10)))
#' data_to_long(wide_data)
#'
#' @noRd

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
