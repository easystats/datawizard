new_data_to_wide <- function(data,
                         id_cols = NULL,
                         values_from = "Value",
                         names_from = "Name",
                         names_sep = "_",
                         names_prefix = "",
                         names_glue = NULL,
                         values_fill = NULL,
                         verbose = TRUE,
                         ...,
                         colnames_from,
                         rows_from,
                         sep) {

  if (!missing(colnames_from)) {
    .is_deprecated("colnames_from", "names_from")
    if (is.null(names_from)) {
      names_from <- colnames_from
    }
  }
  if (!missing(rows_from)) {
    .is_deprecated("rows_from", "id_cols")
    if (is.null(id_cols)) {
      id_cols <- rows_from
    }
  }
  if (!missing(sep)) {
    .is_deprecated("sep", "names_sep")
    if (is.null(names_sep)) {
      names_sep <- sep
    }
  }
  old_names <- names(data)

  # Preserve attributes
  if (inherits(data, "tbl_df")) {
    tbl_input <- TRUE
    data <- as.data.frame(data, stringsAsFactors = FALSE)
  } else {
    tbl_input <- FALSE
  }
  variable_attr <- lapply(data, attributes)

  not_selected <- setdiff(names(data), c(names_from, values_from))
  not_unstacked <- data[, not_selected, drop = FALSE]
  not_unstacked <- unique(not_unstacked)

  unstack_fml <- as.formula(
    paste(
      paste(values_from, collapse = " + "),
      "~",
      paste(names_from, collapse = " + ")
    )
  )
  unstack_order <- unique(data[[names_from]])

  unstacked <- unstack(data, unstack_fml)
  unstacked <- unstacked[, unstack_order]

  out <- cbind(not_unstacked, unstacked)
  row.names(out) <- NULL

  # add back attributes where possible
  for (i in colnames(out)) {
    attributes(out[[i]]) <- variable_attr[[i]]
  }

  if (isTRUE(tbl_input)) {
    class(out) <- c("tbl_df", "tbl", "data.frame")
  }

  out
}
