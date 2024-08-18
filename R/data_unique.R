#' @title Keep only one row from all with duplicated IDs
#'
#' @description From all rows with at least one duplicated ID,
#' keep only one. Methods for selecting the duplicated row are
#' either the first duplicate, the last duplicate, or the "best"
#' duplicate (default), based on the duplicate with the smallest
#' number of `NA`. In case of ties, it picks the first
#' duplicate, as it is the one most likely to be valid and
#' authentic, given practice effects.
#'
#' Contrarily to `dplyr::distinct()`, `data_unique()` keeps all columns.
#'
#' @param keep The method to be used for duplicate selection, either "best"
#'   (the default), "first", or "last".
#' @inheritParams extract_column_names
#'
#' @return A data frame, containing only the chosen duplicates.
#' @seealso [data_duplicated()]
#' @examples
#' df1 <- data.frame(
#'   id = c(1, 2, 3, 1, 3),
#'   item1 = c(NA, 1, 1, 2, 3),
#'   item2 = c(NA, 1, 1, 2, 3),
#'   item3 = c(NA, 1, 1, 2, 3)
#' )
#'
#' data_unique(df1, select = "id")
#' @export
data_unique <- function(data,
                        select = NULL,
                        keep = "best",
                        exclude = NULL,
                        ignore_case = FALSE,
                        regex = FALSE,
                        verbose = TRUE) {
  UseMethod("data_unique")
}


#' @export
data_unique.data.frame <- function(data,
                                   select = NULL,
                                   keep = "best",
                                   exclude = NULL,
                                   ignore_case = FALSE,
                                   regex = FALSE,
                                   verbose = TRUE) {
  select <- .select_nse(
    select,
    data,
    exclude = exclude,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose
  )

  # temporary_id <- paste0(sample(letters), collapse = "")
  data$temporary_id2 <- do.call(paste, c(data_select(data, select), sep = "_"))

  og.names <- names(data)
  dups <- data_duplicated(data, select)

  # if no duplicates, return the original data
  if (nrow(dups) == 0L) {
    data <- data_remove(data, "temporary_id2")
    return(data)
  }

  # count number of duplicates
  dups.n <- sum(duplicated(dups$temporary_id2))
  good.dups <- data_group(dups, "temporary_id2")

  # keep row that has the least duplicates
  if (keep == "best") {
    good.dups <- data_filter(good.dups, "count_na == min(count_na)")
  }

  good.dups <- good.dups[!duplicated(good.dups$temporary_id2,
    fromLast = keep == "last"
  ), ]

  good.dups <- data_select(good.dups, og.names)
  out <- data[!duplicated(data$temporary_id2), ]


  if (keep != "first") {
    match.index <- out$temporary_id2 %in% good.dups$temporary_id2
    out[match.index, ] <- good.dups
  }

  # id is not useful anymore
  out <- data_remove(out, "temporary_id2")

  if (verbose) {
    dup.msg <- sprintf("(%s duplicates removed, with method '%s')", dups.n, keep)
    dup.msg <- paste0(dup.msg, ifelse(dups.n != 69, "", " 69... nice"))
    insight::format_alert(dup.msg)
  }

  out
}


#' @export
data_unique.grouped_df <- function(data,
                                   select = NULL,
                                   keep = "best",
                                   exclude = NULL,
                                   ignore_case = FALSE,
                                   regex = FALSE,
                                   verbose = TRUE) {
  select <- .select_nse(
    select,
    data,
    exclude = exclude,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose
  )


  grps <- attr(data, "groups", exact = TRUE)
  grps <- grps[[".rows"]]

  data2 <- data_ungroup(data)

  out <- lapply(grps, function(x) {
    data_unique.data.frame(data2[x, ], select = select, keep = keep, verbose = verbose)
  })

  out <- do.call(rbind, out)

  if (!insight::object_has_rownames(data)) {
    rownames(out) <- NULL
  }

  class(out) <- class(data)
  attr(out, "groups") <- attr(data, "groups")

  out
}
