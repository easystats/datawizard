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
#' @param data The data frame.
#' @param select The ID variable(s) for which to check for duplicates.
#' @param keep The method to be used for duplicate selection,
#' either "best" (the default), "first", or "last".
#' @return A dataframe, containing only the chosen duplicates.
#' @seealso
#' [data_duplicated()]
#' @export
#' @examples
#' df1 <- data.frame(
#'    id = c(1, 2, 3, 1, 3),
#'    item1 = c(NA, 1, 1, 2, 3),
#'    item2 = c(NA, 1, 1, 2, 3),
#'    item3 = c(NA, 1, 1, 2, 3)
#' )
#'
#' data_unique(df1, select = "id")

data_unique <- function(data, select, keep = "best") {

  select <- .select_nse(select,
    data,
    exclude = NULL,
    ignore_case = TRUE
  )

  # temporary_id <- paste0(sample(letters), collapse = "")
  data$temporary_id2 <- do.call(paste, c(data_select(data, select), sep = "_"))

  og.names <- names(data)
  dups <- data_duplicated(data, "temporary_id2")
  dups.n <- sum(duplicated(dups$temporary_id2))
  good.dups <- data_group(dups, "temporary_id2")

  if (keep == "best") {
    good.dups <- data_filter(good.dups, "count_na == min(count_na)")
  }

  if (keep != "last") {
    good.dups <- good.dups[!duplicated(good.dups$temporary_id2), ]
  } else {
    good.dups <- good.dups[duplicated(good.dups$temporary_id2), ]
  }

  good.dups <- data_select(good.dups, og.names)
  good.data <- data[!duplicated(data$temporary_id2), ]
  match.index <- good.data$temporary_id2 %in% good.dups$temporary_id2
  good.data[match.index, ] <- good.dups

  good.data <- data_select(good.data, exclude = "temporary_id2")

  dup.msg <- sprintf("(%s duplicates removed, with method '%s')", dups.n, keep)
  dup.msg <- paste0(dup.msg, ifelse(dups.n != 69, "", " 69... nice"))
  insight::format_alert(dup.msg)
  good.data

}
