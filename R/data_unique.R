#' @title Keep only one row from all with duplicated IDs
#'
#' @description From all rows with at least one duplicated ID,
#' keep only one. Methods for selecting the duplicated row are
#' either the first duplicate, the last duplicate, or the "best"
#' duplicate (default), based on the duplicate with the fewer
#' number of NA values. In case of ties, it picks the first
#' duplicate, as it is the one most likely to be valid and
#' authentic, given practice effects.
#'
#' @param data The data frame.
#' @param id The ID variable for which to check for duplicates.
#' @param method The method to be used for duplicate selection,
#' either "best" (the default), "first", or "last".
#' @return A dataframe, containing only the chosen duplicates.
#' @export
#' @examples
#' df1 <- data.frame(
#'    id = c(1, 2, 3, 1, 3),
#'    item1 = c(NA, 1, 1, 2, 3),
#'    item2 = c(NA, 1, 1, 2, 3),
#'    item3 = c(NA, 1, 1, 2, 3)
#' )
#'
#' data_unique(df1, id = "id")

data_unique <- function(data, id, method = "best") {

  og.names <- names(data)
  dups <- data_duplicated(data, id)
  dups.n <- sum(duplicated(dups[[id]]))
  good.dups <- data_group(dups, id)

  # Attempt at grouped dplyr::slice_min
  if (method == "best") {
    min.index <- NULL
    for (i in unique(good.dups[[id]])) {
      temp.data <- data_filter(dups, id == {i})
      min.index[i] <- min(temp.data$count_na)
      index2 <- which(dups$count_na == min.index[i])
      temp.data <- data_filter(dups, index2)
    }
    good.dups <- temp.data
  }

  if (method != "last") {
    good.dups <- good.dups[!duplicated(good.dups[[id]]), ]
  } else {
    good.dups <- good.dups[duplicated(good.dups[[id]]), ]
  }

  good.dups <- data_select(good.dups, og.names)
  good.data <- data[!duplicated(data[[id]]), ]
  match.index <- good.data[[id]] %in% good.dups[[id]]
  good.data[match.index, ] <- good.dups

  dup.msg <- paste0(" duplicates removed, with method '", method, "')")
  dup.msg <- c(dup.msg, ifelse(dups.n != 69, "", " 69... nice"))
  message("(", dups.n, dup.msg, sep = "")
  good.data

}
