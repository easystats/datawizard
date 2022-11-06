#' @title Extract all duplicates
#'
#' @description Extract all duplicates, for visual inspection.
#' Note that it also contains the first occurrence of future
#' duplicates, unlike [duplicated()] or [dplyr::distinct()]). Also
#' contains an additional column reporting the number of missing
#' values for that row, to help in the decision-making when
#' selecting which duplicates to keep.
#'
#' @param data The data frame.
#' @param select The id variable for which to check for duplicates.
#' @keywords duplicates
#' @export
#' @return A dataframe, containing all duplicates.
#' @examples
#' df1 <- data.frame(
#'    id = c(1, 2, 3, 1, 3),
#'    year = c(2022, 2022, 2022, 2022, 2000),
#'    item1 = c(NA, 1, 1, 2, 3),
#'    item2 = c(NA, 1, 1, 2, 3),
#'    item3 = c(NA, 1, 1, 2, 3)
#' )
#'
#' data_duplicated(df1, select = "id")
#'
#' data_duplicated(df1, select = c("id", "year"))
#'
#' # Filter to exclude duplicates
#' df2 <- df1[-c(1, 5),]
#' df2

data_duplicated <- function(data, select) {

  data$temporary_id <- do.call(paste, c(data_select(data, select), sep = "_"))

  Row <- seq_len(nrow(data))
  data <- cbind(Row, data)
  dups.index <- data$temporary_id %in% data$temporary_id[duplicated(data$temporary_id)]
  dups <- data[dups.index, ]
  dups.n <- sum(duplicated(dups$temporary_id))
  count_na <- function(x) sum(is.na(x))
  dups$count_na <- apply(dups, 1, count_na)
  dups <- dups[order(dups$temporary_id), ]
  dups <- data_select(dups, exclude = "temporary_id")
  dups

}

