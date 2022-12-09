#' @title Extract all duplicates
#'
#' @description Extract all duplicates, for visual inspection.
#' Note that it also contains the first occurrence of future
#' duplicates, unlike [duplicated()] or [dplyr::distinct()]). Also
#' contains an additional column reporting the number of missing
#' values for that row, to help in the decision-making when
#' selecting which duplicates to keep.
#'
#' @inheritParams find_columns
#'
#' @keywords duplicates
#' @export
#' @seealso
#' [data_unique()]
#' @return A dataframe, containing all duplicates.
#' @examples
#' df1 <- data.frame(
#'   id = c(1, 2, 3, 1, 3),
#'   year = c(2022, 2022, 2022, 2022, 2000),
#'   item1 = c(NA, 1, 1, 2, 3),
#'   item2 = c(NA, 1, 1, 2, 3),
#'   item3 = c(NA, 1, 1, 2, 3)
#' )
#'
#' data_duplicated(df1, select = "id")
#'
#' data_duplicated(df1, select = c("id", "year"))
#'
#' # Filter to exclude duplicates
#' df2 <- df1[-c(1, 5), ]
#' df2
#'
data_duplicated <- function(data,
                            select = NULL,
                            exclude = NULL,
                            ignore_case = FALSE,
                            regex = FALSE,
                            verbose = TRUE) {
  UseMethod("data_duplicated")
}

#' @export
data_duplicated.data.frame <- function(data,
                                       select = NULL,
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

  data$temporary_id <- do.call(paste, c(data_select(data, select), sep = "_"))

  data <- cbind(Row = seq_len(nrow(data)), data)
  dups.index <- data$temporary_id %in% data$temporary_id[duplicated(data$temporary_id)]
  dups <- data[dups.index, ]
  dups$count_na <- rowSums(is.na(dups))
  dups <- data_arrange(dups, select)
  dups <- data_remove(dups, "temporary_id")
  dups
}

#' @export
data_duplicated.grouped_df <- function(data,
                                       select = NULL,
                                       exclude = NULL,
                                       ignore_case = FALSE,
                                       regex = FALSE,
                                       verbose = TRUE) {
  select <- .select_nse(select,
    data,
    exclude = exclude,
    ignore_case = ignore_case,
    regex = regex,
    verbose = verbose
  )

  # works only for dplyr >= 0.8.0
  grps <- attr(data, "groups", exact = TRUE)
  grps <- grps[[".rows"]]

  out <- lapply(grps, function(x) {
    data_duplicated.data.frame(data[x, ], select = select)
  })

  out <- do.call(rbind, out)

  out
}
