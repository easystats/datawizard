#' @title Describe Missing Values in Data According to Guidelines
#'
#' @description Provides a detailed description of missing values in a data frame.
#' This function reports both absolute and percentage missing values of specified
#' variables.
#'
#' @inheritParams extract_column_names
#' @param by Optional character string, indicating the names of one or more
#' variables in the data frame. If supplied, the data will be split by these
#' variables and summary statistics will be computed for each group. Useful
#' for survey data by first reshaping the data to the long format.
#' @param sort Logical. Whether to sort the result from highest to lowest
#' percentage of missing data.
#' @return A dataframe with the following columns:
#'  - `variable`: Variables selected.
#'  - `n_missing`: Number of missing values.
#'  - `missing_percent`: Percentage of missing values.
#'  - `complete_percent`: Percentage of non-missing values.
#' @param ... Arguments passed down to other functions. Currently not used.
#'
#' @export
#' @examples
#' describe_missing(airquality)
#'
#' # Survey data
#' set.seed(15)
#' fun <- function() {
#'   c(sample(c(NA, 1:10), replace = TRUE), NA, NA, NA)
#' }
#' df <- data.frame(
#'   ID = c("idz", NA),
#'   openness_1 = fun(), openness_2 = fun(), openness_3 = fun(),
#'   extroversion_1 = fun(), extroversion_2 = fun(), extroversion_3 = fun(),
#'   agreeableness_1 = fun(), agreeableness_2 = fun(), agreeableness_3 = fun()
#' )
#'
#' df_long <- reshape_longer(
#'   df,
#'   select = -1,
#'   names_sep = "_",
#'   names_to = c("dimension", "item")
#' )
#'
#' describe_missing(
#'   df_long,
#'   select = -c(1, 3),
#'   by = "dimension"
#' )
#'
describe_missing <- function(data,
                             select = NULL,
                             exclude = NULL,
                             ignore_case = FALSE,
                             regex = FALSE,
                             verbose = TRUE,
                             by = NULL,
                             sort = FALSE,
                             ...) {
  if (!is.null(select) || !is.null(exclude)) {
    data <- data_select(
      data = data,
      select = select,
      exclude = exclude,
      ignore_case = ignore_case,
      regex = regex,
      verbose = verbose,
      ...
    )
  }
  if (is.null(by)) {
    na_list <- lapply(names(data), function(x) {
      data_subset <- data[, x, drop = FALSE]
      .describe_missing(data_subset)
    })
  } else {
    if (!by %in% names(data)) {
      stop("The 'by' column does not exist in the data.", call. = FALSE)
    }
    grouped_data <- split(data, data[[by]])
    na_list <- lapply(names(grouped_data), function(group_name) {
      group <- grouped_data[[group_name]]
      # Identify columns to analyze (exclude the 'by' column)
      cols_to_analyze <- setdiff(names(group), by)
      group_na_list <- lapply(cols_to_analyze, function(x) {
        data_subset <- group[, x, drop = FALSE]
        .describe_missing(data_subset)
      })
      group_na_df <- do.call(rbind, group_na_list)
      group_na_df$variable <- group_name
      group_na_df
    })
  }
  na_df <- do.call(rbind, na_list)
  if (isTRUE(sort)) {
    na_df <- na_df[order(-na_df$missing_percent), ]
  }
  na_df_tot <- .describe_missing(data)
  na_df_tot$variable <- "Total"
  na_df <- rbind(na_df, na_df_tot)
  na_df
}

.describe_missing <- function(data) {
  n_missing <- sum(is.na(data))
  missing_percent <- round(n_missing / (nrow(data) * ncol(data)) * 100, 2)
  data.frame(
    variable = names(data)[1],
    n_missing = n_missing,
    missing_percent = missing_percent,
    complete_percent = 100 - missing_percent
  )
}
