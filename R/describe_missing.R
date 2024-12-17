#' @title Describe Missing Values in Data According to Guidelines
#'
#' @description Provides a detailed description of missing values in a data frame.
#' This function reports both absolute and percentage missing values of specified
#' column lists or scales, following recommended guidelines.
#'
#' @details
#' In psychology, it is common to ask participants to answer questionnaires in
#' which people answer several questions about a specific topic. For example,
#' people could answer 10 different questions about how extroversioned they are.
#' In turn, researchers calculate the average for those 10 questions (called
#' items). These questionnaires are called (e.g., Likert) "scales" (such as the
#' Rosenberg Self-Esteem Scale, also known as the RSES).
#'
#' Some authors recommend reporting item-level missingness per scale, as well
#' as a participant's maximum number of missing items by scale. For example,
#' Parent (2013) writes:
#'
#' *I recommend that authors (a) state their tolerance level for missing data by scale
#' or subscale (e.g., "We calculated means for all subscales on which participants gave
#' at least 75% complete data") and then (b) report the individual missingness rates
#' by scale per data point (i.e., the number of missing values out of all data points
#' on that scale for all participants) and the maximum by participant (e.g., "For Attachment
#' Anxiety, a total of 4 missing data points out of 100 were observed, with no participant
#' missing more than a single data point").*
#'
#' @param data The data frame to be analyzed.
#' @param vars Variable (or lists of variables) to check for missing values (NAs).
#' @param scales If you rely on composite scores such as psychological scales
#' or questionnaires, you can provide the shared suffix among those variables
#' (as a character vector). This is useful if the variables you want to check
#' the average of all start with the same name (e.g., `varx`), such as is
#' commonly the case for Likert scales (such as `varx_1`, `varx_2`, `varx_3`,
#' etc.).
#' @return A dataframe with the following columns:
#'  - `variable`: Variables selected.
#'  - `n_columns`: Number of items for selected variables.
#'  - `n_missing`: Number of missing values for those variables (NA stands for Not
#'  Available).
#'  - `n_cells`: Total number of cells (i.e., number of participants multiplied by
#'  the number of columns, `n_columns`).
#'  - `missing_percent`: The percentage of missing values (`na` divided by `cells`).
#'  - `missing_max`: The number of missing values for the participant with the most
#'  missing values for the selected variables.
#'  - `missing_max_percent`: The amount of missing values for the participant with
#'  the most missing values for the selected variables, as a percentage
#'  (i.e., `missing_max` divided by the number of selected columns, `n_columns`).
#'  - `all_missing`: The number of participants missing 100% of items for that scale
#'  (the selected variables).
#' @param ... Arguments passed down to other functions. Currently not used.
#'
#' @export
#' @references Parent, M. C. (2013). Handling item-level missing
#' data: Simpler is just as good. *The Counseling Psychologist*,
#' *41*(4), 568-600. https://doi.org/10.1177%2F0011000012445176
#' @examples
#' # Use the entire data frame
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
#' describe_missing(df)
#'
#' # If the questionnaire items start with the same name,
#' # one can list the scale names directly:
#' describe_missing(df, scales = c("ID", "openness", "extroversion", "agreeableness"))
#'
#' # Otherwise you can provide nested columns manually:
#' describe_missing(df,
#'   select = list(
#'     c("ID"),
#'     c("openness_1", "openness_2", "openness_3"),
#'     c("extroversion_1", "extroversion_2", "extroversion_3"),
#'     c("agreeableness_1", "agreeableness_2", "agreeableness_3")
#'   )
#' )
#'

describe_missing <- function(data, select = NULL, scales = NULL, ...) {
  vars <- select
  if (!is.null(vars) && missing(scales)) {
    vars.internal <- names(data)
  } else if (!missing(scales)) {
    vars.internal <- lapply(scales, function(x) {
      grep(paste0("^", x), names(data), value = TRUE)
    })
  } else if (is.null(vars) && missing(scales)){
    vars <- as.list(names(data))
  }
  if (!is.null(vars)) {
    vars.internal <- vars
  }
  if (!is.list(vars.internal)) {
    vars.internal <- list(vars.internal)
  }
  na_df <- .describe_missing(data)
  if (!is.null(vars) || !missing(scales)) {
    na_list <- lapply(vars.internal, function(x) {
      data_subset <- data[, x, drop = FALSE]
      .describe_missing(data_subset)
    })
    na_df$variable <- "Total"
    na_df <- do.call(rbind, c(na_list, list(na_df)))
  }
  na_df
}

.describe_missing <- function(data) {
  if (ncol(data) > 1) {
    my_var <- paste0(names(data)[1], ":", names(data)[ncol(data)])
  } else {
    my_var <- names(data)
  }
  items <- ncol(data)
  na <- sum(is.na(data))
  cells <- nrow(data) * ncol(data)
  na_percent <- round(na / cells * 100, 2)
  na_max <- max(rowSums(is.na(data)))
  na_max_percent <- round(na_max / items * 100, 2)
  all_na <- sum(apply(data, 1, function(x) all(is.na(x))))

  data.frame(
    variable = my_var,
    n_columns = items,
    n_missing = na,
    n_cells = cells,
    missing_percent = na_percent,
    complete_percent = 100 - na_percent,
    missing_max = na_max,
    missing_max_percent = na_max_percent,
    all_missing = all_na
  )
}
