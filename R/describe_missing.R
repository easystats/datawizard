#' @title Describe Missing Values in Data According to Guidelines
#'
#' @description Provides a detailed description of missing values in a data frame.
#' This function reports both absolute and percentage missing values of specified
#' column lists or scales, following recommended guidelines. Some authors recommend
#' reporting item-level missingness per scale, as well as a participant's maximum
#' number of missing items by scale. For example, Parent (2013) writes:
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
#' @param scales The scale names to check for missing values (as a character vector).
#' @keywords missing values NA guidelines
#' @return A dataframe with the following columns:
#'  - `var`: Variables selected.
#'  - `items`: Number of items for selected variables.
#'  - `na`: Number of missing values for those variables.
#'  - `cells`: Total number of cells (i.e., number of participants multiplied by
#'  the number of variables, `items`).
#'  - `na_percent`: The percentage of missing values (`na` divided by `cells`).
#'  - `na_max`: The number of missing values for the participant with the most
#'  missing values for the selected variables.
#'  - `na_max_percent`: The amount of missing values for the participant with
#'  the most missing values for the selected variables, as a percentage
#'  (i.e., `na_max` divided by the number of selected variables, `items`).
#'  - `all_na`: The number of participants missing 100% of items for that scale
#'  (the selected variables).
#'
#' @export
#' @references Parent, M. C. (2013). Handling item-level missing
#' data: Simpler is just as good. *The Counseling Psychologist*,
#' *41*(4), 568-600. https://doi.org/10.1177%2F0011000012445176
#' @examples
#' # Use the entire data frame
#' describe_missing(airquality)
#'
#' # Use selected columns explicitly
#' describe_missing(airquality,
#'   vars = list(
#'     c("Ozone", "Solar.R", "Wind"),
#'     c("Temp", "Month", "Day")
#'   )
#' )
#'
#' # If the questionnaire items start with the same name, e.g.,
#' set.seed(15)
#' fun <- function() {
#'   c(sample(c(NA, 1:10), replace = TRUE), NA, NA, NA)
#' }
#' df <- data.frame(
#'   ID = c("idz", NA),
#'   open_1 = fun(), open_2 = fun(), open_3 = fun(),
#'   extrovert_1 = fun(), extrovert_2 = fun(), extrovert_3 = fun(),
#'   agreeable_1 = fun(), agreeable_2 = fun(), agreeable_3 = fun()
#' )
#'
#' # One can list the scale names directly:
#' describe_missing(df, scales = c("ID", "open", "extrovert", "agreeable"))
describe_missing <- function(data, vars = NULL, scales = NULL) {
  classes <- lapply(data, class)
  if (missing(vars) && missing(scales)) {
    vars.internal <- names(data)
  } else if (!missing(scales)) {
    vars.internal <- lapply(scales, function(x) {
      grep(paste0("^", x), names(data), value = TRUE)
    })
  }
  if (!missing(vars)) {
    vars.internal <- vars
  }
  if (!is.list(vars.internal)) {
    vars.internal <- list(vars.internal)
  }
  na_df <- .describe_missing(data)
  if (!missing(vars) || !missing(scales)) {
    na_list <- lapply(vars.internal, function(x) {
      data_subset <- data[, x, drop = FALSE]
      .describe_missing(data_subset)
    })
    na_df$var <- "Total"
    na_df <- do.call(rbind, c(na_list, list(na_df)))
  }
  na_df
}

.describe_missing <- function(data) {
  my_var <- paste0(names(data)[1], ":", names(data)[ncol(data)])
  items <- ncol(data)
  na <- sum(is.na(data))
  cells <- nrow(data) * ncol(data)
  na_percent <- round(na / cells * 100, 2)
  na_max <- max(rowSums(is.na(data)))
  na_max_percent <- round(na_max / items * 100, 2)
  all_na <- sum(apply(data, 1, function(x) all(is.na(x))))

  data.frame(
    var = my_var,
    items = items,
    na = na,
    cells = cells,
    na_percent = na_percent,
    na_max = na_max,
    na_max_percent = na_max_percent,
    all_na = all_na
  )
}
