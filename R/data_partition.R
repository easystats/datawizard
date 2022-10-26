#' Partition data
#'
#' Creates data partitions (for instance, a training and a test set) based on a
#' data frame that can also be stratified (i.e., evenly spread a given factor)
#' using the `group` argument.
#'
#' @inheritParams data_rename
#' @param proportion Scalar (between 0 and 1) or numeric vector, indicating the
#'   proportion(s) of the training set(s). The sum of `proportion` must not be
#'   greater than 1. The remaining part will be used for the test set.
#' @param training_proportion Deprecated, please use `proportion`.
#' @param group A character vector indicating the name(s) of the column(s) used
#'   for stratified partitioning.
#' @param seed A random number generator seed. Enter an integer (e.g. 123) so
#'   that the random sampling will be the same each time you run the function.
#' @param row_id Character string, indicating the name of the column that
#'   contains the row-id's.
#' @param verbose Toggle messages and warnings.
#'
#' @return A list of data frames. The list includes one training set per given
#'   proportion and the remaining data as test set. List elements of training
#'   sets are named after the given proportions (e.g., `$p_0.7`), the test set
#'   is named `$test`.
#'
#' @examples
#' data(iris)
#' out <- data_partition(iris, proportion = 0.9)
#' out$test
#' nrow(out$p_0.9)
#'
#' # Stratify by group (equal proportions of each species)
#' out <- data_partition(iris, proportion = 0.9, group = "Species")
#' out$test
#'
#' # Create multiple partitions
#' out <- data_partition(iris, proportion = c(0.3, 0.3))
#' lapply(out, head)
#'
#' # Create multiple partitions, stratified by group - 30% equally sampled
#' # from species in first training set, 50% in second training set and
#' # remaining 20% equally sampled from each species in test set.
#' out <- data_partition(iris, proportion = c(0.3, 0.5), group = "Species")
#' lapply(out, function(i) table(i$Species))
#'
#' @inherit data_rename seealso
#' @export
data_partition <- function(data,
                           proportion = 0.7,
                           group = NULL,
                           seed = NULL,
                           row_id = ".row_id",
                           verbose = TRUE,
                           training_proportion = proportion,
                           ...) {
  # Sanity checks
  data <- .coerce_to_dataframe(data)

  if (sum(proportion) > 1) {
    insight::format_error("Sum of `proportion` cannot be higher than 1.")
  }
  if (any(proportion < 0)) {
    insight::format_error("Values in `proportion` cannot be negative.")
  }
  if (sum(proportion) == 1 && isTRUE(verbose)) {
    insight::format_warning(
      "Proportions of sampled training sets (`proportion`) sums up to 1, so no test set will be generated."
    )
  }

  if (is.null(row_id)) {
    row_id <- ".row_id"
  }

  # check that name of row-id doesn't exist to prevent existing data
  # from overwriting. create new unique name for row-id then...
  if (row_id %in% colnames(data)) {
    if (isTRUE(verbose)) {
      insight::format_warning(
        paste0("A variable named \"", row_id, "\" already exists."),
        "Changing the value of `row_id` to a unique variable name now."
      )
    }
    unique_names <- make.unique(c(colnames(data), row_id), sep = "_")
    row_id <- unique_names[length(unique_names)]
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  # add row-id column
  data[[row_id]] <- seq_len(nrow(data))

  # Create list of data groups. We generally lapply over list of
  # sampled row-id's by group, thus, we even create a list if not grouped.
  if (is.null(group)) {
    indices_list <- list(seq_len(nrow(data)))
  } else {
    # else, split by group(s) and extract row-ids per group
    indices_list <- lapply(
      split(data, data[group]),
      data_extract,
      select = row_id,
      as_data_frame = FALSE
    )
  }

  # iterate over (grouped) row-id's
  training_sets <- lapply(indices_list, function(i) {
    # return value, list of data frames
    d <- list()

    # row-id's by groups
    indices <- i

    # check length of group (= data)
    n <- length(indices)

    # iterate probabilities. we use for/next, so we can change
    # the "indices" variable, where we remove already sampled id's
    for (p in proportion) {
      # training-id's, sampled from id's per group - size is % within each group
      training <- sort(sample(indices, round(n * p)))

      # remove already sampled id's from group-indices
      indices <- setdiff(indices, training)

      # each training set data frame as one list element
      d[[length(d) + 1]] <- data[training, ]
    }
    d
  })

  # we need to move all list elements one level higher.
  if (!is.null(group)) {
    # for grouped training sets, we need to row-bind all sampled training
    # sets from each group. currently, we have a list of data frames,
    # grouped by "group"; but we want one data frame per proportion that
    # contains sampled rows from all groups.
    training_sets <- lapply(seq_along(proportion), function(p) {
      do.call(rbind, lapply(training_sets, function(i) i[[p]]))
    })
  } else {
    # else, just move first list element one level higher
    training_sets <- training_sets[[1]]
  }

  # use probabilies as element names
  names(training_sets) <- sprintf("p_%g", proportion)

  # remove all training set id's from data, add remaining data (= test set)
  out <- c(
    training_sets,
    list(test = data[-unlist(lapply(training_sets, data_extract, select = row_id, as_data_frame = FALSE)), ])
  )

  lapply(out, `row.names<-`, NULL)
}
