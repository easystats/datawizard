#' Partition data
#'
#' Creates data partitions (for instance, a training and a test set) based on a
#' dataframe that can also be stratified (i.e., evenly spread a given factor)
#' using the `group` argument.
#'
#' @inheritParams data_rename
#' @param prob The proportion (between 0 and 1) of the training
#'   set. The remaining part will be used for the test set.
#' @param training_proportion This argument is the old name of the `prob` argument.
#'   It got renamed to `prob` as it can now handle more than one value and split
#'   the data into multiple partitions.
#' @param group A character vector indicating the name(s) of the column(s) used
#'   for stratified partitioning.
#' @param seed A random number generator seed. Enter an integer (e.g. 123) so
#'   that the random sampling will be the same each time you run the function.
#'
#' @return A list of two data frames, named `test` and `training`.
#'
#' @examples
#' data <- iris
#' data$Smell <- rep(c("Strong", "Strong", "Light"), 50)
#'
#' out <- data_partition(data, prob = 0.9)
#' out$test
#' nrow(out$training)
#'
#' # Stratify by group (equal proportions of each species)
#' out <- data_partition(data, prob = 0.9, group = "Species")
#' out$test
#'
#' out <- data_partition(data, prob = 0.9, group = c("Species", "Smell"))
#' out$test
#'
#' # Create multiple partitions (this currrently doesn't work!)
#' out <- data_partition(data, prob = c(0.3, 0.3))
#'
#' @inherit data_rename seealso
#' @export
data_partition <- function(data,
                           prob = 0.7,
                           group = NULL,
                           seed = NULL,
                           training_proportion = prob,
                           ...) {

  # Sanity check
  if (!is.data.frame(data)) {
    data <- tryCatch(as.data.frame(data), error = function(e) NULL)
    if (is.null(data)) {
      stop(insight::format_message("`data` needs to be a data frame, or an object that can be coerced to a data frame."))
    }
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (sum(prob) > 1) {
    stop("`prob` cannot be higher than 1.")
  }

  # Create list of data groups
  data$.row_id <- 1:nrow(data)
  if (is.null(group)) {
    indices_list <- list(1:nrow(data))
  } else {
    indices_list <- lapply(
      split(data, data[group]),
      data_extract,
      select = ".row_id"
    )
  }

  total_n <- nrow(data)
  out <- c()
  training_ids <- c()

  training_sets <- lapply(indices_list, function(i) {
    d <- list()
    indices <- i
    n <- length(indices)
    for (p in prob) {
      training <- sort(sample(indices, round(n * p), FALSE))
      indices <- setdiff(indices, training)
      out <- c(out, training)
      d[[length(d) + 1]] <- data[training, ]
    }
    training_ids <- c(training_ids, out)
    d
  })

  if (!is.null(group)) {
    training_sets <- lapply(1:length(prob), function(p) {
      do.call(rbind, lapply(training_sets, function(i) i[[p]]))
    })
  } else {
    training_sets <- training_sets[[1]]
  }

  names(training_sets) <- sprintf("p=%g", prob)
  training_sets <- c(training_sets, list(test = data[-unlist(lapply(training_sets, data_extract, select = ".row_id")), ]))

  training_sets
}
