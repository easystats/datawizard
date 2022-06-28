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

  # Initialize out list
  out <- list()
  data_copy <- data
  partitions <- rep(0, nrow(data))

  # Assign to same group if no groups
  if (is.null(group)) {
    data_copy$.temp <- "TEMP"
    group <- ".temp"
  }

  # Store row index
  data_copy$.rowid <- 1:nrow(data_copy)

  # Iterate through probabilities (in case there is more than 1)
  for (i in 1:length(prob)) {
    out[i] <- c(NA) # Initialize empty value (will be dropped later)
    # Split into groups, and for each group, get the desired proportion
    for (dat in split(data_copy, data_copy[group])) {
      out[[i]] <- c(out[[i]], dat$.rowid[sample(1:nrow(dat), size = prob[i] * nrow(dat))])
    }
    # Drop NA
    out[[i]] <- as.numeric(na.omit(out[[i]]))
    # Store partition result
    partitions[out[[i]]] <- i
    # Delete used rows from pool
    data_copy <- data_copy[-out[[i]], ]
  }

  # Split according to partitions
  out <- split(data, partitions)

  # Rename if only 2, or drop "remaining" pool if more than 2
  if(length(out) == 2){
    names(out) <- c("test", "training")
  } else{
    out[["0"]] <- NULL
  }
  out
}
