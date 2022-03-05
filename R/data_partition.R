#' Partition data into a test and a training set
#'
#' Creates a training and a test set based on a dataframe. Can also be
#' stratified (i.e., evenly spread a given factor) using the `group`
#' argument.
#'
#' @inheritParams data_rename
#' @param training_proportion The proportion (between 0 and 1) of the training
#'   set. The remaining part will be used for the test set.
#' @param group A character vector indicating the name(s) of the column(s) used
#'   for stratified partitioning.
#' @param seed A random number generator seed. Enter an integer (e.g. 123) so
#'   that the random sampling will be the same each time you run the function.
#'
#' @return A list of two data frames, named `test` and `training`.
#'
#' @examples
#' df <- iris
#' df$Smell <- rep(c("Strong", "Light"), 75)
#'
#' data_partition(df)
#' data_partition(df, group = "Species")
#' data_partition(df, group = c("Species", "Smell"))
#' @inherit data_rename seealso
#' @export
data_partition <- function(data,
                           training_proportion = 0.7,
                           group = NULL,
                           seed = NULL,
                           ...) {
  if (!is.data.frame(data)) {
    data <- tryCatch(
      expr = {
        as.data.frame(data)
      },
      error = function(e) {
        NULL
      }
    )

    if (is.null(data)) {
      stop("`data` needs to be a data frame, or an object that can be coerced to a data frame.")
    }
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }

  training <- data.frame()
  test <- data.frame()

  if (!is.null(group)) {
    for (i in split(data, data[group])) {
      out <- .data_partition(i, training_proportion)
      training <- rbind(training, i[out$training, ])
      test <- rbind(test, i[out$test, ])
    }
  } else {
    out <- .data_partition(data, training_proportion)
    training <- rbind(training, data[out$training, ])
    test <- rbind(test, data[out$test, ])
  }

  list(
    training = training,
    test = test
  )
}


#' @keywords internal
.data_partition <- function(data, training_proportion = 0.8) {
  training_indices <- sample(1:nrow(data), size = training_proportion * nrow(data))
  test_indices <- (1:nrow(data))[-training_indices]

  list(
    training = training_indices,
    test = test_indices
  )
}
