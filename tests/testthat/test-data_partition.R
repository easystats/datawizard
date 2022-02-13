test_that("data_partition works as expected", {
  # not supported

  expect_error(
    data_partition(new.env()),
    "`data` needs to be a data frame, or an object that can be coerced to a data frame."
  )

  # to be coerced to dataframes

  expect_snapshot(data_partition(letters, seed = 123))

  # dataframes

  df <- iris
  set.seed(123)
  df$Smell <- rep(c("Strong", "Light"), 75)

  set.seed(123)
  expect_snapshot(str(data_partition(df)))

  set.seed(123)
  expect_snapshot(str(data_partition(df, group = "Species")))

  set.seed(123)
  expect_snapshot(str(data_partition(df, group = c("Species", "Smell"))))
})
