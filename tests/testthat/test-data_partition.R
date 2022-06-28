test_that("data_partition works as expected", {
  # not supported

  expect_error(
    data_partition(new.env()),
    "`data` needs to be a data frame"
  )

  # to be coerced to dataframes

  expect_snapshot(data_partition(letters, seed = 123))

  # dataframes

  data(iris)
  expect_snapshot(str(data_partition(iris, prob = .7, seed = 123)))
  expect_snapshot(str(data_partition(iris, prob = c(.2, .5), seed = 123)))
  expect_snapshot(str(data_partition(iris, prob = .7, group = "Species", seed = 123)))
  expect_snapshot(str(data_partition(iris, prob = c(.2, .5), group = "Species", seed = 123)))
})
