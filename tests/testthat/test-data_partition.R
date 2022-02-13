test_that("data_partition works as expected", {
  df <- iris
  df$Smell <- rep(c("Strong", "Light"), 75)

  set.seed(123)
  expect_snapshot(str(data_partition(df)))

  set.seed(123)
  expect_snapshot(str(data_partition(df, group = "Species")))

  set.seed(123)
  expect_snapshot(str(data_partition(df, group = c("Species", "Smell"))))
})
