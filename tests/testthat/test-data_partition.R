test_that("data_partition works as expected", {
  # not supported

  expect_error(
    data_partition(new.env()),
    "`data` needs to be a data frame"
  )

  # to be coerced to data frames

  expect_snapshot(data_partition(letters, seed = 123))

  # sanity checks

  expect_warning(
    data_partition(iris, 0.7, row_id = "Species"),
    "exists"
  )

  expect_warning(expect_warning(
    data_partition(iris, c(0.7, 0.3), row_id = "Species"),
    "generated"
  ))

  # values

  out <- data_partition(mtcars, proportion = .8, seed = 123)

  expect_equal(
    out$p_0.8$.row_id,
    c(
      1L, 3L, 4L, 5L, 7L, 8L, 9L, 10L, 11L, 14L, 15L, 17L, 18L, 19L,
      20L, 21L, 22L, 23L, 24L, 26L, 27L, 28L, 29L, 30L, 31L, 32L
    )
  )

  expect_equal(
    colnames(out$p_0.8),
    c(
      "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb", ".row_id"
    )
  )

  expect_equal(
    lapply(out, nrow),
    list(p_0.8 = 26L, test = 6L)
  )


  # data frames

  data(iris)
  expect_snapshot(str(data_partition(iris, proportion = .7, seed = 123)))
  expect_snapshot(str(data_partition(iris, proportion = c(.2, .5), seed = 123)))
  expect_snapshot(str(data_partition(iris, proportion = .7, group = "Species", seed = 123)))
  expect_snapshot(str(data_partition(iris, proportion = c(.2, .5), group = "Species", seed = 123)))
})
