test_that("data_partition works as expected", {
  # not supported

  expect_error(
    data_partition(new.env()),
    "`data` must be a data frame"
  )

  # to be coerced to data frames

  expect_snapshot(data_partition(letters, seed = 123))

  # validation checks

  expect_warning(
    data_partition(iris, 0.7, row_id = "Species"),
    "exists"
  )

  expect_warning(expect_warning(
    data_partition(iris, c(0.7, 0.3), row_id = "Species"),
    "generated"
  ))

  # values

  out <- data_partition(mtcars, proportion = 0.8, seed = 123)

  expect_identical(
    out$p_0.8$.row_id,
    c(
      1L, 3L, 4L, 5L, 7L, 8L, 9L, 10L, 11L, 14L, 15L, 17L, 18L, 19L,
      20L, 21L, 22L, 23L, 24L, 26L, 27L, 28L, 29L, 30L, 31L, 32L
    )
  )

  expect_identical(
    colnames(out$p_0.8),
    c(
      "mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am",
      "gear", "carb", ".row_id"
    )
  )

  expect_identical(
    lapply(out, nrow),
    list(p_0.8 = 26L, test = 6L)
  )


  # data frames

  data(iris)
  expect_snapshot(str(data_partition(iris, proportion = 0.7, seed = 123)))
  expect_snapshot(str(data_partition(iris, proportion = c(0.2, 0.5), seed = 123)))
  expect_snapshot(str(data_partition(iris, proportion = 0.7, by = "Species", seed = 123)))
  expect_snapshot(str(data_partition(iris, proportion = c(0.2, 0.5), by = "Species", seed = 123)))
})

test_that("data_partition warns if no testing set", {
  expect_warning(
    data_partition(iris, proportion = 1),
    "sums up to 1"
  )
  expect_warning(
    data_partition(iris, proportion = c(0.5, 0.5)),
    "sums up to 1"
  )
})

test_that("data_partition errors if values in proportion not between 0 and 1", {
  expect_error(
    data_partition(iris, proportion = 1.3),
    "cannot be higher"
  )
  expect_error(
    data_partition(iris, proportion = c(0.5, 0.6)),
    "cannot be higher"
  )
  expect_error(
    data_partition(iris, proportion = c(1.3, -1)),
    "cannot be negative"
  )
  expect_error(
    data_partition(iris, proportion = -1),
    "cannot be negative"
  )
})

test_that("data_partition warns if row_id already exists", {
  iris2 <- iris

  iris2[[".row_id"]] <- "A"
  expect_warning(
    data_partition(iris2, proportion = 0.5),
    "already exists"
  )

  iris2[["foo"]] <- "A"
  expect_warning(
    data_partition(iris2, proportion = 0.5, row_id = "foo"),
    "already exists"
  )

  part1 <- data_partition(iris, proportion = 0.5, seed = 123)
  part2 <- suppressWarnings(data_partition(iris2, proportion = 0.5, seed = 123))

  expect_identical(
    part1$p_0.5[1:5],
    part2$p_0.5[1:5]
  )
})
