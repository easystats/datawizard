test_that("row_means/sums", {
  d_mn <- data.frame(
    c1 = c(1, 2, NA, 4),
    c2 = c(NA, 2, NA, 5),
    c3 = c(NA, 4, NA, NA),
    c4 = c(2, 3, 7, 8)
  )
  expect_equal(row_means(d_mn, min_valid = 4), c(NA, 2.75, NA, NA), tolerance = 1e-3)
  expect_equal(row_means(d_mn, min_valid = 3), c(NA, 2.75, NA, 5.66667), tolerance = 1e-3)
  expect_equal(row_means(d_mn, min_valid = 2), c(1.5, 2.75, NA, 5.66667), tolerance = 1e-3)
  expect_equal(row_means(d_mn, min_valid = 1), c(1.5, 2.75, 7, 5.66667), tolerance = 1e-3)
  expect_equal(row_means(d_mn, min_valid = 0.5), c(1.5, 2.75, NA, 5.66667), tolerance = 1e-3)
  expect_equal(row_means(d_mn, min_valid = 0.75), c(NA, 2.75, NA, 5.66667), tolerance = 1e-3)
  expect_equal(row_means(d_mn, min_valid = 2, digits = 1), c(1.5, 2.8, NA, 5.7), tolerance = 1e-1)
  expect_message(row_means(iris), regex = "Only numeric")
  expect_equal(row_means(iris, verbose = FALSE), rowMeans(iris[, 1:4]), tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(row_sums(d_mn, min_valid = 4), c(NA, 11, NA, NA), tolerance = 1e-3)
  expect_equal(row_sums(d_mn, min_valid = 3), c(NA, 11, NA, 17), tolerance = 1e-3)
  expect_message(row_sums(iris), regex = "Only numeric")
})

test_that("row_means/sums, errors or messages", {
  data(iris)
  expect_error(expect_warning(row_means(iris, select = "abc")), regex = "No columns")
  expect_error(expect_warning(row_sums(iris, select = "abc")), regex = "No columns")
  expect_error(row_means(iris[1], min_valid = 1), regex = "two numeric")
  expect_error(row_means(iris, min_valid = 1:4), regex = "numeric value")
  expect_error(row_means(iris, min_valid = "a"), regex = "numeric value")
  expect_message(row_means(iris[1:3, ], min_valid = 3), regex = "Only numeric")
  expect_silent(row_means(iris[1:3, ], min_valid = 3, verbose = FALSE))
  expect_error(row_sums(iris[1], min_valid = 1), regex = "two numeric")
  expect_message(row_sums(iris[1:3, ], min_valid = 3), regex = "Only numeric")
  expect_silent(row_sums(iris[1:3, ], min_valid = 3, verbose = FALSE))
})
