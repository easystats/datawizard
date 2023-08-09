test_that("rowmean_n", {
  d_mn <- data.frame(
    c1 = c(1, 2, NA, 4),
    c2 = c(NA, 2, NA, 5),
    c3 = c(NA, 4, NA, NA),
    c4 = c(2, 3, 7, 8)
  )
  expect_equal(rowmean_n(d_mn, 4), c(NA, 2.75, NA, NA), tolerance = 1e-3)
  expect_equal(rowmean_n(d_mn, 3), c(NA, 2.75, NA, 5.66667), tolerance = 1e-3)
  expect_equal(rowmean_n(d_mn, 2), c(1.5, 2.75, NA, 5.66667), tolerance = 1e-3)
  expect_equal(rowmean_n(d_mn, 1), c(1.5, 2.75, 7, 5.66667), tolerance = 1e-3)
  expect_equal(rowmean_n(d_mn, 0.5), c(1.5, 2.75, NA, 5.66667), tolerance = 1e-3)
  expect_equal(rowmean_n(d_mn, 0.75), c(NA, 2.75, NA, 5.66667), tolerance = 1e-3)
  expect_equal(rowmean_n(d_mn, 2, digits = 1), c(1.5, 2.8, NA, 5.7), tolerance = 1e-1)
})

test_that("rowmean_n, errors or messages", {
  data(iris)
  expect_error(rowmean_n(5, n = 1), regex = "`data` must be")
  expect_error(rowmean_n(iris[1], n = 1), regex = "two columns")
  expect_error(rowmean_n(iris, n = NULL), regex = "numeric value")
  expect_error(rowmean_n(iris, n = 1:4), regex = "numeric value")
  expect_error(rowmean_n(iris, n = "a"), regex = "numeric value")
  expect_message(rowmean_n(iris[1:3, ], n = 3), regex = "Only numeric")
  expect_silent(rowmean_n(iris[1:3, ], n = 3, verbose = FALSE))
})
