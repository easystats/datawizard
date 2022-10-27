test_that("smoothness works", {
  set.seed(123)
  x <- (-10:10)^3 + rnorm(21, 0, 100)
  expect_equal(smoothness(x)[[1]], 0.9030014, tolerance = 0.001)
  expect_equal(smoothness(x, method = "auto")[[1]], 1.750452, tolerance = 0.001)
})

test_that("smoothness works with iterations", {
  skip_if_not_installed("boot")

  set.seed(123)
  x <- (-10:10)^3 + rnorm(21, 0, 100)
  expect_equal(smoothness(x, iterations = 100)[[1]], 0.9030014, tolerance = 0.001)
  expect_equal(smoothness(x, method = "auto", iterations = 100)[[1]], 1.750452, tolerance = 0.001)
})


test_that("smoothness with lag works", {
  set.seed(123)
  x <- (-10:10)^3 + rnorm(21, 0, 100)
  expect_equal(smoothness(x, lag = 0.5)[[1]], 0.5859015, tolerance = 0.001)
  expect_error(smoothness(x, lag = 0), "'lag' cannot be that small.")
})

test_that("smoothness works with data frames", {
  skip_if(grepl("3\\.6\\.3", R.version.string))
  set.seed(123)
  expect_snapshot(smoothness(BOD))
})
