test_that("weighted centrality and dispersion measures work as expected", {
  x <- c(3.7, 3.3, 3.5, 2.8)
  wt <- c(5, 5, 4, 1) / 15

  set.seed(123)
  expect_equal(weighted_mean(x, wt), 3.453333, tolerance = 0.001)
  expect_equal(weighted_median(x, wt), 3.5, tolerance = 0.001)
  expect_equal(weighted_sd(x, wt), 0.2852935, tolerance = 0.001)
  expect_equal(weighted_mad(x, wt), 0.29652, tolerance = 0.001)
})

test_that("weighted centrality and dispersion measures work with NA", {
  x <- c(3.7, 3.3, NA, 3.5, 2.8, 5.5)
  wt <- c(5, 5, 4, NA, 1, 7) / 15

  set.seed(123)
  expect_equal(weighted_mean(x, wt), 4.238889, tolerance = 0.001)
  expect_equal(weighted_median(x, wt), 3.7, tolerance = 0.001)
  expect_equal(weighted_sd(x, wt), 1.237671, tolerance = 0.001)
  expect_equal(weighted_mad(x, wt), 0.59304, tolerance = 0.001)
})

test_that("weighted centrality and dispersion measures work with NA when not removed", {
  x <- c(3.7, 3.3, NA, 3.5, 2.8, 5.5)
  wt <- c(5, 5, 4, NA, 1, 7) / 15

  set.seed(123)
  expect_identical(weighted_mean(x, wt, remove_na = FALSE), NA_real_)
  expect_identical(weighted_median(x, wt, remove_na = FALSE), NA_real_)
  expect_identical(weighted_sd(x, wt, remove_na = FALSE), NA_real_)
  expect_identical(weighted_mad(x, wt, remove_na = FALSE), NA_real_)
})

test_that("weighted centrality and dispersion measures work with Inf", {
  x <- c(3.7, 3.3, NA, 3.5, 2.8, 5.5, Inf, 4)
  wt <- c(5, 5, 4, NA, 1, 7, 3, Inf) / 15

  set.seed(123)
  expect_equal(weighted_mean(x, wt), 4.238889, tolerance = 0.001)
  expect_equal(weighted_median(x, wt), 3.7, tolerance = 0.001)
  expect_equal(weighted_sd(x, wt), 1.237671, tolerance = 0.001)
  expect_equal(weighted_mad(x, wt), 0.59304, tolerance = 0.001)
})
