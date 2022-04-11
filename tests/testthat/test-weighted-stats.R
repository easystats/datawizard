test_that("weighted centrality and dispersion measures work as expected", {
  x <- c(3.7, 3.3, 3.5, 2.8)
  wt <- c(5, 5, 4, 1) / 15

  set.seed(123)
  expect_equal(weighted_mean(x, wt), 3.453333, tolerance = 0.001)
  expect_equal(weighted_median(x, wt), 3.5, tolerance = 0.001)
  expect_equal(weighted_sd(x, wt), 0.2852935, tolerance = 0.001)
  expect_equal(weighted_mad(x, wt), 0.29652, tolerance = 0.001)
})
