test_that("distributions", {
  skip_if_not_installed("bayestestR", minimum_version = "0.12.0")
  skip_if_not_installed("parameters")

  set.seed(123)
  x <- bayestestR::distribution_normal(100)

  expect_equal(kurtosis(x)$Kurtosis, -0.1119534, tolerance = 0.01)
  expect_equal(skewness(x)$Skewness, -5.881466e-17, tolerance = 0.01)
  expect_equal(as.numeric(smoothness(x, "diff")), 1.183699, tolerance = 0.01)
  expect_equal(as.numeric(smoothness(x, "cor")), 0.9979799, tolerance = 0.01)
})

