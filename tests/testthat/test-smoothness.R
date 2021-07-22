test_that("smoothness", {
  set.seed(123)
  x <- (-10:10)^3 + rnorm(21, 0, 100)
  expect_equal(smoothness(x)[[1]], 0.9030014, tolerance = 0.001)
  expect_equal(smoothness(x, method = "auto")[[1]], 1.750452, tolerance = 0.001)
})
