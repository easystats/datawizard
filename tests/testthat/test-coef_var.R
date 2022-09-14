test_that("coefficient of variation works", {
  expect_equal(coef_var(1:10), 0.5504818826)
  expect_equal(coef_var(1:10, method = "unbiased"), 0.5552700246)
  expect_equal(coef_var(c(1:10, 100), method = "median_mad"), 0.7413)
  expect_equal(coef_var(c(1:10, 100), method = "qcd"), 0.4166666667)
  expect_equal(coef_var(mu = 10, sigma = 20), 2)
  expect_equal(coef_var(mu = 10, sigma = 20, method = "unbiased", n = 30), 2.250614348)
  expect_equal(distribution_coef_var(1:10), 0.5504818826)
})
