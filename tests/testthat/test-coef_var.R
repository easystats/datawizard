test_that("coefficient of variation works", {
  expect_equal(coef_var(1:10), 0.5504818826)
  expect_equal(coef_var(1:10, method = "unbiased"), 0.5552700246)
  expect_equal(coef_var(c(1:10, 100), method = "median_mad"), 0.7413)
  expect_equal(coef_var(c(1:10, 100), method = "qcd"), 0.4166666667)
  expect_identical(coef_var(mu = 10, sigma = 20), 2)
  expect_equal(coef_var(mu = 10, sigma = 20, method = "unbiased", n = 30), 2.250614348)
  expect_equal(distribution_coef_var(1:10), 0.5504818826)
})


test_that("coef_var returns NULL if can't compute", {
  expect_warning(
    {
      x <- coef_var(as.Date("2022-10-31"))
    },
    "Can't compute"
  )
  expect_null(x)
})


test_that("coef_var: argument 'remove_na' works", {
  expect_identical(coef_var(c(1:10, NA)), NA_real_)

  expect_identical(
    coef_var(1:10),
    coef_var(c(1:10, NA), remove_na = TRUE)
  )
})

test_that("coef_var: deprecation warning", {
  expect_warning(coef_var(c(1:10, NA), na.rm = TRUE))
})

test_that("coef_var: method 'unbiased' needs argument 'n' when sigma and mu are provided", {
  expect_error(
    coef_var(1:10, method = "unbiased", mu = 10, sigma = 20),
    "A value for `n` must be provided"
  )
})
