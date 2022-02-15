if (require("testthat")) {
  set.seed(123)
  d <- sample(1:10, size = 500, replace = TRUE)

  test_that("recode median", {
    expect_equal(data_recode(d), ifelse(d >= median(d), 2, 1))
    expect_equal(data_recode(d, lowest = 0), ifelse(d >= median(d), 1, 0))
  })

  test_that("recode mean", {
    expect_equal(data_recode(d, split = "mean"), ifelse(d >= mean(d), 2, 1))
    expect_equal(data_recode(d, split = "mean", lowest = 0), ifelse(d >= mean(d), 1, 0))
  })

  test_that("recode quantile", {
    expect_equal(data_recode(d, split = "quantile"), ifelse(d >= mean(d), 2, 1))
    expect_equal(data_recode(d, split = "mean", lowest = 0), ifelse(d >= mean(d), 1, 0))
  })
}
