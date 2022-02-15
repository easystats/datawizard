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
    expect_error(data_recode(d, split = "quantile"))

    q <- quantile(d, probs = c(1/3, 2/3, 1))
    f <- cut(d, breaks = unique(c(min(d), q, max(d))), include.lowest = TRUE, right = FALSE)
    levels(f) <- 1:nlevels(f)
    expect_equal(data_recode(d, split = "quantile", n_groups = 3),  as.numeric(f))
    expect_equal(data_recode(d, split = "quantile", n_groups = 3, lowest = 0),  as.numeric(f) - 1)
  })
}
