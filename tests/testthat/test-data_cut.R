if (require("testthat")) {
  set.seed(123)
  d <- sample(1:10, size = 500, replace = TRUE)

  test_that("recode median", {
    expect_equal(data_cut(d), ifelse(d >= median(d), 2, 1))
    expect_equal(data_cut(d, lowest = 0), ifelse(d >= median(d), 1, 0))
  })

  test_that("recode mean", {
    expect_equal(data_cut(d, split = "mean"), ifelse(d >= mean(d), 2, 1))
    expect_equal(data_cut(d, split = "mean", lowest = 0), ifelse(d >= mean(d), 1, 0))
  })

  test_that("recode quantile", {
    expect_error(data_cut(d, split = "quantile"))

    q <- quantile(d, probs = c(1 / 3, 2 / 3, 1))
    f <- cut(d, breaks = unique(c(min(d), q, max(d))), include.lowest = TRUE, right = FALSE)
    levels(f) <- 1:nlevels(f)
    expect_equal(data_cut(d, split = "quantile", n_groups = 3), as.numeric(f))
    expect_equal(data_cut(d, split = "quantile", n_groups = 3, lowest = 0), as.numeric(f) - 1)
  })


  set.seed(123)
  d <- sample(1:100, size = 1000, replace = TRUE)

  test_that("recode range", {
    expect_error(data_cut(d, split = "range"))
    d2 <- d
    d2[d <= 20] <- 1
    d2[d > 20 & d <= 40] <- 2
    d2[d > 40 & d <= 60] <- 3
    d2[d > 60 & d <= 80] <- 4
    d2[d > 80] <- 5
    expect_equal(table(data_cut(d, split = "equal_range", range = 20)), table(d2), ignore_attr = TRUE)
    expect_equal(table(data_cut(d, split = "equal_range", range = 20, lowest = 1)), table(d2), ignore_attr = TRUE)

    d2 <- d
    d2[d < 20] <- 0
    d2[d >= 20 & d < 40] <- 1
    d2[d >= 40 & d < 60] <- 2
    d2[d >= 60 & d < 80] <- 3
    d2[d >= 80] <- 4
    expect_equal(table(data_cut(d, split = "equal_range", range = 20, lowest = 0)), table(d2), ignore_attr = TRUE)
  })

  test_that("recode length", {
    expect_error(data_cut(d, split = "equal_length"))
    d2 <- d
    d2[d <= 20] <- 1
    d2[d > 20 & d <= 40] <- 2
    d2[d > 40 & d <= 60] <- 3
    d2[d > 60 & d <= 80] <- 4
    d2[d > 80] <- 5
    expect_equal(table(data_cut(d, split = "equal_length", n_groups = 5)), table(d2), ignore_attr = TRUE)
    expect_equal(table(data_cut(d, split = "equal_length", n_groups = 5, lowest = 1)), table(d2), ignore_attr = TRUE)
  })


  set.seed(123)
  x <- sample(1:10, size = 30, replace = TRUE)
  test_that("recode factor labels", {
    expect_type(data_cut(x, "equal_length", n_groups = 3), "double")
    expect_s3_class(data_cut(x, "equal_length", n_groups = 3, labels = c("low", "mid", "high")), "factor")
    expect_equal(levels(data_cut(x, "equal_length", n_groups = 3, labels = c("low", "mid", "high"))), c("low", "mid", "high"))
    t1 <- table(data_cut(x, "equal_length", n_groups = 3))
    t2 <- table(data_cut(x, "equal_length", n_groups = 3, labels = c("low", "mid", "high")))
    expect_equal(t1, t2, ignore_attr = TRUE)
  })
}
