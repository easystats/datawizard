test_that("testing Winsorization of factors", {
  expect_identical(winsorize(as.factor(mtcars$am)), as.factor(mtcars$am))
})

test_that("with missing values", {
  skip_if_not_installed("ggplot2")

  expect_snapshot(suppressWarnings(head(winsorize(na.omit(
    ggplot2::msleep$brainwt
  )))))
  expect_length(winsorize(as.factor(ggplot2::msleep$vore)), 83L)
})

test_that("winsorize: missing values do not count in percentile", {
  expect_identical(
    winsorize(c(NA, 1:5, NA, 6:10, NA), threshold = 0.2),
    c(NA, 3L, 3L, 3L, 4L, 5L, NA, 6L, 7L, 8L, 8L, 8L, NA)
  )
})

test_that("winsorize: threshold must be between 0 and 1", {
  expect_error(
    winsorize(sample(1:10, 5), threshold = -0.1),
    regexp = "must be a scalar between 0 and 0.5"
  )
  expect_error(
    winsorize(sample(1:10, 5), threshold = 1.1),
    regexp = "must be a scalar between 0 and 0.5"
  )
  expect_error(
    winsorize(sample(1:10, 5), method = "zscore", threshold = -3),
    regexp = "must be a scalar greater than 0"
  )
  expect_error(
    winsorize(
      sample(1:10, 5),
      method = "zscore",
      threshold = -3,
      robust = TRUE
    ),
    regexp = "must be a scalar greater than 0"
  )
  expect_error(
    winsorize(sample(1:10, 5), method = "raw", threshold = 1.1),
    regexp = "must be of length 2 for lower and upper bound"
  )
})

test_that("winsorize on data.frame", {
  iris2 <- winsorize(iris)
  expect_identical(
    iris2$Sepal.Length,
    winsorize(iris$Sepal.Length)
  )
  expect_identical(
    iris2$Petal.Width,
    winsorize(iris$Petal.Width)
  )
  expect_named(iris2, names(iris))
})

test_that("winsorize: variables that would become constant stay unchanged", {
  dummy <- c(rep(0, 90), rep(1, 10))
  calls <- list(
    list(data = dummy),
    list(data = c(rep(1, 15), rep(2, 70), rep(3, 15)), threshold = 0.2),
    list(data = 1:10, threshold = 0.5),
    list(data = dummy, method = "zscore", robust = TRUE, threshold = 3),
    list(data = 11:20, method = "raw", threshold = c(0, 10)),
    list(data = c(NA, dummy))
  )
  for (args in calls) {
    warnings <- capture_warnings(do.call(winsorize, args))
    expect_identical(suppressWarnings(do.call(winsorize, args)), args$data)
    expect_length(warnings, 1L)
    expect_match(warnings, "would make the variable constant")
  }
})

test_that("winsorize: warning names the columns of a data frame", {
  d <- data.frame(
    score = c(1:90, rep(100, 10)),
    smoker = c(rep(0, 90), rep(1, 10))
  )
  warnings <- capture_warnings(winsorize(d))
  out <- suppressWarnings(winsorize(d))
  expect_identical(out$smoker, d$smoker)
  expect_identical(out$score, winsorize(d$score))
  expect_length(warnings, 1L)
  expect_match(warnings, "`smoker`")
  expect_no_match(warnings, "score")

  d2 <- data.frame(
    a = c(rep(0L, 90), rep(1L, 10)),
    b = c(rep(1, 15), rep(2, 70), rep(3, 15))
  )
  warnings <- capture_warnings(winsorize(d2))
  expect_identical(suppressWarnings(winsorize(d2)), d2)
  expect_length(warnings, 2L)
  expect_match(warnings[1], "`a`")
  expect_match(warnings[2], "`b`")

  d3 <- data.frame(a = 1:5, b = 6:10)
  names(d3) <- c("a", "a")
  out <- winsorize(d3)
  expect_identical(out[[1]], c(2L, 2L, 3L, 4L, 4L))
  expect_identical(out[[2]], c(7L, 7L, 8L, 9L, 9L))
})

test_that("winsorize: no warning when winsorizing does not make a variable constant", {
  out <- expect_silent(winsorize(rep(3, 10)))
  expect_identical(out, rep(3, 10))
  out <- expect_silent(winsorize(c(NA_real_, NA_real_)))
  expect_identical(out, c(NA_real_, NA_real_))
  out <- expect_silent(winsorize(1:10, threshold = 0.2))
  expect_identical(out, c(3L, 3L, 3L, 4L, 5L, 6L, 7L, 8L, 8L, 8L))
  out <- expect_silent(
    winsorize(c(
      rep(1, 5),
      rep(2, 20),
      rep(3, 50),
      rep(4, 20),
      rep(5, 5)
    ))
  )
  expect_identical(out, c(rep(2, 25), rep(3, 50), rep(4, 25)))
  out <- expect_silent(winsorize(1:10, method = "raw", threshold = c(3, 8)))
  expect_identical(out, c(3, 3, 3, 4, 5, 6, 7, 8, 8, 8))
  dummy <- c(rep(0, 90), rep(1, 10))
  out <- expect_silent(winsorize(dummy, method = "zscore", threshold = 3))
  expect_identical(out, dummy)
})

test_that("winsorize: verbose = FALSE silences the warning", {
  dummy <- c(rep(0, 90), rep(1, 10))
  out <- expect_silent(winsorize(dummy, verbose = FALSE))
  expect_identical(out, dummy)
  d <- data.frame(score = c(1:90, rep(100, 10)), smoker = dummy)
  out <- expect_silent(winsorize(d, verbose = FALSE))
  expect_identical(out$smoker, dummy)
  expect_identical(out$score, suppressWarnings(winsorize(d))$score)
})
