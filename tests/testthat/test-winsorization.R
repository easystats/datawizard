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


# factors -------------------------------------------

test_that("winsorize factors, include_factors", {
  x <- factor(c(rep("a", 6), rep("b", 2), "c", "d", NA))
  expect_identical(winsorize(x), x)
  expect_identical(
    winsorize(x, include_factors = TRUE),
    factor(
      c(rep("a", 6), rep("b", 2), ".other", ".other", NA),
      levels = c("a", "b", ".other")
    )
  )
  # only c is rare
  x <- factor(c(rep("a", 5), rep("b", 4), "c"))
  expect_identical(winsorize(x, include_factors = TRUE), x)
  # no non-missing values
  x <- factor(c(NA, NA), levels = c("a", "b"))
  expect_identical(winsorize(x, include_factors = TRUE), x)
})

test_that("winsorize factors, unused and existing .other levels", {
  x <- factor(
    c(rep("a", 6), rep("b", 2), "c", "d"),
    levels = c("a", "b", "c", "d", "e")
  )
  expect_identical(
    winsorize(x, include_factors = TRUE),
    factor(
      c(rep("a", 6), rep("b", 2), ".other", ".other"),
      levels = c("a", "b", "e", ".other")
    )
  )

  x <- factor(
    c(rep("a", 5), rep(".other", 3), "b", "c"),
    levels = c(".other", "a", "b", "c")
  )
  expect_identical(
    winsorize(x, include_factors = TRUE),
    factor(c(rep("a", 5), rep(".other", 5)), levels = c("a", ".other"))
  )
})

test_that("winsorize factors, NA level and missing values", {
  # value 11 is at the NA level and value 12 has an NA code
  x <- addNA(factor(c(rep("a", 6), rep("b", 2), "c", "d", NA, NA)))
  is.na(x) <- 12
  out <- winsorize(x, include_factors = TRUE)
  expect_identical(levels(out), c("a", "b", NA, ".other"))
  expect_identical(is.na(out), is.na(x))
  expect_identical(
    as.character(out),
    c(rep("a", 6), rep("b", 2), ".other", ".other", NA, NA)
  )
})

test_that("winsorize factors, class and attributes", {
  x <- factor(
    c("low", rep("mid", 8), "high"),
    levels = c("low", "mid", "high"),
    ordered = TRUE
  )
  attr(x, "label") <- "My variable"
  expected <- factor(
    c(".other", rep("mid", 8), ".other"),
    levels = c("mid", ".other"),
    ordered = TRUE
  )
  attr(expected, "label") <- "My variable"
  expect_identical(winsorize(x, include_factors = TRUE), expected)

  # contrasts matrix and value labels of the old levels are dropped
  x <- factor(c(rep("a", 6), rep("b", 2), "c", "d"))
  contrasts(x) <- stats::contr.sum(4)
  attr(x, "labels") <- c(a = 1, b = 2, c = 3, d = 4)
  out <- winsorize(x, include_factors = TRUE)
  expect_null(attr(out, "contrasts"))
  expect_null(attr(out, "labels"))
  attr(x, "contrasts") <- "contr.sum"
  out <- winsorize(x, include_factors = TRUE)
  expect_identical(attr(out, "contrasts"), "contr.sum")
})

test_that("winsorize factors in data frames", {
  d <- data.frame(num = 1:10, fac = factor(c(rep("a", 8), "b", "c")))
  out <- winsorize(d, include_factors = TRUE)
  expect_identical(out$fac, winsorize(d$fac, include_factors = TRUE))
  expect_identical(levels(out$fac), c("a", ".other"))
})

test_that("winsorize factors, errors", {
  x <- factor(c(rep("a", 7), "b", "c", "d"))
  expect_error(
    winsorize(x, include_factors = TRUE, method = "zscore"),
    regexp = "method = \"percentile\"",
    fixed = TRUE
  )
  expect_error(
    winsorize(x, include_factors = TRUE, threshold = 0.6),
    regexp = "`threshold` for winsorization must be a scalar between 0 and 0.5.",
    fixed = TRUE
  )
  expect_error(
    winsorize(x, include_factors = NA),
    regexp = "`include_factors` must be `TRUE` or `FALSE`.",
    fixed = TRUE
  )
})
