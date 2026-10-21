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
  # oracle (closed-form): 10 non-missing values, shares a = 0.6, b = 0.2,
  # c = 0.1, d = 0.1; c and d are below 0.2, b equals 0.2 and is kept
  expect_identical(
    winsorize(x, include_factors = TRUE),
    factor(
      c(rep("a", 6), rep("b", 2), ".other", ".other", NA),
      levels = c("a", "b", ".other")
    )
  )
  # oracle (closed-form): with threshold 0.25, b (0.2) is also below
  expect_identical(
    winsorize(x, threshold = 0.25, include_factors = TRUE),
    factor(c(rep("a", 6), rep(".other", 4), NA), levels = c("a", ".other"))
  )

  # oracle (closed-form): b, c, d each have share 1 / 10 = 0.1
  x <- factor(c(rep("a", 7), "b", "c", "d"))
  expect_identical(
    winsorize(x, include_factors = TRUE),
    factor(c(rep("a", 7), rep(".other", 3)), levels = c("a", ".other"))
  )
})

test_that("winsorize factors, ordered and unused levels", {
  x <- factor(
    c("low", rep("mid", 8), "high"),
    levels = c("low", "mid", "high"),
    ordered = TRUE
  )
  # oracle (closed-form): low and high each have share 1 / 10 = 0.1
  expect_identical(
    winsorize(x, include_factors = TRUE),
    factor(
      c(".other", rep("mid", 8), ".other"),
      levels = c("mid", ".other"),
      ordered = TRUE
    )
  )

  # oracle (closed-form): c and d have share 0.1; the unused level e is not
  # counted and stays a level
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

  # oracle (closed-form): only b (0.1) is below 0.2, the unused z is not
  # counted, so nothing is merged
  x <- factor(c(rep("a", 9), "b"), levels = c("a", "b", "z"))
  expect_identical(winsorize(x, include_factors = TRUE), x)
})

test_that("winsorize factors, existing .other level", {
  # oracle (closed-form): b and c have share 0.1 and are merged, and the
  # values of the existing .other level join them
  x <- factor(
    c(rep("a", 5), rep(".other", 3), "b", "c"),
    levels = c(".other", "a", "b", "c")
  )
  expect_identical(
    winsorize(x, include_factors = TRUE),
    factor(c(rep("a", 5), rep(".other", 5)), levels = c("a", ".other"))
  )

  # oracle (closed-form): .other is never rare, and b (0.1) is the only
  # level below 0.2, so nothing is merged
  x <- factor(c(rep("a", 6), rep(".other", 3), "b"))
  expect_identical(winsorize(x, include_factors = TRUE), x)
})

test_that("winsorize factors, nothing to merge", {
  # oracle (closed-form): c (0.1) is the only level below 0.2
  x <- factor(c(rep("a", 5), rep("b", 4), "c"))
  expect_identical(winsorize(x, include_factors = TRUE), x)

  # no non-missing values
  x <- factor(c(NA, NA), levels = c("a", "b"))
  expect_identical(winsorize(x, include_factors = TRUE), x)
})

test_that("winsorize factors, NA level, class, and attributes", {
  x <- factor(
    c(rep("a", 6), rep("b", 2), "c", "d", NA),
    levels = c(NA, "a", "b", "c", "d"),
    exclude = NULL
  )
  # oracle (closed-form): the NA level holds a missing value, so the shares
  # use 10 values: a = 0.6, b = 0.2, c = 0.1, d = 0.1; c and d are merged, and
  # the NA level stays first. Counting the NA level (1 / 11) would make it rare
  # and would make b rare too (2 / 11 < 0.2).
  expect_identical(
    winsorize(x, include_factors = TRUE),
    factor(
      c(rep("a", 6), rep("b", 2), ".other", ".other", NA),
      levels = c(NA, "a", "b", ".other"),
      exclude = NULL
    )
  )

  # oracle (closed-form): same shares, with the NA level between the merged
  # levels c and d; it keeps its place among the kept levels
  x <- factor(
    c(rep("a", 6), rep("b", 2), "c", "d", NA),
    levels = c("a", "c", NA, "d", "b"),
    exclude = NULL
  )
  expect_identical(
    winsorize(x, include_factors = TRUE),
    factor(
      c(rep("a", 6), rep("b", 2), ".other", ".other", NA),
      levels = c("a", NA, "b", ".other"),
      exclude = NULL
    )
  )

  # oracle (closed-form): c (0.1) is the only level below 0.2 and the NA
  # level is never rare, so nothing is merged
  x <- addNA(factor(c(rep("a", 5), rep("b", 4), "c", NA)))
  expect_identical(winsorize(x, include_factors = TRUE), x)

  # oracle (closed-form): a level named "NA" is an ordinary level; "NA" and
  # b each have share 0.1
  x <- factor(c(rep("a", 8), "NA", "b"))
  expect_identical(
    winsorize(x, include_factors = TRUE),
    factor(c(rep("a", 8), ".other", ".other"), levels = c("a", ".other"))
  )

  # oracle (closed-form): b and c each have share 0.1; the extra class and
  # the label are kept
  x <- factor(c(rep("a", 8), "b", "c"))
  class(x) <- c("myfac", "factor")
  attr(x, "label") <- "My variable"
  expected <- factor(
    c(rep("a", 8), ".other", ".other"),
    levels = c("a", ".other")
  )
  class(expected) <- c("myfac", "factor")
  attr(expected, "label") <- "My variable"
  expect_identical(winsorize(x, include_factors = TRUE), expected)

  # oracle (closed-form): value 11 is at the NA level and value 12 has an NA
  # code; 10 values remain, c and d (0.1) are merged, and both missing values
  # keep their kind
  x <- addNA(factor(c(rep("a", 6), rep("b", 2), "c", "d", NA, NA)))
  is.na(x) <- 12
  out <- winsorize(x, include_factors = TRUE)
  expect_identical(levels(out), c("a", "b", NA, ".other"))
  expect_identical(is.na(out), is.na(x))
  expect_identical(
    as.character(out),
    c(rep("a", 6), rep("b", 2), ".other", ".other", NA, NA)
  )

  # contrasts made for the old levels are dropped
  x <- factor(c(rep("a", 6), rep("b", 2), "c", "d"))
  contrasts(x) <- stats::contr.sum(4)
  out <- winsorize(x, include_factors = TRUE)
  expect_null(attr(out, "contrasts"))
  expect_identical(levels(out), c("a", "b", ".other"))
})

test_that("winsorize factors, include_factors must be named", {
  x <- factor(c(rep("a", 7), "b", "c", "d"))
  # a fourth unnamed argument goes to `...` and does not merge levels
  expect_identical(winsorize(x, 0.2, "percentile", TRUE), x)

  d <- data.frame(num = 1:10, fac = x)
  expect_identical(
    winsorize(d, 0.2, "percentile", FALSE, TRUE, TRUE),
    winsorize(d)
  )
})

test_that("winsorize factors, include_factors = FALSE", {
  x <- factor(c(rep("a", 7), "b", "c", "d"))
  expect_identical(winsorize(x), x)
  expect_identical(winsorize(x, include_factors = FALSE), x)

  # character and logical vectors are not changed
  x <- c(rep("a", 7), "b", "c", "d")
  expect_identical(winsorize(x, include_factors = TRUE), x)
  x <- c(rep(TRUE, 7), FALSE, FALSE, NA)
  expect_identical(winsorize(x, include_factors = TRUE), x)
})

test_that("winsorize factors in data frames", {
  d <- data.frame(
    num = 1:10,
    chr = c(rep("a", 8), "b", "c"),
    fac = factor(c(rep("a", 8), "b", "c"))
  )
  out <- winsorize(d, include_factors = TRUE)
  # oracle (closed-form): b and c each have share 0.1
  expect_identical(
    out$fac,
    factor(c(rep("a", 8), ".other", ".other"), levels = c("a", ".other"))
  )
  expect_identical(out$fac, winsorize(d$fac, include_factors = TRUE))
  expect_identical(out$chr, d$chr)
  expect_identical(out$num, winsorize(d$num))

  out <- winsorize(d, threshold = 0.05, include_factors = TRUE)
  expect_identical(
    out$fac,
    winsorize(d$fac, threshold = 0.05, include_factors = TRUE)
  )
  expect_identical(out$fac, d$fac)

  expect_identical(winsorize(d)$fac, d$fac)
})

test_that("winsorize factors, errors", {
  x <- factor(c(rep("a", 7), "b", "c", "d"))

  # method is checked before threshold
  expect_error(
    winsorize(x, include_factors = TRUE, method = "zscore"),
    regexp = "method = \"percentile\"",
    fixed = TRUE
  )
  expect_error(
    winsorize(x, include_factors = TRUE, method = "raw", threshold = c(1, 2)),
    regexp = "method = \"percentile\"",
    fixed = TRUE
  )

  msg <- "`threshold` for winsorization must be a scalar between 0 and 0.5."
  for (th in list(-0.1, 0.6, c(0.1, 0.2), NA, "a")) {
    expect_error(
      winsorize(x, include_factors = TRUE, threshold = th),
      regexp = msg,
      fixed = TRUE
    )
  }

  d <- data.frame(num = 1:10, fac = x)
  for (inc in list(NA, "yes", c(TRUE, TRUE))) {
    expect_error(
      winsorize(x, include_factors = inc),
      regexp = "`include_factors` must be `TRUE` or `FALSE`.",
      fixed = TRUE
    )
    expect_error(
      winsorize(d, include_factors = inc),
      regexp = "`include_factors` must be `TRUE` or `FALSE`.",
      fixed = TRUE
    )
  }
})
