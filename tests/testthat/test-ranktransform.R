test_that("ranktransform works with NAs", {
  x <- c(NA_real_, NA_real_)
  expect_equal(ranktransform(x), x)
})

test_that("ranktransform works with factors", {
  x <- factor(c("apple", "bear", "banana", "dear"))
  expect_equal(ranktransform(x), x)
})

test_that("ranktransform works with unique value vectors", {
  x <- c(1L, 1L, 1L)

  expect_equal(suppressWarnings(ranktransform(x)), x)

  expect_warning(
    ranktransform(x),
    "Variable `x` contains only one unique value and will not be normalized."
  )
})

test_that("ranktransform works with two unique value vectors", {
  x <- c(1L, 1L, 1L, 2L, 2L, 2L)

  expect_equal(suppressWarnings(ranktransform(x)), c(2, 2, 2, 5, 5, 5))

  expect_warning(
    ranktransform(x),
    "Variable `x` contains only two different values. Consider converting it to a factor."
  )
})

test_that("signed rank works as expected", {
  x <- c(-1, 2, -3, 4)

  sr <- ranktransform(x, sign = TRUE)
  r <- ranktransform(x, sign = FALSE)

  expect_equal(sr, x) # unchanged
  expect_equal(r, c(2, 3, 1, 4))

  x <- c(1, -2, -2, 4, 0, 3, -14, 0)
  expect_warning(ranktransform(x, sign = TRUE))
  expect_true(all(is.na(suppressWarnings(ranktransform(x, sign = TRUE)[c(5, 8)]))))
})

test_that("ranktransform works with dataframes", {
  set.seed(123)
  expect_snapshot(ranktransform(BOD))
})
