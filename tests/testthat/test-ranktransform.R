test_that("ranktransform works with NAs", {
  x <- c(NA_real_, NA_real_)
  expect_identical(ranktransform(x), x)
})

test_that("ranktransform works with factors", {
  x <- factor(c("apple", "bear", "banana", "dear"))
  expect_identical(ranktransform(x), x)
})

test_that("ranktransform works with unique value vectors", {
  x <- c(1L, 1L, 1L)

  expect_identical(suppressWarnings(ranktransform(x)), x)

  expect_warning(
    ranktransform(x),
    "Variable `x` contains only one unique value and will not be normalized."
  )
})

test_that("ranktransform works with two unique value vectors", {
  x <- c(1L, 1L, 1L, 2L, 2L, 2L)

  expect_identical(suppressWarnings(ranktransform(x)), c(2, 2, 2, 5, 5, 5))

  expect_warning(
    ranktransform(x),
    "Consider converting it"
  )
})

test_that("signed rank works as expected", {
  x <- c(-1, 2, -3, 4)

  sr <- ranktransform(x, sign = TRUE)
  r <- ranktransform(x, sign = FALSE)

  expect_identical(sr, x) # unchanged
  expect_identical(r, c(2, 3, 1, 4))

  x <- c(1, -2, -2, 4, 0, 3, -14, 0)
  expect_warning(ranktransform(x, sign = TRUE))
  expect_true(all(is.na(suppressWarnings(
    ranktransform(x, sign = TRUE)[c(5, 8)]
  ))))
})

test_that("argument 'zeros' works", {
  x <- c(-1, 0, 2, -3, 4)
  expect_warning(
    ranktransform(x, sign = TRUE),
    "cannot be sign-rank"
  )
  expect_identical(
    ranktransform(x, sign = TRUE, zeros = "signrank"),
    c(-2, 0, 3, -4, 5)
  )
  expect_error(
    ranktransform(x, sign = TRUE, zeros = "foo"),
    "should be one of"
  )
})

test_that("ranktransform works with data frames", {
  set.seed(123)
  expect_snapshot(ranktransform(BOD))
})


# with grouped data -------------------------------------------

test_that("ranktransform works with data frames (grouped data)", {
  skip_if_not_installed("poorman")

  set.seed(123)
  value1 <- sample.int(20, 9, replace = TRUE)
  set.seed(456)
  value2 <- sample.int(20, 9, replace = TRUE)

  test_df <- data.frame(
    id = rep(c("A", "B", "C"), each = 3),
    value1 = value1,
    value2 = value2,
    stringsAsFactors = FALSE
  )

  # nolint start: nested_pipe_linter
  expect_identical(
    test_df %>%
      poorman::group_by(id) %>%
      ranktransform(exclude = "id") %>%
      poorman::ungroup(),
    data.frame(
      id = rep(c("A", "B", "C"), each = 3),
      value1 = c(2, 3, 1, 1, 2, 3, 2, 1, 3),
      value2 = c(3, 2, 1, 1, 3, 2, 2, 3, 1),
      stringsAsFactors = FALSE
    )
  )
  # nolint end
})


test_that("ranktransform works with data frames containing NAs (grouped data)", {
  skip_if_not_installed("poorman")

  set.seed(789)
  value1 <- sample(c(1:15, NA), 9, replace = TRUE)
  set.seed(10)
  value2 <- sample(c(1:15, NA), 9, replace = TRUE)

  test_df <- data.frame(
    id = rep(c("A", "B", "C"), each = 3),
    value1 = value1,
    value2 = value2,
    stringsAsFactors = FALSE
  )

  # nolint start: nested_pipe_linter
  expect_identical(
    test_df %>%
      poorman::group_by(id) %>%
      ranktransform(exclude = "id") %>%
      poorman::ungroup(),
    data.frame(
      id = rep(c("A", "B", "C"), each = 3),
      value1 = c(2, NA, 1, 1, 3, 2, 2, NA, 1),
      value2 = c(3, 1, 2, NA, 2, 1, 3, 1, 2),
      stringsAsFactors = FALSE
    )
  )
  # nolint end
})

# select helpers ------------------------------
test_that("ranktransform regex", {
  expect_identical(
    ranktransform(mtcars, select = "pg", regex = TRUE),
    ranktransform(mtcars, select = "mpg")
  )
})
