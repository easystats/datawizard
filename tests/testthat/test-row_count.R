test_that("row_count", {
  d_mn <- data.frame(
    c1 = c(1, 2, NA, 4),
    c2 = c(NA, 2, NA, 5),
    c3 = c(NA, 4, NA, NA),
    c4 = c(2, 3, 7, 8)
  )
  expect_identical(row_count(d_mn, count = 2), c(1, 2, 0, 0))
  expect_identical(row_count(d_mn, count = NA), c(2, 0, 3, 1))
  d_mn <- data.frame(
    c1 = c("a", "b", NA, "c"),
    c2 = c(NA, "b", NA, "d"),
    c3 = c(NA, 4, NA, NA),
    c4 = c(2, 3, 7, Inf),
    stringsAsFactors = FALSE
  )
  expect_identical(row_count(d_mn, count = "b"), c(0, 2, 0, 0))
  expect_identical(row_count(d_mn, count = Inf), c(0, 0, 0, 1))
})

test_that("row_count, errors or messages", {
  data(iris)
  expect_error(expect_warning(row_count(iris, select = "abc")), regex = "must be a valid")
  expect_error(expect_warning(row_count(iris, select = "abc", count = 3)), regex = "No columns")
  expect_error(row_count(iris[1], count = 3), regex = "with at least")
  expect_error(row_count(iris[-seq_len(nrow(iris)), , drop = FALSE], count = 2), regex = "one row")
})

test_that("row_count, allow_coercion match", {
  d_mn <- data.frame(
    c1 = c("1", "2", NA, "3"),
    c2 = c(NA, "2", NA, "3"),
    c3 = c(NA, 4, NA, NA),
    c4 = c(2, 3, 7, Inf),
    stringsAsFactors = FALSE
  )
  expect_identical(row_count(d_mn, count = 2, allow_coercion = TRUE), c(1, 2, 0, 0))
  expect_identical(row_count(d_mn, count = 2, allow_coercion = FALSE), c(1, 0, 0, 0))
  expect_identical(row_count(d_mn, count = "2", allow_coercion = FALSE), c(0, 2, 0, 0))
  expect_identical(row_count(d_mn, count = factor("2"), allow_coercion = TRUE), c(1, 2, 0, 0))
  expect_error(row_count(d_mn, count = factor("2"), allow_coercion = FALSE), regex = "No column has")

  # mix character / factor
  d_mn <- data.frame(
    c1 = factor(c("1", "2", NA, "3")),
    c2 = c("2", "1", NA, "3"),
    c3 = c(NA, 4, NA, NA),
    c4 = c(2, 3, 7, Inf),
    stringsAsFactors = FALSE
  )
  expect_identical(row_count(d_mn, count = 2, allow_coercion = TRUE), c(2, 1, 0, 0))
  expect_identical(row_count(d_mn, count = 2, allow_coercion = FALSE), c(1, 0, 0, 0))
  expect_identical(row_count(d_mn, count = "2", allow_coercion = FALSE), c(1, 0, 0, 0))
  expect_identical(row_count(d_mn, count = "2", allow_coercion = TRUE), c(2, 1, 0, 0))
  expect_identical(row_count(d_mn, count = factor("2"), allow_coercion = FALSE), c(0, 1, 0, 0))
  expect_identical(row_count(d_mn, count = factor("2"), allow_coercion = TRUE), c(2, 1, 0, 0))
})
