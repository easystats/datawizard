test_that("remove empty with character", {
  tmp <- data.frame(
    a = c(1, 2, 3, NA, 5),
    b = c(1, NA, 3, NA, 5),
    c = c(NA, NA, NA, NA, NA),
    d = c(1, NA, 3, NA, 5)
  )

  expect_equal(empty_columns(tmp), c(c = 3L))
  expect_equal(empty_rows(tmp), 4)

  expect_equal(dim(remove_empty_columns(tmp)), c(5L, 3L))
  expect_equal(dim(remove_empty_rows(tmp)), c(4L, 4L))
  expect_equal(dim(remove_empty(tmp)), c(4L, 3L))

  expect_snapshot(remove_empty_columns(tmp))
  expect_snapshot(remove_empty_rows(tmp))
  expect_snapshot(remove_empty(tmp))
})


test_that("remove empty with character", {
  tmp <- data.frame(
    a = c(1, 2, 3, NA, 5),
    b = c("", NA, "", NA, ""),
    c = c(NA, NA, NA, NA, NA),
    d = c(1, NA, 3, NA, 5),
    e = c("", "", "", "", ""),
    f = factor(c("", "", "", "", "")),
    stringsAsFactors = FALSE
  )

  expect_equal(empty_columns(tmp), c(b = 2L, c = 3L, e = 5L))
  expect_equal(dim(remove_empty_columns(tmp)), c(5L, 3L))
  expect_equal(dim(remove_empty(tmp)), c(5L, 3L))

  expect_equal(empty_columns(tmp, include_empty_string = FALSE), c(c = 3L))
  expect_equal(dim(remove_empty_columns(tmp, include_empty_string = FALSE)), c(5L, 5L))
})
