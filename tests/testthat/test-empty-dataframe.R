test_that("remove empty with character", {
  tmp <- data.frame(
    a = c(1, 2, 3, NA, 5),
    b = c(1, NA, 3, NA, 5),
    c = c(NA, NA, NA, NA, NA),
    d = c(1, NA, 3, NA, 5)
  )

  expect_identical(empty_columns(tmp), c(c = 3L))
  expect_identical(empty_rows(tmp), 4L)

  expect_identical(dim(remove_empty_columns(tmp)), c(5L, 3L))
  expect_identical(dim(remove_empty_rows(tmp)), c(4L, 4L))
  expect_identical(dim(remove_empty(tmp)), c(4L, 3L))

  expect_snapshot(remove_empty_columns(tmp))
  expect_snapshot(remove_empty_rows(tmp))
  expect_snapshot(remove_empty(tmp))
})


test_that("remove empty columns with character", {
  tmp <- data.frame(
    a = c(1, 2, 3, NA, 5),
    b = c("", NA, "", NA, ""),
    c = c(NA, NA, NA, NA, NA),
    d = c(1, NA, 3, NA, 5),
    e = c("", "", "", "", ""),
    stringsAsFactors = FALSE
  )

  expect_identical(empty_columns(tmp), c(b = 2L, c = 3L, e = 5L))
  expect_identical(dim(remove_empty_columns(tmp)), c(5L, 2L))
  expect_identical(dim(remove_empty(tmp)), c(4L, 2L))
})


test_that("remove empty rows with character", {
  tmp <- data.frame(
    a = c(1, "", 3, NA, 5),
    b = c("", NA, "", NA, ""),
    c = c(NA, NA, NA, NA, NA),
    d = c(1, NA, 3, NA, 5),
    e = c("", "", "", "", ""),
    f = factor(c("", "", "", "", "")),
    g = factor(c("", NA, "", NA, "")),
    stringsAsFactors = FALSE
  )

  expect_identical(empty_rows(tmp), c(2L, 4L))
  expect_identical(dim(remove_empty_rows(tmp)), c(3L, 7L))
  expect_identical(dim(remove_empty(tmp)), c(3L, 2L))
})

test_that("empty_columns with only NA characters", {
  tmp <- data.frame(
    var1 = c(1, 1, 1),
    var2 = c(NA_character_, NA_character_, NA_character_)
  )
  expect_identical(empty_columns(tmp), c(var2 = 2L))
})
