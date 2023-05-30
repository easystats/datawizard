test_char <- data.frame(
  a = c("iso", 2, 5),
  b = c("year", 3, 6),
  c = c(NA, 5, 7),
  stringsAsFactors = FALSE
)

test_num <- data.frame(
  a = c(5, 2, 5),
  b = c(3, 3, 6),
  c = c(NA, 5, 7)
)

test_na <- data.frame(
  a = c(NA, 2, 5),
  b = c(NA, 3, 6),
  c = c(NA, 5, 7)
)

test_that("row_to_colnames works", {
  test <- row_to_colnames(test_char, verbose = FALSE)
  expect_identical(
    colnames(test),
    c("iso", "year", "x1")
  )

  test <- row_to_colnames(test_num, verbose = FALSE)
  expect_identical(
    colnames(test),
    c("5", "3", "x1")
  )

  test <- row_to_colnames(test_na, verbose = FALSE)
  expect_identical(
    colnames(test),
    c("x1", "x2", "x3")
  )
})

test_that("row_to_colnames: check arg 'row'", {
  expect_error(
    row_to_colnames(test_num, row = "hi", verbose = FALSE),
    regexp = "Argument `row`"
  )
  expect_error(
    row_to_colnames(test_num, row = 6),
    regexp = "You used row = 6"
  )
  expect_error(
    row_to_colnames(test_num, row = c(3, 5), verbose = FALSE),
    regexp = "Argument `row`"
  )
  expect_identical(
    row_to_colnames(test_num, verbose = FALSE),
    row_to_colnames(test_num, row = 1, verbose = FALSE)
  )
})

test_that("row_to_colnames: check arg 'na_prefix'", {
  test <- row_to_colnames(test_char, na_prefix = "foo", verbose = FALSE)
  expect_identical(
    colnames(test),
    c("iso", "year", "foo1")
  )

  test <- row_to_colnames(test_num, na_prefix = "foo", verbose = FALSE)
  expect_identical(
    colnames(test),
    c("5", "3", "foo1")
  )
})

#-----------------------------------------------------

foo <- data.frame(
  ARG = c("BRA", "FRA"),
  `1960` = c(1960, 1960),
  `2000` = c(2000, 2000),
  stringsAsFactors = FALSE
)

test_that("colnames_to_row works", {
  test <- colnames_to_row(foo)
  expect_identical(
    colnames(test),
    c("x1", "x2", "x3")
  )
  expect_true(
    all(
      test[1, 1] == "ARG",
      test[1, 2] == "X1960",
      test[1, 3] == "X2000"
    )
  )
  expect_s3_class(test, "data.frame")
})

test_that("colnames_to_row: check arg 'prefix'", {
  test <- colnames_to_row(foo, prefix = "hi")
  expect_identical(
    colnames(test),
    c("hi1", "hi2", "hi3")
  )
  expect_error(
    colnames_to_row(test_num, prefix = 6),
    regexp = "Argument `prefix`"
  )
  expect_error(
    colnames_to_row(test_num, prefix = c("A", "B")),
    regexp = "Argument `prefix`"
  )
  expect_identical(
    colnames_to_row(test),
    colnames_to_row(test, prefix = "x")
  )
})
