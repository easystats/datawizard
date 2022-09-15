test_that("rownames_as_column works", {
  test <- rownames_as_column(mtcars, "new_column")
  expect_true("new_column" %in% names(test))
  expect_true(test[1, "new_column"] == "Mazda RX4")
})

test_that("rownames_as_column doesn't work if var is not a character", {
  expect_error(rownames_as_column(mtcars, var = 1),
    regexp = "Argument 'var' must be of type character"
  )
  expect_error(rownames_as_column(mtcars, var = TRUE),
    regexp = "Argument 'var' must be of type character"
  )
})

test_that("rownames_as_column uses 'rowname' as default column name", {
  test <- rownames_as_column(mtcars, var = NULL)
  expect_true("rowname" %in% names(test))
})

#-------------------------------------------------s

test_that("column_as_rownames works", {
  continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
  test <- data.frame(
    continent = continents,
    some_value = seq(1, 6, by = 1)
  )
  test2 <- column_as_rownames(test, "continent")
  expect_identical(rownames(test2), continents)
  expect_true(ncol(test2) == 1)

  test3 <- column_as_rownames(test, 1)
  expect_identical(rownames(test3), continents)
  expect_true(ncol(test3) == 1)
})

test_that("column_as_rownames sanity checks work", {
  continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
  test <- data.frame(
    continent = continents,
    some_value = seq(1, 6, by = 1)
  )
  expect_error(column_as_rownames(test, TRUE),
    regexp = "Argument 'var' must be of type character or numeric"
  )
  expect_error(column_as_rownames(test, "foo"),
    regexp = "not in the data frame"
  )
  expect_error(column_as_rownames(test, 0),
    regexp = "does not exist"
  )
  expect_error(column_as_rownames(test, 3),
    regexp = "does not exist"
  )
})

test_that("rownames_as_column and column_as_rownames cancel each other", {
  test <- rownames_as_column(mtcars)
  test2 <- column_as_rownames(test)
  expect_identical(test2, mtcars)
})


#-----------------------------------------------------

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
  expect_equal(
    colnames(test),
    c("iso", "year", "x1")
  )

  test <- row_to_colnames(test_num, verbose = FALSE)
  expect_equal(
    colnames(test),
    c("5", "3", "x1")
  )

  test <- row_to_colnames(test_na, verbose = FALSE)
  expect_equal(
    colnames(test),
    c("x1", "x2", "x3")
  )
})

test_that("row_to_colnames: check arg 'row'", {
  expect_error(
    row_to_colnames(test_num, row = "hi", verbose = FALSE),
    regexp = "Argument 'row' must be of type numeric"
  )
  expect_error(
    row_to_colnames(test_num, row = 6),
    regexp = "You used row = 6"
  )
  expect_error(
    row_to_colnames(test_num, row = c(3, 5), verbose = FALSE),
    regexp = "Argument 'row' must be of length 1"
  )
  expect_equal(
    row_to_colnames(test_num, verbose = FALSE),
    row_to_colnames(test_num, row = 1, verbose = FALSE)
  )
})

test_that("row_to_colnames: check arg 'na_prefix'", {
  test <- row_to_colnames(test_char, na_prefix = "foo", verbose = FALSE)
  expect_equal(
    colnames(test),
    c("iso", "year", "foo1")
  )

  test <- row_to_colnames(test_num, na_prefix = "foo", verbose = FALSE)
  expect_equal(
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
  expect_equal(
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
  expect_equal(
    colnames(test),
    c("hi1", "hi2", "hi3")
  )
  expect_error(
    colnames_to_row(test_num, prefix = 6),
    regexp = "Argument 'prefix' must be of type character"
  )
  expect_error(
    colnames_to_row(test_num, prefix = c("A", "B")),
    regexp = "Argument 'prefix' must be of length 1"
  )
  expect_equal(
    colnames_to_row(test),
    colnames_to_row(test, prefix = "x")
  )
})
