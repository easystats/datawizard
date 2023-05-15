test_that("rownames_as_column works", {
  test <- rownames_as_column(mtcars, "new_column")
  expect_true("new_column" %in% names(test))
  expect_identical(test[1, "new_column"], "Mazda RX4")
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

#-------------------------------------------------

test_that("rowid_as_column works", {
  test <- rowid_as_column(mtcars, "new_column")
  expect_true("new_column" %in% names(test))
  expect_identical(test$new_column, 1:32)
})

test_that("rowid_as_column doesn't work if var is not a character", {
  expect_error(rowid_as_column(mtcars, var = 1),
               regexp = "Argument 'var' must be of type character"
  )
  expect_error(rowid_as_column(mtcars, var = TRUE),
               regexp = "Argument 'var' must be of type character"
  )
})

test_that("rowid_as_column uses 'rowid' as default column name", {
  test <- rowid_as_column(mtcars, var = NULL)
  expect_true("rowid" %in% names(test))
})

#-------------------------------------------------

test_that("column_as_rownames works", {
  continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
  test <- data.frame(
    continent = continents,
    some_value = seq(1, 6, by = 1)
  )
  test2 <- column_as_rownames(test, "continent")
  expect_identical(rownames(test2), continents)
  expect_identical(ncol(test2), 1L)

  test3 <- column_as_rownames(test, 1)
  expect_identical(rownames(test3), continents)
  expect_identical(ncol(test3), 1L)
})

test_that("column_as_rownames sanity checks work", {
  continents <- c("Africa", "Asia", "Europe", "North America", "Oceania", "South America")
  test <- data.frame(
    continent = continents,
    some_value = seq(1, 6, by = 1)
  )
  expect_error(column_as_rownames(test, TRUE),
    regexp = "Argument `var`"
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
