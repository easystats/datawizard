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

test_that("rownames_as_column preserves labels", {
  test_data <- mtcars
  test_data <- assign_labels(test_data, select = "hp", variable = "horsepower")

  # ungrouped
  with_id <- rownames_as_column(test_data)
  expect_identical(
    attributes(with_id$hp)$label,
    "horsepower"
  )

  # grouped
  with_id_grouped <- data_group(test_data, "cyl")
  with_id_grouped <- rownames_as_column(with_id_grouped)
  expect_identical(
    attributes(with_id_grouped$hp)$label,
    "horsepower"
  )
})

test_that("rownames_as_column preserves other attribs", {
  test_data <- standardize(mtcars)

  # ungrouped
  with_id <- rownames_as_column(test_data)
  expect_false(is.null(attributes(with_id)$center))

  # grouped
  with_id_grouped <- data_group(test_data, "cyl")
  with_id_grouped <- rownames_as_column(with_id_grouped)
  expect_false(is.null(attributes(with_id_grouped)$center))
})

test_that("rownames_as_column errors if already var of same name", {
  expect_error(
    rownames_as_column(mtcars, "mpg"),
    "already a variable named"
  )
})

#-------------------------------------------------

test_that("rowid_as_column works", {
  test <- rowid_as_column(mtcars, "new_column")
  expect_true("new_column" %in% names(test))
  expect_identical(test$new_column, 1:32)
})

test_that("rowid_as_column works with grouped data", {
  test_data <- data_group(iris, "Species")
  test <- rowid_as_column(test_data)
  expect_identical(test$rowid, rep(1:50, 3))
  expect_true("rowid" %in% names(test))
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

test_that("rowid_as_column preserves labels", {
  test_data <- mtcars
  test_data <- assign_labels(test_data, select = "hp", variable = "horsepower")

  # ungrouped
  with_id <- rowid_as_column(test_data)
  expect_identical(
    attributes(with_id$hp)$label,
    "horsepower"
  )

  # grouped
  with_id_grouped <- data_group(test_data, "cyl")
  with_id_grouped <- rowid_as_column(with_id_grouped)
  expect_identical(
    attributes(with_id_grouped$hp)$label,
    "horsepower"
  )
})

test_that("rowid_as_column preserves other attribs", {
  test_data <- standardize(mtcars)

  # ungrouped
  with_id <- rowid_as_column(test_data)
  expect_false(is.null(attributes(with_id)$center))

  # grouped
  with_id_grouped <- data_group(test_data, "cyl")
  with_id_grouped <- rowid_as_column(with_id_grouped)
  expect_false(is.null(attributes(with_id_grouped)$center))
})

test_that("rowid_as_column has no issue if another variable is called 'var'", {
  foo <- data.frame(
    grp = c("A", "A", "B", "B"),
    var = 1:4,
    stringsAsFactors = FALSE
  )

  out <- data_group(foo, grp)
  out <- rowid_as_column(out)
  expect_named(out, c("rowid", "grp", "var"))
})

test_that("rowid_as_column errors if already var of same name", {
  expect_error(
    rowid_as_column(mtcars, "mpg"),
    "already a variable named"
  )
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

test_that("column_as_rownames preserves labels", {
  test_data <- rownames_as_column(mtcars)
  test_data <- assign_labels(test_data, select = "hp", variable = "horsepower")

  # ungrouped
  with_id <- column_as_rownames(test_data)
  expect_identical(
    attributes(with_id$hp)$label,
    "horsepower"
  )

  # grouped
  with_id_grouped <- data_group(test_data, "cyl")
  with_id_grouped <- column_as_rownames(with_id_grouped)
  expect_identical(
    attributes(with_id_grouped$hp)$label,
    "horsepower"
  )
})


test_that("column_as_rownames preserves other attribs", {
  test_data <- rownames_as_column(standardize(mtcars))

  # ungrouped
  with_id <- column_as_rownames(test_data, "rowname")
  expect_false(is.null(attributes(with_id)$center))

  # grouped
  with_id_grouped <- data_group(test_data, "cyl")
  with_id_grouped <- column_as_rownames(with_id_grouped)
  expect_false(is.null(attributes(with_id_grouped)$center))
})
