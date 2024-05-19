test_that("extract_column_names works as expected", {
  expect_identical(
    extract_column_names(iris, starts_with("Sepal")),
    c("Sepal.Length", "Sepal.Width")
  )

  expect_identical(
    extract_column_names(iris, starts_with("Sepal", "Petal")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_identical(
    extract_column_names(iris, ends_with("Width")),
    c("Sepal.Width", "Petal.Width")
  )

  expect_identical(
    extract_column_names(iris, ends_with("Length", "Width")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_identical(
    extract_column_names(iris, regex("\\.")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_identical(
    extract_column_names(iris, c("Petal.Width", "Sepal.Length")),
    c("Petal.Width", "Sepal.Length")
  )

  expect_identical(
    extract_column_names(iris, contains("Wid")),
    c("Sepal.Width", "Petal.Width")
  )

  expect_identical(
    extract_column_names(iris, contains("en", "idt")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_identical(
    extract_column_names(mtcars, c("am", "gear", "cyl")),
    c("am", "gear", "cyl")
  )

  expect_identical(
    extract_column_names(mtcars, c("vam", "gear", "cyl")),
    c("gear", "cyl")
  )

  expect_warning(expect_null(extract_column_names(mtcars, ends_with("abc"))))

  expect_identical(
    extract_column_names(mtcars, regex("rb$")),
    "carb"
  )

  expect_identical(
    extract_column_names(mtcars, regex("^c")),
    c("cyl", "carb")
  )

  expect_warning(expect_null(extract_column_names(mtcars, "^c")))

  expect_identical(
    extract_column_names(mtcars, regex("^C"), ignore_case = TRUE),
    c("cyl", "carb")
  )

  expect_identical(
    extract_column_names(iris, "Width$", regex = TRUE),
    c("Sepal.Width", "Petal.Width")
  )
})


test_that("extract_column_names from other functions", {
  test_fun1 <- function(data, i) {
    extract_column_names(data, select = i)
  }
  expect_identical(
    test_fun1(iris, c("Sepal.Length", "Sepal.Width")),
    c("Sepal.Length", "Sepal.Width")
  )

  expect_identical(
    test_fun1(iris, starts_with("Sep")),
    c("Sepal.Length", "Sepal.Width")
  )

  test_fun1a <- function(data, i) {
    extract_column_names(data, select = i, regex = TRUE)
  }
  expect_identical(
    test_fun1a(iris, "Sep"),
    c("Sepal.Length", "Sepal.Width")
  )

  test_fun1b <- function(data, i) {
    extract_column_names(data, select = i, regex = TRUE)
  }
  expect_identical(
    test_fun1b(iris, "Width$"),
    c("Sepal.Width", "Petal.Width")
  )

  test_fun2 <- function(data) {
    extract_column_names(data, select = starts_with("Sep"))
  }
  expect_identical(
    test_fun2(iris),
    c("Sepal.Length", "Sepal.Width")
  )

  test_fun3 <- function(data) {
    i <- "Sep"
    extract_column_names(data, select = starts_with(i))
  }
  expect_identical(
    test_fun3(iris),
    c("Sepal.Length", "Sepal.Width")
  )
})

test_that("extract_column_names regex", {
  expect_identical(
    extract_column_names(mtcars, select = "pg", regex = TRUE),
    extract_column_names(mtcars, select = "mpg")
  )
})

test_that("extract_column_names works correctly with minus sign", {
  expect_identical(
    extract_column_names(iris, -"Sepal.Length"),
    c("Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_identical(
    extract_column_names(iris, -c("Sepal.Length", "Petal.Width")),
    c("Sepal.Width", "Petal.Length", "Species")
  )

  expect_identical(
    extract_column_names(iris, -1),
    c("Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_error(
    extract_column_names(iris, -1:2),
    regexp = "can't mix negative"
  )

  expect_identical(
    extract_column_names(iris, -(1:2)),
    c("Petal.Length", "Petal.Width", "Species")
  )

  expect_identical(
    extract_column_names(iris, -c(1, 3)),
    c("Sepal.Width", "Petal.Width", "Species")
  )

  expect_identical(
    extract_column_names(iris, -starts_with("Sepal", "Petal")),
    "Species"
  )

  expect_identical(
    extract_column_names(iris, -ends_with("Length", "Width")),
    "Species"
  )

  expect_identical(
    extract_column_names(iris, -contains("en", "idt")),
    "Species"
  )

  expect_identical(
    extract_column_names(iris, -c("Sepal.Length", "Petal.Width"), exclude = "Species"),
    c("Sepal.Width", "Petal.Length")
  )
})

test_that("extract_column_names with square brackets", {
  expect_identical(
    extract_column_names(mtcars, select = names(mtcars)[-1]),
    extract_column_names(mtcars, select = 2:11)
  )
})
