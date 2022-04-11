test_that("find_columns works as expected", {
  expect_equal(
    find_columns(iris, starts_with("Sepal")),
    c("Sepal.Length", "Sepal.Width")
  )

  expect_equal(
    find_columns(iris, ends_with("Width")),
    c("Sepal.Width", "Petal.Width")
  )

  expect_equal(
    find_columns(iris, regex("\\.")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_equal(
    find_columns(iris, c("Petal.Width", "Sepal.Length")),
    c("Petal.Width", "Sepal.Length")
  )

  expect_equal(
    find_columns(iris, contains("Wid")),
    c("Sepal.Width", "Petal.Width")
  )

  expect_equal(
    find_columns(mtcars, c("am", "gear", "cyl")),
    c("am", "gear", "cyl")
  )

  expect_equal(
    find_columns(mtcars, c("vam", "gear", "cyl")),
    c("gear", "cyl")
  )

  expect_equal(
    find_columns(iris, Spec),
    "Species"
  )

  expect_warning(expect_null(find_columns(mtcars, ends_with("abc"))))

  expect_equal(
    find_columns(mtcars, regex("rb$")),
    "carb"
  )

  expect_equal(
    find_columns(mtcars, regex("^c")),
    c("cyl", "carb")
  )

  expect_warning(expect_null(find_columns(mtcars, "^c")))

  expect_equal(
    find_columns(mtcars, regex("^C"), ignore_case = TRUE),
    c("cyl", "carb")
  )

  expect_equal(
    find_columns(iris, "Width$", regex = TRUE),
    c("Sepal.Width", "Petal.Width")
  )
})


test_that("find_columns from other functions", {
  test_fun1 <- function(data, i) {
    find_columns(data, select = i)
  }
  expect_equal(
    test_fun1(iris, c("Sepal.Length", "Sepal.Width")),
    c("Sepal.Length", "Sepal.Width")
  )

  # This turns out to be that the variable name "i" in test_fun1
  # becomes the search string "i" after evaluation, so all columns
  # containing an "i" will be returned.

  expect_equal(
    test_fun1(iris, starts_with("Sep")),
    c("Sepal.Width", "Petal.Width", "Species")
  )

  test_fun1a <- function(data, i) {
    find_columns(data, select = i, regex = TRUE)
  }
  expect_equal(
    test_fun1a(iris, "Sep"),
    c("Sepal.Length", "Sepal.Width")
  )

  test_fun1b <- function(data, i) {
    find_columns(data, select = i, regex = TRUE)
  }
  expect_equal(
    test_fun1b(iris, "Width$"),
    c("Sepal.Width", "Petal.Width")
  )

  test_fun2 <- function(data) {
    find_columns(data, select = starts_with("Sep"))
  }
  expect_equal(
    test_fun2(iris),
    c("Sepal.Length", "Sepal.Width")
  )

  test_fun3 <- function(data) {
    i <- "Sep"
    find_columns(data, select = starts_with(i))
  }
  expect_warning(expect_null(test_fun3(iris)))
})
