test_that("get_columns works as expected", {
  expect_equal(
    get_columns(iris, starts_with("Sepal")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  expect_equal(
    get_columns(iris, ends_with("Width")),
    iris[c("Sepal.Width", "Petal.Width")]
  )

  expect_equal(
    get_columns(iris, regex("\\.")),
    iris[c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
  )

  expect_equal(
    get_columns(iris, c("Petal.Width", "Sepal.Length")),
    iris[c("Petal.Width", "Sepal.Length")]
  )

  expect_equal(
    get_columns(iris, contains("Wid")),
    iris[c("Sepal.Width", "Petal.Width")]
  )

  expect_equal(
    get_columns(mtcars, c("am", "gear", "cyl")),
    mtcars[c("am", "gear", "cyl")]
  )

  expect_equal(
    get_columns(mtcars, c("vam", "gear", "cyl")),
    mtcars[c("gear", "cyl")]
  )

  expect_equal(
    get_columns(iris, Spec),
    iris["Species"]
  )

  expect_warning(expect_null(get_columns(mtcars, ends_with("abc"))))

  expect_equal(
    get_columns(mtcars, regex("rb$")),
    mtcars["carb"]
  )

  expect_equal(
    get_columns(mtcars, regex("^c")),
    mtcars[c("cyl", "carb")]
  )

  expect_warning(expect_null(get_columns(mtcars, "^c")))

  expect_equal(
    get_columns(mtcars, regex("^C"), ignore_case = TRUE),
    mtcars[c("cyl", "carb")]
  )
})


test_that("get_columns from other functions", {
  test_fun1 <- function(data, i) {
    get_columns(data, select = i)
  }
  expect_equal(
    test_fun1(iris, c("Sepal.Length", "Sepal.Width")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  # This turns out to be that the variable name "i" in test_fun1
  # becomes the search string "i" after evaluation, so all columns
  # containing an "i" will be returned.

  expect_equal(
    test_fun1(iris, starts_with("Sep")),
    iris[c("Sepal.Width", "Petal.Width", "Species")]
  )

  test_fun1a <- function(data, i) {
    get_columns(data, select = i, regex = TRUE)
  }
  expect_equal(
    test_fun1a(iris, "Sep"),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  test_fun1b <- function(data, i) {
    get_columns(data, select = i, regex = TRUE)
  }
  expect_equal(
    test_fun1b(iris, "Width$"),
    iris[c("Sepal.Width", "Petal.Width")]
  )

  test_fun2 <- function(data) {
    get_columns(data, select = starts_with("Sep"))
  }
  expect_equal(
    test_fun2(iris),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  test_fun3 <- function(data) {
    i <- "Sep"
    get_columns(data, select = starts_with(i))
  }
  expect_warning(expect_null(test_fun3(iris)))
})



# preserve attributes --------------------------

test_that("get_columns preserves attributes", {
  skip_if_not_installed("parameters")

  m <- lm(Sepal.Length ~ Species, data = iris)
  out <- parameters::parameters(m)
  a1 <- attributes(out)

  out2 <- get_columns(out, 1:3)
  a2 <- attributes(out2)

  expect_equal(names(a1), names(a2))
})
