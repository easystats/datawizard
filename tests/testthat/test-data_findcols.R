test_that("data_findcols works as expected", {
  expect_equal(
    data_findcols(iris, starts_with = "Sepal"),
    c("Sepal.Length", "Sepal.Width")
  )

  expect_equal(
    data_findcols(iris, ends_with = "Width"),
    c("Sepal.Width", "Petal.Width")
  )

  expect_equal(
    data_findcols(iris, pattern = "\\."),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_equal(
    data_findcols(iris, c("Petal.Width", "Sepal.Length")),
    c("Petal.Width", "Sepal.Length")
  )
})



test_that("data_findcols nse", {
  expect_equal(
    data_findcols(iris, pattern = starts_with("Sepal")),
    c("Sepal.Length", "Sepal.Width")
  )

  expect_equal(
    data_findcols(iris, pattern = ends_with("Width")),
    c("Sepal.Width", "Petal.Width")
  )

  expect_equal(
    data_findcols(iris, pattern = "\\."),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_equal(
    data_findcols(iris, pattern = contains("Wid")),
    c("Sepal.Width", "Petal.Width")
  )

  expect_equal(
    data_findcols(iris, c("Petal.Width", "Sepal.Length")),
    c("Petal.Width", "Sepal.Length")
  )

  expect_equal(
    data_findcols(mtcars, c("am", "gear", "cyl")),
    c("am", "gear", "cyl")
  )

  expect_equal(
    data_findcols(mtcars, c("vam", "gear", "cyl")),
    c("gear", "cyl")
  )

  expect_equal(
    data_findcols(iris, Spec),
    "Species"
  )

  expect_equal(
    data_findcols(mtcars, ends_with = "abc"),
    vector("character")
  )

  expect_equal(
    data_findcols(mtcars, "rb$"),
    "carb"
  )

  expect_equal(
    data_findcols(mtcars, "^c"),
    c("cyl", "carb")
  )

  expect_equal(
    data_findcols(mtcars, "^C"),
    vector("character")
  )

  expect_equal(
    data_findcols(mtcars, "^C", ignore_case = TRUE),
    c("cyl", "carb")
  )
})


test_that("data_findcols from other functions", {
  test_fun <- function(data, i) {
    data_findcols(data, pattern = i)
  }
  expect_equal(
    test_fun(iris, "Sep"),
    c("Sepal.Length", "Sepal.Width")
  )
})
