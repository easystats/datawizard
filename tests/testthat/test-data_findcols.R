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
