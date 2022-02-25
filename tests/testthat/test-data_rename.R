test_that("data_remove works as expected", {
  expect_error(
    data_rename(iris, "FakeCol", "length", safe = FALSE),
    "Variable 'FakeCol' is not in your dataframe"
  )

  expect_equal(
    names(data_rename(iris, "Sepal.Length", "length")),
    c("length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    names(data_rename(iris, "FakeCol", "length")),
    names(iris)
  )

  expect_equal(
    names(data_rename(iris, c("Sepal.Length", "Sepal.Width"), c("length", "width"))),
    c("length", "width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    names(data_rename(iris, NULL)),
    c("1", "2", "3", "4", "5")
  )

  expect_equal(
    names(data_rename(iris, paste0("Var", 1:5))),
    paste0("Var", 1:5)
  )
})
