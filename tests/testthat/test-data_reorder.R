test_that("data_reorder works as expected", {
  expect_equal(
    names(data_reorder(iris, c("Species", "Sepal.Length"))),
    c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_warning(expect_equal(
    names(data_reorder(iris, c("Species", "dupa"))),
    c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  ))
})
