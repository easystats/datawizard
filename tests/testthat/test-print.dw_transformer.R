test_that("print.dw_transformer", {
  data(iris)
  expect_snapshot(rescale(iris$Sepal.Length))
  expect_snapshot(normalize(iris$Sepal.Length))
  expect_snapshot(center(iris$Sepal.Length))
  expect_snapshot(standardize(iris$Sepal.Length))
})
