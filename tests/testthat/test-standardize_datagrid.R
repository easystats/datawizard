# standardize -----------------------------------------------------
test_that("standardize.datagrid", {
  x <- insight::get_datagrid(iris, at = "Sepal.Length", range = "sd", length = 3)
  out <- standardize(x)
  expect_equal(as.numeric(out$Sepal.Length), c(-1, 0, 1))
  expect_equal(as.numeric(out$Sepal.Width), c(0, 0, 0))

  x <- insight::get_datagrid(iris, at = "Sepal.Length = c(-1, 0)")
  out <- unstandardize(x, select = "Sepal.Length")
  expect_equal(out$Sepal.Length[1:2], c(mean(iris$Sepal.Length) - sd(iris$Sepal.Length), mean(iris$Sepal.Length)))
})
