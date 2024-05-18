# standardize -----------------------------------------------------
test_that("standardize.datagrid", {
  x <- insight::get_datagrid(iris, by = "Sepal.Length", range = "sd", length = 3)
  out <- standardize(x)
  expect_identical(as.numeric(out$Sepal.Length), c(-1, 0, 1), tolerance = 1e-3)
  expect_identical(as.numeric(out$Sepal.Width), c(0, 0, 0), tolerance = 1e-3)

  x <- insight::get_datagrid(iris, by = "Sepal.Length = c(-1, 0)")
  out <- unstandardize(x, select = "Sepal.Length")
  expect_identical(
    out$Sepal.Length[1:2],
    c(mean(iris$Sepal.Length) - sd(iris$Sepal.Length), mean(iris$Sepal.Length)),
    tolerance = 1e-3
  )
})
