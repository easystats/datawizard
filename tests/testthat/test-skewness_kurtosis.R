test_that("skewness", {
  expect_equal(skewness(iris$Sepal.Length)[[1]], 0.314911, tolerance = 1e-3)
  expect_equal(skewness(iris$Sepal.Length, type = 1)[[1]], 0.3117531, tolerance = 1e-3)
  expect_equal(skewness(iris$Sepal.Length, type = 3)[[1]], 0.3086407, tolerance = 1e-3)
})

test_that("kurtosis", {
  expect_equal(kurtosis(iris$Sepal.Length)[[1]], -0.552064, tolerance = 1e-3)
  expect_equal(kurtosis(iris$Sepal.Length, type = 1)[[1]], -0.5735679, tolerance = 1e-3)
  expect_equal(kurtosis(iris$Sepal.Length, type = 3)[[1]], -0.6058125, tolerance = 1e-3)
})

test_that("kurtosis and skewness with bootstrapping", {
  skip_if_not_installed("boot")

  set.seed(123)
  expect_equal(skewness(iris$Sepal.Length, iterations = 100)[[2]], 0.1262203, tolerance = 1e-3)

  set.seed(123)
  expect_equal(kurtosis(iris$Sepal.Length, iterations = 100)[[2]], 0.1878741, tolerance = 1e-3)
})

test_that("skewness", {
  expect_equal(
    skewness(iris[, 1:4])[[2]],
    c(
      0.314910956636973, 0.318965664713603,
      -0.274884179751012, -0.10296674764898
    ),
    tolerance = 1e-3
  )
})

test_that("kurtosis", {
  expect_equal(
    kurtosis(iris[, 1:4])[[2]],
    c(
      -0.552064041315639, 0.228249042468194,
      -1.40210341552175, -1.34060399661265
    ),
    tolerance = 1e-3
  )
})
