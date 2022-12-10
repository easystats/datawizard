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

test_that("skewness works with data frames", {
  skip_if_not_installed("boot")

  set.seed(123)
  expect_snapshot(skewness(iris[, 1:4]))

  set.seed(123)
  expect_snapshot(skewness(iris[, 1:4], iterations = 100))
})

test_that("kurtosis works with data frames", {
  skip_if_not_installed("boot")

  set.seed(123)
  expect_snapshot(kurtosis(iris[, 1:4]))

  set.seed(123)
  expect_snapshot(kurtosis(iris[, 1:4], iterations = 100))
})


test_that("skewness works with matrices", {
  skip_if_not_installed("boot")

  set.seed(123)
  expect_snapshot(skewness(as.matrix(iris[, 1:4])))

  set.seed(123)
  expect_snapshot(skewness(as.matrix(iris[, 1:4]), iterations = 100))
})

test_that("kurtosis works with matrices", {
  skip_if_not_installed("boot")

  set.seed(123)
  expect_snapshot(kurtosis(as.matrix(iris[, 1:4])))

  set.seed(123)
  expect_snapshot(kurtosis(as.matrix(iris[, 1:4]), iterations = 100))
})

test_that("skewness uses type 1 if not enough obs for type 2", {
  expect_warning(
    test <- skewness(c(1, 2), type = "2"),
    "Need at least 3 complete obs"
  )
  expect_equal(test, skewness(c(1, 2), type = "1"))
})

test_that("kurtosis uses type 1 if not enough obs for type 2", {
  expect_warning(
    test <- kurtosis(c(1, 2, 3), type = "2"),
    "Need at least 4 complete obs"
  )
  expect_equal(test, kurtosis(c(1, 2, 3), type = "1"))
})
