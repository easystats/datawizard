test_that("skewness", {
  expect_equal(skewness(iris$Sepal.Length)[[1]], 0.314911, tolerance = 1e-3)
  expect_equal(
    skewness(iris$Sepal.Length, type = 1)[[1]],
    0.3117531,
    tolerance = 1e-3
  )
  expect_equal(
    skewness(iris$Sepal.Length, type = 3)[[1]],
    0.3086407,
    tolerance = 1e-3
  )
})

test_that("kurtosis", {
  expect_equal(kurtosis(iris$Sepal.Length)[[1]], -0.552064, tolerance = 1e-3)
  expect_equal(
    kurtosis(iris$Sepal.Length, type = 1)[[1]],
    -0.5735679,
    tolerance = 1e-3
  )
  expect_equal(
    kurtosis(iris$Sepal.Length, type = 3)[[1]],
    -0.6058125,
    tolerance = 1e-3
  )
})

test_that("kurtosis and skewness with bootstrapping", {
  skip_if_not_installed("boot")

  set.seed(123)
  expect_equal(
    skewness(iris$Sepal.Length, iterations = 100)[[2]],
    0.1262203,
    tolerance = 1e-3
  )

  set.seed(123)
  expect_equal(
    kurtosis(iris$Sepal.Length, iterations = 100)[[2]],
    0.1878741,
    tolerance = 1e-3
  )
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

test_that("skewness and kurtosis return datawizard classes", {
  inputs <- list(
    vector = iris$Sepal.Length,
    data_frame = iris[, 1:4],
    matrix = as.matrix(iris[, 1:4])
  )
  for (x in inputs) {
    expect_s3_class(
      skewness(x),
      c("datawizard_skewness", "parameters_skewness", "data.frame"),
      exact = TRUE
    )
    expect_s3_class(
      kurtosis(x),
      c("datawizard_kurtosis", "parameters_kurtosis", "data.frame"),
      exact = TRUE
    )
  }
})

test_that("summary, as.numeric and as.double work for skewness and kurtosis", {
  inputs <- list(vector = iris$Sepal.Length, data_frame = iris[, 1:4])
  for (x in inputs) {
    skew <- skewness(x)
    kurt <- kurtosis(x)

    expect_true(all(c("z", "p") %in% colnames(summary(skew, test = TRUE))))
    expect_true(all(c("z", "p") %in% colnames(summary(kurt, test = TRUE))))

    expect_identical(as.numeric(skew), skew$Skewness)
    expect_identical(as.double(skew), skew$Skewness)
    expect_identical(as.numeric(kurt), kurt$Kurtosis)
    expect_identical(as.double(kurt), kurt$Kurtosis)
  }
})

test_that("objects with the old parameters_* classes still work", {
  inputs <- list(vector = iris$Sepal.Length, data_frame = iris[, 1:4])
  for (x in inputs) {
    new_skew <- skewness(x)
    old_skew <- new_skew
    class(old_skew) <- c("parameters_skewness", "data.frame")

    new_kurt <- kurtosis(x)
    old_kurt <- new_kurt
    class(old_kurt) <- c("parameters_kurtosis", "data.frame")

    pairs <- list(list(old_skew, new_skew), list(old_kurt, new_kurt))
    for (pair in pairs) {
      old <- pair[[1]]
      new <- pair[[2]]
      expect_identical(capture.output(print(old)), capture.output(print(new)))
      expect_identical(
        capture.output(print(old, test = TRUE)),
        capture.output(print(new, test = TRUE))
      )
      expect_identical(
        unclass(summary(old, test = TRUE)),
        unclass(summary(new, test = TRUE))
      )
      expect_s3_class(summary(old, test = TRUE), class(old), exact = TRUE)
      expect_identical(as.numeric(old), as.numeric(new))
      expect_identical(as.double(old), as.double(new))
    }
  }
})
