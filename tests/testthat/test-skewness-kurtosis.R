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

test_that("skewness SE matches the type", {
  x <- mtcars$mpg
  n <- length(x)
  s1 <- skewness(x, type = "1")
  s2 <- skewness(x, type = "2")
  s3 <- skewness(x, type = "3")
  # oracle (closed-form): SE of g1 under normality
  se1 <- sqrt(6 * (n - 2) / ((n + 1) * (n + 3)))
  expect_equal(s1$SE, se1, tolerance = 1e-10)
  # oracle (closed-form): G1 = g1 * sqrt(n * (n - 1)) / (n - 2) and
  # b1 = g1 * ((n - 1) / n)^1.5, so their SEs scale by the same factors
  expect_equal(s2$SE, se1 * sqrt(n * (n - 1)) / (n - 2), tolerance = 1e-10)
  expect_equal(s3$SE, se1 * ((n - 1) / n)^1.5, tolerance = 1e-10)
  # oracle (invariant): types 2 and 3 are multiples of type 1, so the SE
  # ratio equals the estimate ratio
  expect_equal(s2$SE / s1$SE, s2$Skewness / s1$Skewness, tolerance = 1e-10)
  expect_equal(s3$SE / s1$SE, s3$Skewness / s1$Skewness, tolerance = 1e-10)
})

test_that("kurtosis SE matches the type", {
  x <- mtcars$mpg
  y <- mtcars$wt
  n <- length(x)
  k1 <- kurtosis(x, type = "1")
  k2 <- kurtosis(x, type = "2")
  k3 <- kurtosis(x, type = "3")
  # oracle (closed-form): SE of g2 under normality
  se1 <- sqrt((24 * n * (n - 2) * (n - 3)) / ((n + 1)^2 * (n + 3) * (n + 5)))
  expect_equal(k1$SE, se1, tolerance = 1e-10)
  # oracle (closed-form): types 2 and 3 are linear in g2, with slopes
  # (n + 1) (n - 1) / ((n - 2) (n - 3)) and ((n - 1) / n)^2, so their SEs
  # scale by those slopes
  expect_equal(
    k2$SE,
    se1 * (n + 1) * (n - 1) / ((n - 2) * (n - 3)),
    tolerance = 1e-10
  )
  expect_equal(k3$SE, se1 * ((n - 1) / n)^2, tolerance = 1e-10)
  # oracle (invariant): for two samples of the same size, the change in the
  # type-2 or type-3 estimate per change in the type-1 estimate is the slope
  d1 <- k1$Kurtosis - kurtosis(y, type = "1")$Kurtosis
  d2 <- k2$Kurtosis - kurtosis(y, type = "2")$Kurtosis
  d3 <- k3$Kurtosis - kurtosis(y, type = "3")$Kurtosis
  expect_equal(k2$SE / k1$SE, d2 / d1, tolerance = 1e-10)
  expect_equal(k3$SE / k1$SE, d3 / d1, tolerance = 1e-10)
})

test_that("type-2 SEs match the spread of simulated estimates", {
  # oracle (simulation-coverage): the SD of type-2 estimates over normal
  # samples of size n recovers the analytic SE, which is 8% (skewness) and
  # 30% (kurtosis) above the type-1 SE at n = 20
  set.seed(123)
  n <- 20L
  sims <- matrix(stats::rnorm(n * 20000L), ncol = n)
  d <- sims - rowMeans(sims)
  m2 <- rowMeans(d^2)
  g1 <- rowMeans(d^3) / m2^1.5
  g2 <- rowMeans(d^4) / m2^2 - 3
  big_g1 <- g1 * sqrt(n * (n - 1)) / (n - 2)
  big_g2 <- ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
  x <- sims[1, ]
  expect_equal(skewness(x)$SE, stats::sd(big_g1), tolerance = 0.025)
  expect_equal(kurtosis(x)$SE, stats::sd(big_g2), tolerance = 0.05)
})
