test_that("smoothness works", {
  set.seed(123)
  x <- (-10:10)^3 + rnorm(21, 0, 100)
  expect_equal(smoothness(x)[[1]], 0.9030014, tolerance = 0.001)
  expect_equal(smoothness(x, method = "auto")[[1]], 1.750452, tolerance = 0.001)
})

test_that("smoothness works with iterations", {
  skip_if_not_installed("boot")

  set.seed(123)
  x <- (-10:10)^3 + rnorm(21, 0, 100)
  expect_equal(
    smoothness(x, iterations = 100)[[1]],
    0.9030014,
    tolerance = 0.001
  )
  expect_equal(
    smoothness(x, method = "auto", iterations = 100)[[1]],
    1.750452,
    tolerance = 0.001
  )
})


test_that("smoothness with lag works", {
  set.seed(123)
  x <- (-10:10)^3 + rnorm(21, 0, 100)
  expect_equal(smoothness(x, lag = 0.5)[[1]], 0.5859015, tolerance = 0.001)
  expect_error(smoothness(x, lag = 0), "'lag' cannot be that small.")
})

test_that("smoothness works with data frames", {
  skip_if(getRversion() < "4.0.0")
  set.seed(123)
  expect_snapshot(smoothness(BOD))
})

test_that("smoothness returns datawizard class", {
  set.seed(123)
  x <- (-10:10)^3 + rnorm(21, 0, 100)
  expect_s3_class(
    smoothness(x),
    c("datawizard_smoothness", "parameters_smoothness", "numeric"),
    exact = TRUE
  )
  expect_s3_class(
    smoothness(BOD),
    c("datawizard_smoothness", "parameters_smoothness", "matrix", "array"),
    exact = TRUE
  )

  skip_if_not_installed("boot")
  expect_s3_class(
    smoothness(x, iterations = 20),
    c("datawizard_smoothness", "parameters_smoothness", "data.frame"),
    exact = TRUE
  )
  expect_s3_class(
    smoothness(BOD, iterations = 20),
    c("datawizard_smoothness", "parameters_smoothness", "data.frame"),
    exact = TRUE
  )
})

test_that("as.numeric and as.double work for smoothness", {
  set.seed(123)
  x <- (-10:10)^3 + rnorm(21, 0, 100)
  out <- smoothness(x)
  expect_identical(as.numeric(out), unclass(out))
  expect_identical(as.double(out), unclass(out))

  skip_if_not_installed("boot")
  out <- smoothness(x, iterations = 20)
  expect_identical(as.numeric(out), out$Smoothness)
  expect_identical(as.double(out), out$Smoothness)
})

test_that("objects with the old parameters_smoothness class still work", {
  set.seed(123)
  x <- (-10:10)^3 + rnorm(21, 0, 100)
  expect_old_class_works <- function(new) {
    old <- new
    class(old) <- setdiff(class(new), "datawizard_smoothness")
    expect_identical(as.numeric(old), as.numeric(new))
    expect_identical(as.double(old), as.double(new))
  }
  expect_old_class_works(smoothness(x))

  skip_if_not_installed("boot")
  expect_old_class_works(smoothness(x, iterations = 20))
})
