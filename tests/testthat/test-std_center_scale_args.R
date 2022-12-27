d <- data.frame(a = 1:5, b = 21:25, c = 41:45)

test_that("standardize", {
  x <- standardize(d)
  expect_equal(as.vector(x$a), as.vector(scale(d$a)), tolerance = 0.001)
})

test_that("standardize", {
  x <- standardize(d, center = 5, scale = 2)
  expect_equal(as.vector(x$a), c(-2, -1.5, -1, -0.5, 0), tolerance = 0.001)
})

test_that("standardize", {
  x <- standardize(d, center = c(5, 25, 45), scale = c(3, 3, 3))
  expect_equal(as.vector(x$a), c(-1.33333, -1, -0.66667, -0.33333, 0), tolerance = 0.001)
  expect_equal(as.vector(x$b), c(-1.33333, -1, -0.66667, -0.33333, 0), tolerance = 0.001)
})

test_that("standardize", {
  x <- standardize(d, center = c(c = 45, a = 5, b = 25), scale = c(3, 3, 3))
  expect_equal(as.vector(x$a), c(-1.33333, -1, -0.66667, -0.33333, 0), tolerance = 0.001)
  expect_equal(as.vector(x$b), c(-1.33333, -1, -0.66667, -0.33333, 0), tolerance = 0.001)
})

test_that("standardize", {
  x <- standardize(d, center = c(c = 45, a = 5, b = 25), scale = c(1, 2, 3))
  expect_equal(as.vector(x$a), c(-4, -3, -2, -1, 0), tolerance = 0.001)
  expect_equal(as.vector(x$b), c(-2, -1.5, -1, -0.5, 0), tolerance = 0.001)
})

test_that("standardize", {
  x <- standardize(d, center = c(c = 45, a = 5, b = 25), scale = c(a = 1, b = 2, c = 3))
  expect_equal(as.vector(x$a), c(-4, -3, -2, -1, 0), tolerance = 0.001)
  expect_equal(as.vector(x$b), c(-2, -1.5, -1, -0.5, 0), tolerance = 0.001)
})

test_that("standardize", {
  x <- standardize(d, center = c(c = 45, a = 5, b = 25), scale = c(c = 3, b = 2, a = 1))
  expect_equal(as.vector(x$a), c(-4, -3, -2, -1, 0), tolerance = 0.001)
  expect_equal(as.vector(x$b), c(-2, -1.5, -1, -0.5, 0), tolerance = 0.001)
})

test_that("standardize", {
  x <- standardize(d, center = c(c = 45, a = 5, b = 25), scale = c(c = 1, b = 2, a = 3))
  expect_equal(as.vector(x$a), c(-1.33333, -1, -0.66667, -0.33333, 0), tolerance = 0.001)
  expect_equal(as.vector(x$b), c(-2, -1.5, -1, -0.5, 0), tolerance = 0.001)
})
