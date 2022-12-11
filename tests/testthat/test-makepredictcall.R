# library(testthat)

test_that("makepredictcall", {
  data("mtcars")
  nd <- data.frame(hp = c(100, 200))

  m1 <- lm(mpg ~ scale(hp, scale = FALSE), mtcars)
  m2 <- lm(mpg ~ center(hp), mtcars)

  m3 <- lm(mpg ~ scale(hp), mtcars)
  m4 <- lm(mpg ~ standardize(hp), mtcars)

  p1 <- predict(m1, nd)
  expect_equal(p1, predict(m2, nd))
  expect_equal(p1, predict(m3, nd))
  expect_equal(p1, predict(m4, nd))

  X <- matrix(rnorm(100), ncol = 2)
  Y <- rnorm(50)
  expect_error(lm(Y ~ standardize(X)), "matrices")
})


test_that("makepredictcall, normalize", {
  data("mtcars")
  m1 <- lm(mpg ~ normalize(hp), data = mtcars)
  m2 <- lm(mpg ~ hp, data = mtcars)
  m3 <- lm(mpg ~ normalize(hp, include_bounds = FALSE), data = mtcars)

  out1 <- predict(m1, newdata = data.frame(hp = c(100, 110, 120)))
  out2 <- predict(m2, newdata = data.frame(hp = c(100, 110, 120)))
  out3 <- predict(m3, newdata = data.frame(hp = c(100, 110, 120)))

  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1, out3, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out2, out3, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- predict(m1, newdata = data.frame(hp = 110))
  out2 <- predict(m2, newdata = data.frame(hp = 110))
  out3 <- predict(m3, newdata = data.frame(hp = 110))

  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1, out3, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out2, out3, tolerance = 1e-3, ignore_attr = TRUE)
})


test_that("makepredictcall, rescale", {
  data("mtcars")
  m1 <- lm(mpg ~ rescale(hp, to = c(50, 80)), data = mtcars)
  m2 <- lm(mpg ~ hp, data = mtcars)
  m3 <- lm(mpg ~ rescale(hp), data = mtcars)

  out1 <- predict(m1, newdata = data.frame(hp = c(100, 110, 120)))
  out2 <- predict(m2, newdata = data.frame(hp = c(100, 110, 120)))
  out3 <- predict(m3, newdata = data.frame(hp = c(100, 110, 120)))

  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1, out3, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out2, out3, tolerance = 1e-3, ignore_attr = TRUE)

  out1 <- predict(m1, newdata = data.frame(hp = 110))
  out2 <- predict(m2, newdata = data.frame(hp = 110))
  out3 <- predict(m3, newdata = data.frame(hp = 110))

  expect_equal(out1, out2, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out1, out3, tolerance = 1e-3, ignore_attr = TRUE)
  expect_equal(out2, out3, tolerance = 1e-3, ignore_attr = TRUE)
})
