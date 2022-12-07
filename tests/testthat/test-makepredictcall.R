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
