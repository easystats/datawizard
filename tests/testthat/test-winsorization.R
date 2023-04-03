test_that("testing Winsorization of factors", {
  expect_identical(winsorize(as.factor(mtcars$am)), as.factor(mtcars$am))
})

test_that("with missing values", {
  skip_if_not_installed("ggplot2")

  expect_snapshot(suppressWarnings(head(winsorize(na.omit(ggplot2::msleep$brainwt)))))
  expect_length(winsorize(as.factor(ggplot2::msleep$vore)), 83L)
})

test_that("winsorize: threshold must be between 0 and 1", {
  expect_error(
    winsorize(sample(1:10, 5), threshold = -0.1),
    regexp = "must be a scalar between 0 and 0.5"
  )
  expect_error(
    winsorize(sample(1:10, 5), threshold = 1.1),
    regexp = "must be a scalar between 0 and 0.5"
  )
  expect_error(
    winsorize(sample(1:10, 5), method = "zscore", threshold = -3),
    regexp = "must be a scalar greater than 0"
  )
  expect_error(
    winsorize(sample(1:10, 5), method = "zscore", threshold = -3, robust = TRUE),
    regexp = "must be a scalar greater than 0"
  )
  expect_error(
    winsorize(sample(1:10, 5), method = "raw", threshold = 1.1),
    regexp = "must be of length 2 for lower and upper bound"
  )
})

test_that("winsorize on data.frame", {
  iris2 <- winsorize(iris)
  expect_identical(
    iris2$Sepal.Length,
    winsorize(iris$Sepal.Length)
  )
  expect_identical(
    iris2$Petal.Width,
    winsorize(iris$Petal.Width)
  )
  expect_named(iris2, names(iris))
})
