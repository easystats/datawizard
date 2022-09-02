test_that("data_peek works as expected", {
  out <- data_peek(iris)
  expect_equal(colnames(out), c("Variable", "Type", "Values"))
  expect_equal(
    out$Variable,
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
  expect_equal(dim(out), c(5, 3))
})

test_that("data_peek works as expected with custom n", {
  out <- data_peek(iris, n = 3)
  expect_equal(colnames(out), c("Variable", "Type", "Values"))
  expect_equal(
    out$Variable,
    c("Sepal.Length", "Sepal.Width", "Petal.Length")
  )
  expect_equal(dim(out), c(3, 3))
})

test_that("data_peek works as expetced with custom width", {
  out <- data_peek(iris, width = 130)
  expect_equal(colnames(out), c("Variable", "Type", "Values"))
  expect_equal(
    out$Variable,
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
  expect_equal(dim(out), c(5, 3))
  expect_equal(
    out$Values[1],
    "5.1, 4.9, 4.7, 4.6, 5, 5.4, 4.6, 5, 4.4, 4.9, 5.4, 4.8, 4.8, 4.3, 5.8, 5.7, 5.4, 5.1, 5.7, 5.1, 5.4, 5.1, 4.6, 5.1, 4.8, 5, 5, 5.2, 5.2, 4.7, 4.8, 5.4, 5.2, 5.5, 4.9, 5, 5.5, 4.9, 4.4, 5.1, 5, 4.5, 4.4, 5, 5.1, 4.8, 5.1, 4.6, 5.3, 5, 7, 6.4, 6.9, 5.5, 6.5, 5.7, 6.3, 4.9, 6.6, 5.2, 5, 5.9, 6, 6.1, 5.6, 6.7, 5.6, 5.8, 6.2, 5.6, 5.9, 6.1, 6.3, 6.1, 6.4, 6.6, 6.8, 6.7, 6, 5.7, 5.5, 5.5, 5.8, 6, 5.4, 6, 6.7, 6.3, 5.6, 5.5, 5.5, 6.1, 5.8, 5, 5.6, 5.7, 5.7, 6.2, 5.1, 5.7, 6.3, 5.8, 7.1, 6.3, 6.5, 7.6, 4.9, 7.3, 6.7, 7.2, 6.5, 6.4, 6.8, 5.7, 5.8, 6.4, 6.5, 7.7, 7.7, 6, 6.9, 5.6, 7.7, 6.3, 6.7, 7.2, 6.2, 6.1, 6.4, 7.2"
  )
})

test_that("data_peek snapshots look as expected", {
  expect_snapshot(data_peek(iris))
  expect_snapshot(data_peek(iris, n = 3))
  expect_snapshot(data_peek(iris, width = 130))
})
