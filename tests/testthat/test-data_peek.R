test_that("data_peek works as expected", {
  out <- data_peek(iris)
  expect_equal(colnames(out), c("Variable", "Type", "Values"))
  expect_equal(
    out$Variable,
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
  expect_equal(dim(out), c(5, 3))
})

test_that("data_peek works as expected with select", {
  out <- data_peek(iris, select = 2:4)
  expect_equal(colnames(out), c("Variable", "Type", "Values"))
  expect_equal(
    out$Variable,
    c("Sepal.Width", "Petal.Length", "Petal.Width")
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
})

test_that("data_peek snapshots look as expected", {
  expect_snapshot(data_peek(iris))
  expect_snapshot(data_peek(iris, select = 1:3))
  expect_snapshot(data_peek(iris, width = 130))
})
