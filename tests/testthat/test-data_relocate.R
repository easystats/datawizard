test_that("data_relocate works as expected", {
  expect_error(
    data_relocate(iris, cols = "Species", before = 2, after = 3),
    "You must supply only one of `before` or `after`."
  )

  expect_error(
    data_relocate(iris, cols = "Species", before = 10),
    "No valid position defined in 'before'."
  )

  expect_error(
    data_relocate(iris, cols = "Species", after = 10),
    "No valid position defined in 'after'."
  )

  expect_equal(
    names(data_relocate(iris, cols = "Species", before = "Sepal.Length")),
    c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
  expect_equal(
    names(data_relocate(iris, cols = "Species", before = "Sepal.Width")),
    c("Sepal.Length", "Species", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_equal(
    names(data_relocate(iris, cols = "Sepal.Width", after = "Species")),
    names(data_relocate(iris, cols = "Sepal.Width", after = -1))
  )

  expect_equal(
    names(data_relocate(iris, cols = c("Species", "Petal.Length"), after = "Sepal.Width")),
    names(data_relocate(iris, cols = c("Species", "Petal.Length"), after = 2))
  )
})
