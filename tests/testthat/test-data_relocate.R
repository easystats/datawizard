test_that("data_relocate works as expected", {
  expect_error(
    data_relocate(iris, select = "Species", before = 2, after = 3),
    "You must supply only one of `before` or `after`."
  )

  expect_error(
    data_relocate(iris, select = "Species", before = 10),
    "No valid position defined in 'before'."
  )

  expect_error(
    data_relocate(iris, select = "Species", after = 10),
    "No valid position defined in 'after'."
  )

  expect_equal(
    names(data_relocate(iris, select = "Species", before = "Sepal.Length")),
    c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
  expect_equal(
    names(data_relocate(iris, select = "Species", before = "Sepal.Width")),
    c("Sepal.Length", "Species", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_equal(
    names(data_relocate(iris, select = "Sepal.Width", after = "Species")),
    names(data_relocate(iris, select = "Sepal.Width", after = -1))
  )

  expect_equal(
    names(data_relocate(iris, select = c("Species", "Petal.Length"), after = "Sepal.Width")),
    names(data_relocate(iris, select = c("Species", "Petal.Length"), after = 2))
  )
})



test_that("data_relocate select-helpers", {
  expect_equal(
    colnames(data_relocate(iris, select = starts_with("Sepal"), after = 5)),
    colnames(iris[c(3:5, 1:2)])
  )
  expect_equal(
    colnames(data_relocate(iris, select = 1:2, after = 5)),
    colnames(iris[c(3:5, 1:2)])
  )
  expect_equal(
    colnames(data_relocate(iris, select = -1)),
    colnames(iris[c(5, 1:4)])
  )
  expect_equal(
    colnames(data_relocate(iris, select = Species, after = 1)),
    colnames(iris[c(1, 5, 2:4)])
  )
  expect_equal(
    colnames(data_relocate(iris, select = ~ Sepal.Width + Species)),
    colnames(iris[c(2, 5, 1, 3:4)])
  )
  expect_equal(
    colnames(data_relocate(iris, select = starts_with("sepal"), after = 5)),
    colnames(iris)
  )
  expect_equal(
    colnames(data_relocate(iris, select = starts_with("sepal"), after = 5, ignore_case = TRUE)),
    colnames(iris[c(3:5, 1:2)])
  )
})


# preserve attributes --------------------------

test_that("data_relocate preserves attributes", {
  skip_if_not_installed("parameters")

  m <- lm(Sepal.Length ~ Species, data = iris)
  out <- parameters::parameters(m)
  a1 <- attributes(out)

  out2 <- data_relocate(out, 4:6)
  a2 <- attributes(out2)

  expect_equal(names(a1), names(a2))
})
