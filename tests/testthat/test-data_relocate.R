test_that("data_relocate works as expected", {
  expect_error(
    data_relocate(iris, select = "Species", before = 2, after = 3),
    "You must supply only one of `before` or `after`."
  )

  expect_error(
    data_relocate(iris, select = "Species", before = 10),
    "No valid position defined in `before`."
  )

  expect_error(
    data_relocate(iris, select = "Species", after = 10),
    "No valid position defined in `after`."
  )

  expect_named(
    data_relocate(iris, select = "Species", before = "Sepal.Length"),
    c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
  expect_named(
    data_relocate(iris, select = "Species", before = "Sepal.Width"),
    c("Sepal.Length", "Species", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_named(
    data_relocate(iris, select = "Sepal.Width", after = "Species"),
    names(data_relocate(iris, select = "Sepal.Width", after = -1))
  )

  expect_named(
    data_relocate(iris, select = c("Species", "Petal.Length"), after = "Sepal.Width"),
    names(data_relocate(iris, select = c("Species", "Petal.Length"), after = 2))
  )
})



test_that("data_relocate select-helpers", {
  expect_identical(
    colnames(data_relocate(iris, select = starts_with("Sepal"), after = 5)),
    colnames(iris[c(3:5, 1:2)])
  )
  expect_identical(
    colnames(data_relocate(iris, select = 1:2, after = 5)),
    colnames(iris[c(3:5, 1:2)])
  )
  expect_identical(
    colnames(data_relocate(iris, select = -1)),
    colnames(iris[c(2:5, 1)])
  )
  expect_identical(
    colnames(data_relocate(iris, select = Species, after = 1)),
    colnames(iris[c(1, 5, 2:4)])
  )
  expect_identical(
    colnames(data_relocate(iris, select = ~ Sepal.Width + Species)),
    colnames(iris[c(2, 5, 1, 3:4)])
  )
  expect_identical(
    colnames(data_relocate(iris, select = starts_with("sepal"), after = 5)),
    colnames(iris)
  )
  expect_identical(
    colnames(data_relocate(iris, select = starts_with("sepal"), after = 5, ignore_case = TRUE)),
    colnames(iris[c(3:5, 1:2)])
  )
})


# preserve attributes --------------------------

test_that("data_relocate preserves attributes", {
  skip_if_not_or_load_if_installed("parameters")

  m <- lm(Sepal.Length ~ Species, data = iris)
  out <- parameters::parameters(m)
  a1 <- attributes(out)

  out2 <- data_relocate(out, 4:6)
  a2 <- attributes(out2)

  # attributes may not be in the same order
  expect_true(all(names(a1) %in% names(a2)))
  expect_identical(length(a1), length(a2))
})


# select helpers ------------------------------
test_that("data_relocate regex", {
  expect_identical(
    names(data_relocate(mtcars, select = "pg", regex = TRUE, after = "carb"))[11],
    "mpg"
  )
})


# fuzzy matching ------------------------------
out <- data.frame(
  Parameter = "Test",
  Median = 0.5,
  CI_low = 0.4,
  CI_high = 0.6,
  pd = 0.97,
  Rhat = 0.99,
  ESS = 1000,
  log_BF = 3,
  stringsAsFactors = FALSE
)

test_that("data_relocate misspelled", {
  # close match
  expect_error(
    data_relocate(out, "pd", before = "BF"),
    "log_BF"
  )
  # close multiple matches
  expect_error(
    data_relocate(out, "pd", before = "CIl"),
    "CI_low"
  )
  # not even close
  expect_error(
    data_relocate(out, "pd", before = "xyz"),
    "misspelled"
  )
})
