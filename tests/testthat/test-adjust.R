test_that("adjust multilevel", {
  skip_if_not_installed("lme4")
  adj <- adjust(iris[c("Sepal.Length", "Species")], multilevel = TRUE, bayesian = FALSE)
  expect_equal(
    head(adj$Sepal.Length),
    c(0.08698, -0.11302, -0.31302, -0.41302, -0.01302, 0.38698),
    tolerance = 1e-3
  )
})

test_that("adjust", {
  adj <- adjust(iris[c("Sepal.Length", "Species")], multilevel = FALSE, bayesian = FALSE)
  expect_equal(
    head(adj$Sepal.Length),
    c(0.094, -0.106, -0.306, -0.406, -0.006, 0.394),
    tolerance = 1e-3
  )
})

# select helpers ------------------------------
test_that("adjust regex", {
  expect_identical(
    adjust(mtcars, select = "pg", regex = TRUE),
    adjust(mtcars, select = "mpg")
  )
  expect_identical(
    adjust(mtcars, select = "pg$", regex = TRUE),
    adjust(mtcars, select = "mpg")
  )
})
