# numeric
test_that("slide", {
  x <- c(10, 11, 12)
  expect_identical(slide(x), c(0, 1, 2))

  x <- c(10, 11, 12)
  expect_identical(slide(x, lowest = 10), x)

  x <- c(10, 11, 12)
  expect_identical(slide(x, lowest = 1), c(1, 2, 3))

  x <- c(10, 11, NA, 12)
  expect_identical(slide(x, lowest = 1), c(1, 2, NA, 3))
})

# factor
test_that("slide", {
  data(efc)
  expect_message(expect_identical(slide(efc$e42dep), efc$e42dep))
})

# data frame
test_that("slide", {
  data(iris)
  expect_message(
    out <- slide(iris), # nolint
    "Shifting non-numeric variables is not possible"
  )
  expect_identical(out$Species, iris$Species)
  expect_identical(range(out$Sepal.Length), c(0, 3.6), tolerance = 1e-2)
})

# select helpers ------------------------------
test_that("slide regex", {
  expect_identical(
    slide(mtcars, select = "pg", regex = TRUE),
    slide(mtcars, select = "mpg")
  )
})
