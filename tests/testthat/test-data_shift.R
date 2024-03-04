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
    {
      out <- slide(iris)
    },
    regex = "Shifting"
  )
  expect_identical(out$Species, iris$Species)
  expect_equal(range(out$Sepal.Length), c(0, 3.6), tolerance = 1e-2)
  out <- slide(iris[1:3], lowest = 1, append = TRUE)
  expect_equal(range(out$Sepal.Length_s), c(1, 4.6), tolerance = 1e-2)
  expect_named(
    out,
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Sepal.Length_s",
      "Sepal.Width_s", "Petal.Length_s"
    )
  )
})

# select helpers ------------------------------
test_that("slide regex", {
  expect_identical(
    slide(mtcars, select = "pg", regex = TRUE),
    slide(mtcars, select = "mpg")
  )
})
