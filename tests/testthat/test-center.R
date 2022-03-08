
data(iris)
data(mtcars)
z <- center(iris$Sepal.Width)
test_that("center", {
  expect_equal(as.vector(z), iris$Sepal.Width - mean(iris$Sepal.Width), tolerance = 1e-4, ignore_attr = TRUE)
})
z <- center(mtcars$hp, robust = TRUE)
test_that("center, robust", {
  expect_equal(as.vector(z), mtcars$hp - median(mtcars$hp), tolerance = 1e-4, ignore_attr = TRUE)
})

z <- center(iris, select = "Sepal.Width")
test_that("center, select", {
  expect_equal(as.vector(z$Sepal.Width), iris$Sepal.Width - mean(iris$Sepal.Width), tolerance = 1e-4, ignore_attr = TRUE)
})

z <- center(iris, select = "Species")
test_that("center, factors", {
  expect_equal(z$Species, iris$Species)
})

z <- center(iris, select = "Species", force = TRUE)
v <- as.numeric(iris$Species)
test_that("center, force factors", {
  expect_equal(as.vector(z$Species), v - median(v), tolerance = 1e-4, ignore_attr = TRUE)
})

test_that("center, all na", {
  z <- center(c(NA, NA, NA))
  expect_equal(z, c(NA, NA, NA))
})
