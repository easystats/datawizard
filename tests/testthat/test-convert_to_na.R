
data(iris)

test_that("convert_to_na-factor", {
  x <- convert_to_na(iris$Species, na = "versicolor")
  expect_equal(sum(is.na(x)), 50)

  x <- convert_to_na(iris$Species, na = list(2, "versicolor"))
  expect_equal(sum(is.na(x)), 50)

  x <- suppressWarnings(convert_to_na(iris$Species, na = 2))
  expect_warning(convert_to_na(iris$Species, na = 2))
  expect_equal(sum(is.na(x)), 0)
})

test_that("convert_to_na-numeric", {
  x <- convert_to_na(iris$Sepal.Length, na = 5)
  expect_equal(sum(is.na(x)), sum(iris$Sepal.Length == 5))

  x <- convert_to_na(iris$Sepal.Length, na = list(5, "versicolor"))
  expect_equal(sum(is.na(x)), 10)

  x <- suppressWarnings(convert_to_na(iris$Sepal.Width, na = "a"))
  expect_warning(convert_to_na(iris$Sepal.Width, na = "a"))
  expect_equal(sum(is.na(x)), 0)
})

test_that("convert_to_na-df", {
  x <- convert_to_na(iris, na = 5)
  expect_equal(sum(is.na(x)), sum(sapply(iris, function(i) sum(i == 5))))

  x <- convert_to_na(iris, na = list(5, "versicolor"))
  expect_equal(sum(is.na(x)), 64)

  data(iris)
  iris$Sepal.Width <- as.character(iris$Sepal.Width)
  x <- convert_to_na(iris, na = 3)
  expect_equal(sum(is.na(x)), sum(sapply(iris, function(i) if (is.numeric(i)) sum(i == 3) else 0)))

  x <- convert_to_na(iris, na = list(3, "3"))
  expect_equal(sum(is.na(x)), 27)
})
