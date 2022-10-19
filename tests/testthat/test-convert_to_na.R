
data(iris)

test_that("convert_to_na-factor", {
  x <- convert_to_na(iris$Species, na = "versicolor")
  expect_equal(sum(is.na(x)), 50)

  x <- convert_to_na(iris$Species, na = list(2, "versicolor"))
  expect_equal(sum(is.na(x)), 50)

  x <- convert_to_na(iris$Species, na = list(2, "versicolor"), drop_levels = FALSE)
  expect_equal(levels(x), c("setosa", "versicolor", "virginica"))
  expect_equal(as.vector(table(x)), c(50, 0, 50))

  x <- convert_to_na(iris$Species, na = list(2, "versicolor"), drop_levels = TRUE)
  expect_equal(levels(x), c("setosa", "virginica"))
  expect_equal(as.vector(table(x)), c(50, 50))

  x <- suppressWarnings(convert_to_na(iris$Species, na = 2))
  expect_message(convert_to_na(iris$Species, na = 2))
  expect_equal(sum(is.na(x)), 0)
})

test_that("convert_to_na-numeric", {
  x <- convert_to_na(iris$Sepal.Length, na = 5)
  expect_equal(sum(is.na(x)), sum(iris$Sepal.Length == 5))

  x <- convert_to_na(iris$Sepal.Length, na = list(5, "versicolor"))
  expect_equal(sum(is.na(x)), 10)

  x <- suppressWarnings(convert_to_na(iris$Sepal.Width, na = "a"))
  expect_message(convert_to_na(iris$Sepal.Width, na = "a"))
  expect_equal(sum(is.na(x)), 0)
})

test_that("convert_to_na-df", {
  expect_message(x <- convert_to_na(iris, na = 5))
  expect_equal(sum(is.na(x)), sum(sapply(iris, function(i) sum(i == 5))))

  x <- convert_to_na(iris, na = list(5, "versicolor"))
  expect_equal(sum(is.na(x)), 64)

  data(iris)
  iris$Sepal.Width <- as.character(iris$Sepal.Width)
  expect_message(expect_message(x <- convert_to_na(iris, na = 3)))
  expect_equal(sum(is.na(x)), sum(sapply(iris, function(i) if (is.numeric(i)) sum(i == 3) else 0)))

  x <- convert_to_na(iris, na = list(3, "3"))
  expect_equal(sum(is.na(x)), 27)
})


test_that("convert_to_na other classes", {
  d <- data.frame(
    a = 1:5,
    b = factor(letters[1:5]),
    c = as.Date(c("2022-03-22", "2022-01-02", "2022-02-02", "2021-04-02", "2020-01-19")),
    d = c(TRUE, TRUE, FALSE, FALSE, TRUE),
    e = as.complex(1:5)
  )

  x <- convert_to_na(d$a, na = 3)
  expect_equal(x, c(1, 2, NA, 4, 5), tolerance = 1e-3, ignore_attr = TRUE)
  expect_message(x <- convert_to_na(d$a, na = "c"))
  expect_equal(x, 1:5, tolerance = 1e-3, ignore_attr = TRUE)

  x <- convert_to_na(d$b, na = "c")
  expect_equal(x, structure(c(1L, 2L, NA, 4L, 5L),
    .Label = c("a", "b", "c", "d", "e"),
    class = "factor"
  ), tolerance = 1e-3, ignore_attr = TRUE)

  x <- convert_to_na(d$b, na = "c", drop_levels = TRUE)
  expect_equal(x, structure(c(1L, 2L, NA, 3L, 4L),
    .Label = c("a", "b", "d", "e"),
    class = "factor"
  ), tolerance = 1e-3, ignore_attr = TRUE)

  expect_message(
    convert_to_na(d$c, na = "2022-03-22"),
    "of class 'Date'"
  )
  x <- convert_to_na(d$c, na = as.Date("2022-03-22"))
  expect_equal(x, structure(c(NA, 18994, 19025, 18719, 18280), class = "Date"), tolerance = 1e-3, ignore_attr = TRUE)

  x <- convert_to_na(d$d, na = TRUE)
  expect_equal(x, c(NA, NA, FALSE, FALSE, NA), tolerance = 1e-3, ignore_attr = TRUE)
  expect_message(x <- convert_to_na(d$e, na = as.complex(4)))
  expect_equal(x, d$e, tolerance = 1e-3, ignore_attr = TRUE)

  out <- data.frame(
    a = c(1, 2, NA, 4, 5),
    b = factor(c("a", "b", NA, "d", "e"), levels = letters[1:5]),
    c = as.Date(c("2022-03-22", "2022-01-02", "2022-02-02", "2021-04-02", "2020-01-19")),
    d = c(NA, NA, FALSE, FALSE, NA),
    e = as.complex(1:5)
  )
  expect_message(
    convert_to_na(d, na = list(3, "c", TRUE, "2022-01-02")),
    "must be of class 'Date'"
  )
  x <- convert_to_na(d, na = list(3, "c", TRUE, as.Date("2022-01-02")))
  expect_equal(x, out, ignore_attr = TRUE, tolerance = 1e-3)
})

# select helpers ------------------------------
test_that("convert_to_na regex", {
  expect_equal(
    convert_to_na(mtcars, na = 4, select = "arb", regex = TRUE),
    convert_to_na(mtcars, na = 4, select = "carb")
  )
  expect_equal(
    convert_to_na(mtcars, na = 4, select = "arb$", regex = TRUE),
    convert_to_na(mtcars, na = 4, select = "carb")
  )
})
