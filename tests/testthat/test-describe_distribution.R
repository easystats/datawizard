# numeric ---------------------------------------

test_that("describe_distribution - numeric: works with basic numeric vector", {
  x <- describe_distribution(mtcars$mpg)
  expect_equal(dim(x), c(1, 9))
  expect_equal(round(x$Mean), 20)
})

test_that("describe_distribution - numeric: correctly handles missing values", {
  no_missing <- describe_distribution(mtcars$mpg)
  test <- mtcars$mpg
  test[1] <- NA
  with_missing <- describe_distribution(test)
  expect_equal(with_missing$n, 31)
  expect_equal(with_missing$n_Missing, 1)
  expect_false(with_missing$Mean == no_missing$Mean)
})

test_that("describe_distribution - numeric: works with quartiles", {
  x <- describe_distribution(mtcars$mpg, quartiles = TRUE)
  expect_equal(dim(x), c(1, 11))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - numeric: works with range", {
  x <- describe_distribution(mtcars$mpg, range = FALSE)
  expect_equal(dim(x), c(1, 7))
  expect_false("min" %in% names(x))
  expect_false("max" %in% names(x))
})

test_that("describe_distribution - NULL for date", {
  v <- as.Date(c("2022-01-01", "2022-01-02"))
  expect_warning(expect_null(describe_distribution(v)))
})



# dataframe ---------------------------------------

test_that("describe_distribution - dataframe: works with basic dataframe", {
  x <- describe_distribution(mtcars)
  expect_equal(dim(x), c(11, 10))
  expect_equal(round(x[1, "Mean"]), 20)
})

test_that("describe_distribution - dataframe: correctly handles missing values", {
  no_missing <- describe_distribution(mtcars)
  test <- mtcars
  test[1, ] <- NA
  with_missing <- describe_distribution(test)
  expect_equal(unique(with_missing$n), 31)
  expect_equal(unique(with_missing$n_Missing), 1)
  expect_false(unique(with_missing$Mean == no_missing$Mean))
})

test_that("describe_distribution - dataframe: works with quartiles", {
  x <- describe_distribution(mtcars, quartiles = TRUE)
  expect_equal(dim(x), c(11, 12))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - dataframe: works with range", {
  x <- describe_distribution(mtcars, range = FALSE)
  expect_equal(dim(x), c(11, 8))
  expect_false("min" %in% names(x))
  expect_false("max" %in% names(x))
})



# factor ---------------------------------------

test_that("describe_distribution - factor", {
  expect_snapshot(describe_distribution(factor(substring("statistics", 1:10, 1:10))))
})



# character ---------------------------------------

test_that("describe_distribution - character", {
  expect_snapshot(describe_distribution(as.character(ToothGrowth$supp)))
})



# list ---------------------------------------

test_that("describe_distribution - list: works with basic list", {
  x <- list(mtcars$mpg, mtcars$cyl)
  stored <- describe_distribution(x)
  unnamed <- describe_distribution(list(mtcars$mpg, mtcars$cyl))
  named <- describe_distribution(list(foo = mtcars$mpg, foo2 = mtcars$cyl))
  mix <- describe_distribution(list(foo = mtcars$mpg, mtcars$cyl))

  expect_equal(dim(stored), c(2, 10))
  expect_equal(round(stored$Mean), c(20, 6))
  expect_equal(dim(unnamed), c(2, 10))
  expect_equal(round(unnamed$Mean), c(20, 6))
  expect_equal(dim(named), c(2, 10))
  expect_equal(round(named$Mean), c(20, 6))
  expect_equal(dim(mix), c(2, 10))
  expect_equal(round(mix$Mean), c(20, 6))
})

test_that("describe_distribution - list: works with include_factors", {
  x1 <- describe_distribution(list(mtcars$mpg, factor(mtcars$cyl)))
  y <- describe_distribution(list(mtcars$mpg))
  expect_identical(x1, y)

  x2 <- describe_distribution(list(mtcars$mpg, factor(mtcars$cyl)),
    include_factors = TRUE
  )
  expect_equal(dim(x2), c(2, 10))
  expect_equal(x2$Variable, c("mtcars$mpg", "factor(mtcars$cyl)"))

  x3 <- describe_distribution(list(mtcars$mpg, foo = factor(mtcars$cyl)),
    include_factors = TRUE
  )
  expect_equal(dim(x3), c(2, 10))
  expect_equal(x3$Variable, c("mtcars$mpg", "foo"))
})

test_that("describe_distribution - list: correctly removes character elements", {
  x <- describe_distribution(list(mtcars$mpg, "something"))
  y <- describe_distribution(list(mtcars$mpg))
  expect_identical(x, y)
})

test_that("describe_distribution - list: correctly handles variable names", {
  x <- list(mtcars$mpg, mtcars$cyl)
  stored <- describe_distribution(x)
  unnamed <- describe_distribution(list(mtcars$mpg, mtcars$cyl))
  named <- describe_distribution(list(foo = mtcars$mpg, foo2 = mtcars$cyl))
  mix <- describe_distribution(list(foo = mtcars$mpg, mtcars$cyl))

  expect_equal(stored$Variable, c("Var_1", "Var_2"))
  expect_equal(unnamed$Variable, c("mtcars$mpg", "mtcars$cyl"))
  expect_equal(named$Variable, c("foo", "foo2"))
  expect_equal(mix$Variable, c("foo", "mtcars$cyl"))
})

test_that("describe_distribution - list: correctly handles missing values", {
  no_missing <- describe_distribution(list(mtcars$mpg, mtcars$cyl))
  test <- mtcars$mpg
  test2 <- mtcars$cyl
  test[1] <- NA
  test2[1] <- NA
  with_missing <- describe_distribution(list(test, test2))
  expect_equal(unique(with_missing$n), 31)
  expect_equal(unique(with_missing$n_Missing), 1)
  expect_false(unique(with_missing$Mean == no_missing$Mean))
})

test_that("describe_distribution - list: works with quartiles", {
  x <- describe_distribution(list(mtcars$mpg, mtcars$cyl), quartiles = TRUE)
  expect_equal(dim(x), c(2, 12))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - list: works with range", {
  x <- describe_distribution(list(mtcars$mpg, mtcars$cyl), range = FALSE)
  expect_equal(dim(x), c(2, 8))
  expect_false("min" %in% names(x))
  expect_false("max" %in% names(x))
})



# select ----------------------

test_that("describe_distribution - select", {
  data(iris)
  out <- describe_distribution(iris, select = starts_with("Petal"))

  expect_equal(out$Variable, c("Petal.Length", "Petal.Width"))
  expect_equal(out$Mean, c(3.758000, 1.199333), tolerance = 1e-3)
})



# select and grouped df ----------------------

test_that("describe_distribution - grouped df", {
  data(iris)
  x <- data_group(iris, Species)
  out <- describe_distribution(x, select = starts_with("Petal"))

  expect_equal(out$.group, c(
    "Species=setosa", "Species=setosa",
    "Species=versicolor", "Species=versicolor",
    "Species=virginica", "Species=virginica"
  ))
  expect_equal(out$Variable, c(
    "Petal.Length", "Petal.Width",
    "Petal.Length", "Petal.Width",
    "Petal.Length", "Petal.Width"
  ))
  expect_equal(out$Mean, c(1.462, 0.246, 4.26, 1.326, 5.552, 2.026), tolerance = 1e-3)
})


# distribution_mode --------------------------

test_that("distribution_mode works as expected", {
  # atomic vector
  expect_equal(distribution_mode(c(1, 2, 3, 3, 4, 5)), 3)
  expect_equal(distribution_mode(c(1, 2, 3, 3, 4, 4, 5)), 3)
  expect_equal(distribution_mode(c(1.5, 2.3, 3.7, 3.7, 4.0, 5)), 3.7)

  # list
  expect_equal(distribution_mode(list(1, 2, 3, 3, 4, 5)), list(3))

  # scalar
  expect_equal(distribution_mode("a"), "a")

  # empty
  expect_null(distribution_mode(c()))
})
