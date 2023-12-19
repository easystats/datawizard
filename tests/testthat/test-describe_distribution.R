# numeric ---------------------------------------

test_that("describe_distribution - numeric: works with basic numeric vector", {
  skip_if_not_installed("bayestestR")

  x <- describe_distribution(mtcars$mpg)
  expect_identical(dim(x), c(1L, 9L))
  expect_identical(round(x$Mean), 20)
})

test_that("describe_distribution - numeric: correctly handles missing values", {
  skip_if_not_installed("bayestestR")

  no_missing <- describe_distribution(mtcars$mpg)
  test <- mtcars$mpg
  test[1] <- NA
  with_missing <- describe_distribution(test)
  expect_identical(with_missing$n, 31L)
  expect_identical(with_missing$n_Missing, 1L)
  expect_false(with_missing$Mean == no_missing$Mean)
})

test_that("describe_distribution - numeric: works with quartiles", {
  skip_if_not_installed("bayestestR")

  x <- describe_distribution(mtcars$mpg, quartiles = TRUE)
  expect_identical(dim(x), c(1L, 11L))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - numeric: works with range", {
  skip_if_not_installed("bayestestR")

  x <- describe_distribution(mtcars$mpg, range = FALSE)
  expect_identical(dim(x), c(1L, 7L))
  expect_false("min" %in% names(x))
  expect_false("max" %in% names(x))
})

test_that("describe_distribution - NULL for date", {
  skip_if_not_installed("bayestestR")

  v <- as.Date(c("2022-01-01", "2022-01-02"))
  expect_warning(expect_null(describe_distribution(v)))
})



# data frame ---------------------------------------

test_that("describe_distribution - data frame: works with basic data frame", {
  skip_if_not_installed("bayestestR")

  x <- describe_distribution(mtcars)
  expect_identical(dim(x), c(11L, 10L))
  expect_identical(round(x[1, "Mean"]), 20)
})

test_that("describe_distribution - data frame: correctly handles missing values", {
  skip_if_not_installed("bayestestR")

  no_missing <- describe_distribution(mtcars)
  test <- mtcars
  test[1, ] <- NA
  with_missing <- describe_distribution(test)
  expect_identical(unique(with_missing$n), 31L)
  expect_identical(unique(with_missing$n_Missing), 1L)
  expect_false(unique(with_missing$Mean == no_missing$Mean))
})

test_that("describe_distribution - data frame: works with quartiles", {
  skip_if_not_installed("bayestestR")

  x <- describe_distribution(mtcars, quartiles = TRUE)
  expect_identical(dim(x), c(11L, 12L))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - data frame: works with range", {
  skip_if_not_installed("bayestestR")

  x <- describe_distribution(mtcars, range = FALSE)
  expect_identical(dim(x), c(11L, 8L))
  expect_false("min" %in% names(x))
  expect_false("max" %in% names(x))
})



# factor ---------------------------------------

test_that("describe_distribution - factor", {
  skip_if_not_installed("bayestestR")

  expect_snapshot(describe_distribution(factor(substring("statistics", 1:10, 1:10))))
})



# character ---------------------------------------

test_that("describe_distribution - character", {
  skip_if_not_installed("bayestestR")

  expect_snapshot(describe_distribution(as.character(ToothGrowth$supp)))
})



# list ---------------------------------------

test_that("describe_distribution - list: works with basic list", {
  skip_if_not_installed("bayestestR")

  x <- list(mtcars$mpg, mtcars$cyl)
  stored <- describe_distribution(x)
  unnamed <- describe_distribution(list(mtcars$mpg, mtcars$cyl))
  named <- describe_distribution(list(foo = mtcars$mpg, foo2 = mtcars$cyl))
  mix <- describe_distribution(list(foo = mtcars$mpg, mtcars$cyl))

  expect_identical(dim(stored), c(2L, 10L))
  expect_identical(round(stored$Mean), c(20, 6))
  expect_identical(dim(unnamed), c(2L, 10L))
  expect_identical(round(unnamed$Mean), c(20, 6))
  expect_identical(dim(named), c(2L, 10L))
  expect_identical(round(named$Mean), c(20, 6))
  expect_identical(dim(mix), c(2L, 10L))
  expect_identical(round(mix$Mean), c(20, 6))
})

test_that("describe_distribution - list: works with include_factors", {
  skip_if_not_installed("bayestestR")

  x1 <- describe_distribution(list(mtcars$mpg, factor(mtcars$cyl)))
  y <- describe_distribution(list(mtcars$mpg))
  expect_identical(x1, y)

  x2 <- describe_distribution(list(mtcars$mpg, factor(mtcars$cyl)),
    include_factors = TRUE
  )
  expect_identical(dim(x2), c(2L, 10L))
  expect_identical(x2$Variable, c("mtcars$mpg", "factor(mtcars$cyl)"))

  x3 <- describe_distribution(list(mtcars$mpg, foo = factor(mtcars$cyl)),
    include_factors = TRUE
  )
  expect_identical(dim(x3), c(2L, 10L))
  expect_identical(x3$Variable, c("mtcars$mpg", "foo"))
})

test_that("describe_distribution - list: correctly removes character elements", {
  skip_if_not_installed("bayestestR")

  x <- describe_distribution(list(mtcars$mpg, "something"))
  y <- describe_distribution(list(mtcars$mpg))
  expect_identical(x, y)
})

test_that("describe_distribution - list: correctly handles variable names", {
  skip_if_not_installed("bayestestR")

  x <- list(mtcars$mpg, mtcars$cyl)
  stored <- describe_distribution(x)
  unnamed <- describe_distribution(list(mtcars$mpg, mtcars$cyl))
  named <- describe_distribution(list(foo = mtcars$mpg, foo2 = mtcars$cyl))
  mix <- describe_distribution(list(foo = mtcars$mpg, mtcars$cyl))

  expect_identical(stored$Variable, c("Var_1", "Var_2"))
  expect_identical(unnamed$Variable, c("mtcars$mpg", "mtcars$cyl"))
  expect_identical(named$Variable, c("foo", "foo2"))
  expect_identical(mix$Variable, c("foo", "mtcars$cyl"))
})

test_that("describe_distribution - list: correctly handles missing values", {
  skip_if_not_installed("bayestestR")

  no_missing <- describe_distribution(list(mtcars$mpg, mtcars$cyl))
  test <- mtcars$mpg
  test2 <- mtcars$cyl
  test[1] <- NA
  test2[1] <- NA
  with_missing <- describe_distribution(list(test, test2))
  expect_identical(unique(with_missing$n), 31L)
  expect_identical(unique(with_missing$n_Missing), 1L)
  expect_false(unique(with_missing$Mean == no_missing$Mean))
})

test_that("describe_distribution - list: works with quartiles", {
  skip_if_not_installed("bayestestR")

  x <- describe_distribution(list(mtcars$mpg, mtcars$cyl), quartiles = TRUE)
  expect_identical(dim(x), c(2L, 12L))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - list: works with range", {
  skip_if_not_installed("bayestestR")

  x <- describe_distribution(list(mtcars$mpg, mtcars$cyl), range = FALSE)
  expect_identical(dim(x), c(2L, 8L))
  expect_false("min" %in% names(x))
  expect_false("max" %in% names(x))
})



# select ----------------------

test_that("describe_distribution - select", {
  skip_if_not_installed("bayestestR")

  data(iris)
  out <- describe_distribution(iris, select = starts_with("Petal"))

  expect_identical(out$Variable, c("Petal.Length", "Petal.Width"))
  expect_equal(out$Mean, c(3.758000, 1.199333), tolerance = 1e-3)

  expect_null(describe_distribution(iris, select = "Species"))
  out <- describe_distribution(iris, select = "Species", include_factors = TRUE)
  exp <- describe_distribution(iris$Species)
  expect_identical(out$Range, exp$Range)
})



# select and grouped df ----------------------

test_that("describe_distribution - grouped df", {
  skip_if_not_installed("bayestestR")

  data(iris)
  x <- data_group(iris, Species)
  out <- describe_distribution(x, select = starts_with("Petal"))

  expect_identical(out$.group, c(
    "Species=setosa", "Species=setosa",
    "Species=versicolor", "Species=versicolor",
    "Species=virginica", "Species=virginica"
  ))
  expect_identical(out$Variable, c(
    "Petal.Length", "Petal.Width",
    "Petal.Length", "Petal.Width",
    "Petal.Length", "Petal.Width"
  ))
  expect_equal(out$Mean, c(1.462, 0.246, 4.26, 1.326, 5.552, 2.026), tolerance = 1e-3)
})


# distribution_mode --------------------------
test_that("distribution_mode works as expected", {
  skip_if_not_installed("bayestestR")

  # atomic vector
  expect_identical(distribution_mode(c(1, 2, 3, 3, 4, 5)), 3)
  expect_identical(distribution_mode(c(1, 2, 3, 3, 4, 4, 5)), 3)
  expect_identical(distribution_mode(c(1.5, 2.3, 3.7, 3.7, 4.0, 5)), 3.7)

  # list
  expect_identical(distribution_mode(list(1, 2, 3, 3, 4, 5)), list(3))

  # scalar
  expect_identical(distribution_mode("a"), "a")

  # empty
  expect_null(distribution_mode(NULL))
})

# select helpers ------------------------------
test_that("describe_distribution regex", {
  skip_if_not_installed("bayestestR")

  expect_equal(
    describe_distribution(mtcars, select = "pg", regex = TRUE),
    describe_distribution(mtcars, select = "mpg"),
    ignore_attr = TRUE
  )
})

# formatting ------------------------------
test_that("describe_distribution formatting", {
  skip_if_not_installed("bayestestR")
  data(iris)
  x <- describe_distribution(iris$Sepal.Width, quartiles = TRUE)
  expect_snapshot(format(x))
})
