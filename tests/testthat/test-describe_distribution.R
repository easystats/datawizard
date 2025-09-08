skip_if_not_installed("bayestestR")

# numeric ---------------------------------------

test_that("describe_distribution - numeric: works with basic numeric vector", {
  x <- describe_distribution(mtcars$mpg)
  expect_identical(dim(x), c(1L, 9L))
  expect_identical(round(x$Mean), 20)
})

test_that("describe_distribution - numeric: correctly handles missing values", {
  no_missing <- describe_distribution(mtcars$mpg)
  test <- mtcars$mpg
  test[1] <- NA
  with_missing <- describe_distribution(test)
  expect_identical(with_missing$n, 31L)
  expect_identical(with_missing$n_Missing, 1L)
  expect_false(with_missing$Mean == no_missing$Mean)
})

test_that("describe_distribution - numeric: works with quartiles", {
  x <- describe_distribution(mtcars$mpg, quartiles = TRUE)
  expect_identical(dim(x), c(1L, 11L))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - numeric: works with range", {
  x <- describe_distribution(mtcars$mpg, range = FALSE)
  expect_identical(dim(x), c(1L, 7L))
  expect_false("min" %in% names(x))
  expect_false("max" %in% names(x))
})

test_that("describe_distribution - NULL for date", {
  v <- as.Date(c("2022-01-01", "2022-01-02"))
  expect_warning(expect_null(describe_distribution(v)))
})


# data frame ---------------------------------------

test_that("describe_distribution - data frame: works with basic data frame", {
  x <- describe_distribution(mtcars)
  expect_identical(dim(x), c(11L, 10L))
  expect_identical(round(x[1, "Mean"]), 20)
})

test_that("describe_distribution - data frame: correctly handles missing values", {
  no_missing <- describe_distribution(mtcars)
  test <- mtcars
  test[1, ] <- NA
  with_missing <- describe_distribution(test)
  expect_identical(unique(with_missing$n), 31L)
  expect_identical(unique(with_missing$n_Missing), 1L)
  expect_false(unique(with_missing$Mean == no_missing$Mean))
})

test_that("describe_distribution - data frame: works with quartiles", {
  x <- describe_distribution(mtcars, quartiles = TRUE)
  expect_identical(dim(x), c(11L, 12L))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - data frame: works with range", {
  x <- describe_distribution(mtcars, range = FALSE)
  expect_identical(dim(x), c(11L, 8L))
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

  expect_identical(stored$Variable, c("Var_1", "Var_2"))
  expect_identical(unnamed$Variable, c("mtcars$mpg", "mtcars$cyl"))
  expect_identical(named$Variable, c("foo", "foo2"))
  expect_identical(mix$Variable, c("foo", "mtcars$cyl"))
})

test_that("describe_distribution - list: correctly handles missing values", {
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
  x <- describe_distribution(list(mtcars$mpg, mtcars$cyl), quartiles = TRUE)
  expect_identical(dim(x), c(2L, 12L))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - list: works with range", {
  x <- describe_distribution(list(mtcars$mpg, mtcars$cyl), range = FALSE)
  expect_identical(dim(x), c(2L, 8L))
  expect_false("min" %in% names(x))
  expect_false("max" %in% names(x))
})


# select ----------------------

test_that("describe_distribution - select", {
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
  data(iris)
  x <- data_group(iris, Species)
  out <- describe_distribution(x, select = starts_with("Petal"))

  expect_snapshot(out)
  expect_equal(out$Mean, c(1.462, 0.246, 4.26, 1.326, 5.552, 2.026), tolerance = 1e-3)
})

# Mostly to test printing
test_that("describe_distribution - grouped df and multiple groups", {
  x <- data.frame(
    grp1 = rep(letters[1:3], each = 20),
    grp2 = rep(letters[1:3], 20),
    values = 1:30
  )
  x <- data_group(x, c("grp1", "grp2"))
  expect_snapshot(describe_distribution(x))
})

test_that("argument 'by' works", {
  # basic
  grouped <- data_group(mtcars, c("am", "vs"))
  expect_identical(
    describe_distribution(grouped),
    describe_distribution(mtcars, by = c("am", "vs")),
    ignore_attr = TRUE
  )

  # mixing data_group() and arg 'by'
  grouped <- data_group(mtcars, c("am", "vs"))
  half_grouped <- data_group(mtcars, "am")
  expect_identical(
    describe_distribution(grouped),
    describe_distribution(half_grouped, by = "vs"),
    ignore_attr = TRUE
  )

  expect_error(
    describe_distribution(mtcars, by = 2),
    "must be a character vector"
  )
})

test_that("empty groups are discarded from the output, #608", {
  dat <- data.frame(
    grp1 = factor("a", levels = c("a", "b")),
    grp2 = factor(c("A", "B")),
    value = 1:2
  )
  dat <- data_group(dat, c("grp1", "grp2"))
  expect_no_error(
    suppressWarnings(describe_distribution(dat, ci = 0.95))
  )
})

# distribution_mode --------------------------
test_that("distribution_mode works as expected", {
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
  expect_equal(
    describe_distribution(mtcars, select = "pg", regex = TRUE),
    describe_distribution(mtcars, select = "mpg"),
    ignore_attr = TRUE
  )
})

# formatting ------------------------------
test_that("describe_distribution formatting", {
  data(iris)
  x <- describe_distribution(iris$Sepal.Width, quartiles = TRUE)
  expect_snapshot(format(x))
})

# other -----------------------------------
test_that("return NA in CI if sample is too sparse", {
  set.seed(123456)
  expect_silent(expect_message(
    {
      res <- describe_distribution(mtcars[mtcars$cyl == "6", ], wt, centrality = "map", ci = 0.95)
    },
    regex = "Bootstrapping"
  ))
  expect_equal(res$CI_low_map, 2.6462, tolerance = 1e-2)
  expect_equal(res$CI_high_map, 3.4531, tolerance = 1e-2)

  x <- c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.2, 2.2, 2.2, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5)
  expect_message(
    {
      out <- describe_distribution(x, centrality = "map")
    },
    regex = "Could not calculate"
  )
  expect_identical(out$MAP, NA_real_)
  expect_silent(describe_distribution(x, centrality = "map", verbose = FALSE))
})

# check for reserved column names
test_that("errors on invalid column names (reserved word)", {
  data(mtcars)

  out <- data_to_long(mtcars, cols = 1:3, names_to = "Variable", values_to = "Values")
  out <- data_group(out, c("gear", "Variable"))
  expect_error(
    describe_distribution(out, select = "Values"),
    regex = "Following variable names are reserved"
  )

  out <- data_to_long(mtcars, cols = 1:3, names_to = "Variable", values_to = "Values")
  expect_error(
    describe_distribution(out, select = "Variable"),
    regex = "Following variable names are reserved"
  )
})

# multiple centralities
test_that("multiple centralities work", {
  data(iris)
  out <- describe_distribution(
    iris,
    select = "Petal.Width",
    centrality = c("median", "mean")
  )
  expect_identical(dim(out), c(1L, 12L))
  expect_named(
    out,
    c(
      "Variable", "Median", "MAD", "Mean", "SD", "IQR", "Min", "Max",
      "Skewness", "Kurtosis", "n", "n_Missing"
    )
  )
  out <- describe_distribution(
    iris,
    select = "Petal.Width",
    centrality = list("median", "mean")
  )
  expect_identical(dim(out), c(1L, 12L))
  expect_named(
    out,
    c(
      "Variable", "Median", "MAD", "Mean", "SD", "IQR", "Min", "Max",
      "Skewness", "Kurtosis", "n", "n_Missing"
    )
  )
})


test_that("(multiple) centralities with CIs", {
  data(iris)
  x <- iris$Sepal.Width
  set.seed(123456)
  expect_message(
    {
      out <- describe_distribution(x, centrality = "all", ci = 0.95, iterations = 100)
    },
    regex = "For more stable intervals"
  )
  expect_named(
    out,
    c(
      "Median", "MAD", "Mean", "SD", "MAP", "IQR", "CI_low_mean",
      "CI_high_mean", "CI_low_median", "CI_high_median", "CI_low_MAP",
      "CI_high_MAP", "Min", "Max", "Skewness", "Kurtosis", "n", "n_Missing"
    )
  )
  expect_snapshot(print(out, table_width = Inf))
  expect_silent(describe_distribution(x, centrality = "all", ci = 0.95, iterations = 100, verbose = FALSE))

  set.seed(123456)
  out <- describe_distribution(x, centrality = "mean", ci = 0.95, iterations = 100, verbose = FALSE)
  expect_named(
    out,
    c(
      "Mean", "SD", "IQR", "CI_low_mean", "CI_high_mean", "Min",
      "Max", "Skewness", "Kurtosis", "n", "n_Missing"
    )
  )
  expect_snapshot(print(out, table_width = Inf))

  set.seed(123456)
  out <- describe_distribution(x, centrality = c("MAP", "median"), ci = 0.95, iterations = 100, verbose = FALSE)
  expect_named(
    out,
    c(
      "Median", "MAD", "MAP", "IQR", "CI_low_MAP", "CI_high_MAP",
      "CI_low_median", "CI_high_median", "Min", "Max", "Skewness",
      "Kurtosis", "n", "n_Missing"
    )
  )
  expect_snapshot(print(out, table_width = Inf))

  # only one message for data frame
  expect_silent(expect_message(describe_distribution(iris, ci = 0.95)))
})


test_that("display() method exports to markdown", {
  skip_if_not_installed("knitr")
  data(iris)
  out <- describe_distribution(iris)
  expect_error(display(out, format = "invalid"), regex = "Invalid option")
  expect_snapshot(display(out))
})


test_that("display() method exports to tinytable", {
  skip_if_not_installed("tinytable")
  data(iris)
  out <- describe_distribution(iris)
  expect_snapshot(display(out, format = "tt"))
})
