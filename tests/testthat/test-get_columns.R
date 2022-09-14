# input check ---------------------

test_that("get_columns checks for data frame", {
  expect_error(get_columns(NULL), regexp = "provided")
  x <- list(a = 1:2, b = letters[1:3])
  expect_error(get_columns(x), regexp = "coerced")
})



# select helpers ---------------------

test_that("get_columns works with select helpers", {
  expect_equal(
    get_columns(iris, starts_with("Sepal")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  expect_equal(
    get_columns(iris, ends_with("Width")),
    iris[c("Sepal.Width", "Petal.Width")]
  )

  expect_equal(
    get_columns(iris, col_ends_with("Width")),
    iris[c("Sepal.Width", "Petal.Width")]
  )

  expect_equal(
    get_columns(iris, regex("\\.")),
    iris[c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
  )

  expect_equal(
    get_columns(iris, contains("Wid")),
    iris[c("Sepal.Width", "Petal.Width")]
  )
})



# select helpers, negation ---------------------

test_that("get_columns works with negation of select helpers", {
  expect_equal(
    get_columns(iris, -starts_with("Sepal")),
    iris[c("Petal.Length", "Petal.Width", "Species")]
  )

  expect_equal(
    get_columns(iris, -ends_with("Width")),
    iris[c("Sepal.Length", "Petal.Length", "Species")]
  )

  expect_equal(
    get_columns(iris, -col_ends_with("Width")),
    iris[c("Sepal.Length", "Petal.Length", "Species")]
  )
})



# select-nse with function  ---------------------

test_that("get_columns works with select-functions", {
  expect_equal(
    get_columns(iris, is.numeric()),
    iris[sapply(iris, is.numeric)]
  )

  expect_equal(
    get_columns(iris, is.numeric),
    iris[sapply(iris, is.numeric)]
  )

  expect_equal(
    get_columns(iris, is.factor()),
    iris[sapply(iris, is.factor)]
  )

  expect_equal(
    get_columns(iris, is.factor),
    iris[sapply(iris, is.factor)]
  )

  expect_warning(expect_null(get_columns(iris, is.logical())))
})



# select-nse with user-function  ---------------------

test_that("get_columns works with user-defined select-functions", {
  testfun <<- function(i) {
    is.numeric(i) && mean(i, na.rm = TRUE) > 3.5
  }
  expect_equal(get_columns(iris, testfun), iris[sapply(iris, testfun)])
  expect_equal(get_columns(iris, -testfun), iris[!sapply(iris, testfun)])

  testfun2 <<- function(i) {
    is.numeric(i) && mean(i, na.rm = TRUE) < 5
  }
  expect_equal(
    get_columns(iris, select = testfun, exclude = testfun2),
    iris["Sepal.Length"]
  )
  expect_equal(
    get_columns(iris, select = testfun, exclude = -testfun2),
    iris["Petal.Length"]
  )
})



# select-nse with negation of functions  ---------------------

test_that("get_columns works with negated select-functions", {
  expect_equal(
    get_columns(iris, -is.numeric()),
    iris[sapply(iris, function(i) !is.numeric(i))]
  )

  expect_equal(
    get_columns(iris, -is.numeric),
    iris[sapply(iris, function(i) !is.numeric(i))]
  )

  expect_equal(
    get_columns(iris, -is.factor()),
    iris[sapply(iris, function(i) !is.factor(i))]
  )

  expect_equal(
    get_columns(iris, -is.factor),
    iris[sapply(iris, function(i) !is.factor(i))]
  )

  expect_equal(get_columns(iris, -is.logical), iris)
})



# select-nse with ranges  ---------------------

test_that("get_columns works with ranges", {
  expect_equal(
    get_columns(iris, 2:3),
    iris[2:3]
  )

  expect_equal(
    get_columns(iris, Sepal.Width:Petal.Length),
    iris[2:3]
  )
})



# select-nse with negated ranges  ---------------------

test_that("get_columns works with negated ranges", {
  expect_equal(
    get_columns(iris, -(1:2)),
    iris[c(3, 4, 5)]
  )

  expect_equal(
    get_columns(iris, -1:-2),
    iris[c(3, 4, 5)]
  )

  expect_equal(
    get_columns(iris, exclude = -1:-2),
    iris[1:2]
  )

  expect_equal(
    get_columns(iris, exclude = 2:3),
    iris[c(1, 4, 5)]
  )

  expect_equal(
    get_columns(iris, -Sepal.Width:Petal.Length),
    iris[c(1, 4, 5)]
  )
})



# select-nse with formulas  ---------------------

test_that("get_columns works with formulas", {
  expect_equal(
    get_columns(iris, ~ Sepal.Width + Petal.Length),
    iris[2:3]
  )

  expect_equal(
    get_columns(iris, exclude = ~ Sepal.Width + Petal.Length),
    iris[c(1, 4, 5)]
  )
})



# select-nse, other cases ---------------------

test_that("get_columns works, other cases", {
  expect_equal(get_columns(iris), iris)

  expect_equal(
    get_columns(iris, c("Petal.Width", "Sepal.Length")),
    iris[c("Petal.Width", "Sepal.Length")]
  )

  expect_equal(
    get_columns(iris, -c("Petal.Width", "Sepal.Length")),
    iris[setdiff(colnames(iris), c("Petal.Width", "Sepal.Length"))]
  )

  expect_equal(
    get_columns(iris, -Petal.Width),
    iris[setdiff(colnames(iris), "Petal.Width")]
  )

  expect_equal(
    get_columns(mtcars, c("am", "gear", "cyl")),
    mtcars[c("am", "gear", "cyl")]
  )

  expect_equal(
    get_columns(mtcars, c("vam", "gear", "cyl")),
    mtcars[c("gear", "cyl")]
  )

  expect_warning(expect_null(get_columns(mtcars, ends_with("abc"))))

  expect_equal(
    get_columns(mtcars, regex("rb$")),
    mtcars["carb"]
  )

  expect_equal(
    get_columns(mtcars, regex("^c")),
    mtcars[c("cyl", "carb")]
  )

  expect_warning(expect_null(get_columns(mtcars, "^c")))

  expect_equal(
    get_columns(mtcars, regex("^C"), ignore_case = TRUE),
    mtcars[c("cyl", "carb")]
  )
})



# select-nse works when called from other function  ---------------------

test_that("get_columns from other functions", {
  test_fun1 <- function(data, i) {
    get_columns(data, select = i)
  }
  expect_equal(
    test_fun1(iris, c("Sepal.Length", "Sepal.Width")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  # This turns out to be that the variable name "i" in test_fun1
  # becomes the search string "i" after evaluation, so all columns
  # containing an "i" will be returned.

  expect_equal(
    test_fun1(iris, starts_with("Sep")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  test_fun1a <- function(data, i) {
    get_columns(data, select = i, regex = TRUE)
  }
  expect_equal(
    test_fun1a(iris, "Sep"),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  test_fun1b <- function(data, i) {
    get_columns(data, select = i, regex = TRUE)
  }
  expect_equal(
    test_fun1b(iris, "Width$"),
    iris[c("Sepal.Width", "Petal.Width")]
  )

  test_fun1c <- function(data, i) {
    get_columns(data, select = -i)
  }
  expect_equal(
    test_fun1c(iris, c("Sepal.Length", "Sepal.Width")),
    iris[c("Petal.Length", "Petal.Width", "Species")]
  )


  test_fun2 <- function(data) {
    get_columns(data, select = starts_with("Sep"))
  }
  expect_equal(
    test_fun2(iris),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  test_fun3 <- function(data) {
    i <- "Sep"
    get_columns(data, select = starts_with(i))
  }
  expect_equal(
    test_fun3(iris),
    iris[, c("Sepal.Length", "Sepal.Width")]
  )

  test_top <- function(x) {
    testfun1 <- function(i) {
      is.numeric(i) && mean(i, na.rm = TRUE) > 3.5
    }
    testfun2 <- function(i) {
      is.numeric(i) && mean(i, na.rm = TRUE) < 5
    }
    get_columns(x, select = testfun, exclude = -testfun2)
  }
  expect_equal(test_top(iris), iris["Petal.Length"])
})



# preserve attributes --------------------------

test_that("get_columns preserves attributes", {
  skip_if_not_installed("parameters")

  m <- lm(Sepal.Length ~ Species, data = iris)
  out <- parameters::parameters(m)
  a1 <- attributes(out)

  out2 <- get_columns(out, 1:3)
  a2 <- attributes(out2)

  expect_equal(names(a1), names(a2))
})

# Select helpers work in functions and loops

test_that("select helpers work in functions and loops", {
  foo <- function(data, i) {
    find_columns(data, select = starts_with(i))
  }
  expect_equal(
    foo(iris, "Sep"),
    c("Sepal.Length", "Sepal.Width")
  )

  for (i in "Sepal") {
    x <- find_columns(iris, select = starts_with(i))
  }
  expect_equal(
    x,
    c("Sepal.Length", "Sepal.Width")
  )

  for (i in "Length") {
    x <- find_columns(iris, select = ends_with(i))
  }
  expect_equal(
    x,
    c("Sepal.Length", "Petal.Length")
  )
})

test_that("select helpers work in functions and loops even if there's an object with the same name in the environment above", {
  i <- "Petal"
  foo <- function(data, i) {
    find_columns(data, select = starts_with(i))
  }
  expect_equal(
    foo(iris, "Sep"),
    c("Sepal.Length", "Sepal.Width")
  )

  for (i in "Sepal") {
    x <- find_columns(iris, select = starts_with(i))
  }
  expect_equal(
    x,
    c("Sepal.Length", "Sepal.Width")
  )

  i <- "Width"

  for (i in "Length") {
    x <- find_columns(iris, select = ends_with(i))
  }
  expect_equal(
    x,
    c("Sepal.Length", "Petal.Length")
  )
})

test_that("old solution still works", {
  foo <- function(data) {
    i <- "Sep"
    find_columns(data, select = i, regex = TRUE)
  }
  expect_equal(
    foo(iris),
    c("Sepal.Length", "Sepal.Width")
  )
})
