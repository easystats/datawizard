# input check ---------------------

test_that("data_select checks for data frame", {
  expect_error(data_select(NULL), regexp = "provided")
  x <- list(a = 1:2, b = letters[1:3])
  expect_error(data_select(x), regexp = "coerced")
})


# select helpers ---------------------

test_that("data_select works with select helpers", {
  expect_identical(
    data_select(iris, starts_with("Sepal")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  expect_identical(
    data_select(iris, ends_with("Width")),
    iris[c("Sepal.Width", "Petal.Width")]
  )

  expect_identical(
    data_select(iris, regex("\\.")),
    iris[c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
  )

  expect_identical(
    data_select(iris, contains("Wid")),
    iris[c("Sepal.Width", "Petal.Width")]
  )
})


# select helpers, negation ---------------------

test_that("data_select works with negation of select helpers", {
  expect_identical(
    data_select(iris, -starts_with("Sepal")),
    iris[c("Petal.Length", "Petal.Width", "Species")]
  )

  expect_identical(
    data_select(iris, -ends_with("Width")),
    iris[c("Sepal.Length", "Petal.Length", "Species")]
  )
})


# select-nse with function  ---------------------

test_that("data_select works with select-functions", {
  expect_identical(
    data_select(iris, is.numeric()),
    iris[sapply(iris, is.numeric)]
  )

  expect_identical(
    data_select(iris, is.numeric),
    iris[sapply(iris, is.numeric)]
  )

  expect_identical(
    data_select(iris, is.factor()),
    iris[sapply(iris, is.factor)]
  )

  expect_identical(
    data_select(iris, is.factor),
    iris[sapply(iris, is.factor)]
  )

  expect_warning(expect_null(data_select(iris, is.logical())))
})


# select-nse with user-function  ---------------------
testfun <- function(i) {
  is.numeric(i) && mean(i, na.rm = TRUE) > 3.5
}
test_that("data_select works with user-defined select-functions", {
  expect_identical(data_select(iris, testfun), iris[sapply(iris, testfun)])
  expect_identical(data_select(iris, -testfun), iris[!sapply(iris, testfun)])

  testfun2 <- function(i) {
    is.numeric(i) && mean(i, na.rm = TRUE) < 5
  }
  expect_identical(
    data_select(iris, select = testfun, exclude = testfun2),
    iris["Sepal.Length"]
  )
  expect_identical(
    data_select(iris, select = testfun, exclude = -testfun2),
    iris["Petal.Length"]
  )
})


# select-nse with negation of functions  ---------------------

test_that("data_select works with negated select-functions", {
  expect_identical(
    data_select(iris, -is.numeric()),
    iris[sapply(iris, function(i) !is.numeric(i))] # nolint
  )

  expect_identical(
    data_select(iris, -is.numeric),
    iris[sapply(iris, function(i) !is.numeric(i))] # nolint
  )

  expect_identical(
    data_select(iris, -is.factor()),
    iris[sapply(iris, function(i) !is.factor(i))] # nolint
  )

  expect_identical(
    data_select(iris, -is.factor),
    iris[sapply(iris, function(i) !is.factor(i))] # nolint
  )

  expect_identical(data_select(iris, -is.logical), iris)
})


# select-nse with ranges  ---------------------

test_that("data_select works with ranges", {
  expect_identical(
    data_select(iris, 2:3),
    iris[2:3]
  )

  expect_identical(
    data_select(iris, Sepal.Width:Petal.Length),
    iris[2:3]
  )
})


# select-nse with negated ranges  ---------------------

test_that("data_select works with negated ranges", {
  expect_identical(
    data_select(iris, -(1:2)),
    iris[c(3, 4, 5)]
  )

  expect_identical(
    data_select(iris, -1:-2),
    iris[c(3, 4, 5)]
  )

  expect_identical(
    data_select(iris, exclude = -1:-2),
    iris[1:2]
  )

  expect_identical(
    data_select(iris, exclude = 2:3),
    iris[c(1, 4, 5)]
  )

  expect_error(
    data_select(iris, -Sepal.Width:Petal.Length),
    "can't mix negative and positive"
  )
  expect_identical(
    data_select(iris, -(Sepal.Width:Petal.Length)),
    iris[c(1, 4, 5)]
  )
})


# select-nse with formulas  ---------------------

test_that("data_select works with formulas", {
  expect_identical(
    data_select(iris, ~ Sepal.Width + Petal.Length),
    iris[2:3]
  )

  expect_identical(
    data_select(iris, exclude = ~ Sepal.Width + Petal.Length),
    iris[c(1, 4, 5)]
  )
})


# select-nse, other cases ---------------------

test_that("data_select works, other cases", {
  expect_identical(data_select(iris), iris)

  expect_identical(
    data_select(iris, c("Petal.Width", "Sepal.Length")),
    iris[c("Petal.Width", "Sepal.Length")]
  )

  expect_identical(
    data_select(iris, -c("Petal.Width", "Sepal.Length")),
    iris[setdiff(colnames(iris), c("Petal.Width", "Sepal.Length"))]
  )

  expect_identical(
    data_select(iris, -Petal.Width),
    iris[setdiff(colnames(iris), "Petal.Width")]
  )

  expect_identical(
    data_select(mtcars, c("am", "gear", "cyl")),
    mtcars[c("am", "gear", "cyl")]
  )

  expect_identical(
    data_select(mtcars, c("vam", "gear", "cyl")),
    mtcars[c("gear", "cyl")]
  )

  expect_warning(expect_null(data_select(mtcars, ends_with("abc"))))

  expect_identical(
    data_select(mtcars, regex("rb$")),
    mtcars["carb"]
  )

  expect_identical(
    data_select(mtcars, regex("^c")),
    mtcars[c("cyl", "carb")]
  )

  expect_warning(expect_null(data_select(mtcars, "^c")))

  expect_identical(
    data_select(mtcars, regex("^C"), ignore_case = TRUE),
    mtcars[c("cyl", "carb")]
  )
})


# select-nse works when called from other function  ---------------------

test_that("data_select from other functions", {
  test_fun1 <- function(data, i) {
    data_select(data, select = i)
  }
  expect_identical(
    test_fun1(iris, c("Sepal.Length", "Sepal.Width")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  expect_identical(
    test_fun1(iris, starts_with("Sep")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  test_fun1a <- function(data, i) {
    data_select(data, select = i, regex = TRUE)
  }
  expect_identical(
    test_fun1a(iris, "Sep"),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  test_fun1b <- function(data, i) {
    data_select(data, select = i, regex = TRUE)
  }
  expect_identical(
    test_fun1b(iris, "Width$"),
    iris[c("Sepal.Width", "Petal.Width")]
  )

  test_fun1c <- function(data, i) {
    data_select(data, select = -i)
  }
  expect_identical(
    test_fun1c(iris, c("Sepal.Length", "Sepal.Width")),
    iris[c("Petal.Length", "Petal.Width", "Species")]
  )


  test_fun2 <- function(data) {
    data_select(data, select = starts_with("Sep"))
  }
  expect_identical(
    test_fun2(iris),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  test_fun3 <- function(data) {
    i <- "Sep"
    data_select(data, select = starts_with(i))
  }
  expect_identical(
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
    data_select(x, select = testfun, exclude = -testfun2)
  }
  expect_identical(test_top(iris), iris["Petal.Length"])
})


# preserve attributes --------------------------

test_that("data_select preserves attributes", {
  skip_if_not_installed("parameters")

  m <- lm(Sepal.Length ~ Species, data = iris)
  out <- parameters::parameters(m)
  a1 <- attributes(out)

  out2 <- data_select(out, 1:3)
  a2 <- attributes(out2)

  expect_identical(sort(names(a1)), sort(names(a2)))
})

# Select helpers work in functions and loops

test_that("select helpers work in functions and loops", {
  foo <- function(data, i) {
    extract_column_names(data, select = starts_with(i))
  }
  expect_identical(
    foo(iris, "Sep"),
    c("Sepal.Length", "Sepal.Width")
  )

  for (i in "Sepal") {
    x <- extract_column_names(iris, select = starts_with(i))
  }
  expect_identical(
    x,
    c("Sepal.Length", "Sepal.Width")
  )

  for (i in "Length") {
    x <- extract_column_names(iris, select = ends_with(i))
  }
  expect_identical(
    x,
    c("Sepal.Length", "Petal.Length")
  )
})

test_that("select helpers work in functions and loops even if there's an object with the same name in the environment above", {
  i <- "Petal"
  foo <- function(data, i) {
    extract_column_names(data, select = starts_with(i))
  }
  expect_identical(
    foo(iris, "Sep"),
    c("Sepal.Length", "Sepal.Width")
  )

  for (i in "Sepal") {
    x <- extract_column_names(iris, select = starts_with(i))
  }
  expect_identical(
    x,
    c("Sepal.Length", "Sepal.Width")
  )

  i <- "Width"

  for (i in "Length") {
    x <- extract_column_names(iris, select = ends_with(i))
  }
  expect_identical(
    x,
    c("Sepal.Length", "Petal.Length")
  )
})

test_that("old solution still works", {
  foo <- function(data) {
    i <- "Sep"
    extract_column_names(data, select = i, regex = TRUE)
  }
  expect_identical(
    foo(iris),
    c("Sepal.Length", "Sepal.Width")
  )
})

test_that("data_select renames variables on the fly", {
  data(mtcars)
  expect_named(
    data_select(mtcars, c(new = "mpg", old = "cyl", hoho = "wt")),
    c("new", "old", "hoho")
  )
  expect_named(
    data_select(mtcars, c(new = "mpg", "cyl", hoho = "wt")),
    c("new", "cyl", "hoho")
  )
  expect_named(
    data_select(mtcars, c("mpg", "cyl", "wt")),
    c("mpg", "cyl", "wt")
  )
  # don't fail for non-existing columns
  expect_named(
    data_select(mtcars, c(new = "mpg", "cyl", hoho = "wt", test = "grea")),
    c("new", "cyl", "hoho")
  )
  # check that excluded variables don't cause troubles
  expect_named(
    data_select(mtcars, c(new = "mpg", "cyl", hoho = "wt"), exclude = "wt"),
    c("new", "cyl")
  )
  # error when names are not unique
  expect_error(
    data_select(mtcars, c(new = "mpg", old = "cyl", new = "wt")), # nolint
    regex = "Following names are duplicated"
  )
  expect_error(
    data_select(mtcars, c(new = "mpg", "cyl", cyl = "wt")), # nolint
    regex = "Following names are duplicated"
  )
  # when new name is used in exclude, it should be ignored
  expect_named(
    data_select(mtcars, c(drat = "mpg"), exclude = "drat"),
    "drat"
  )
})
