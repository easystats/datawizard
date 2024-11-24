data(efc, package = "datawizard")

test_that("data_match works as expected", {
  matching_rows <- data_match(mtcars, data.frame(vs = 0, am = 1), return_indices = TRUE)
  df1 <- mtcars[matching_rows, ]
  expect_identical(unique(df1$vs), 0)
  expect_identical(unique(df1$am), 1)

  matching_rows <- data_match(mtcars, data.frame(vs = 0, am = c(0, 1)), return_indices = TRUE)
  df2 <- mtcars[matching_rows, ]
  expect_identical(unique(df2$vs), 0)
  expect_identical(unique(df2$am), c(1, 0))
})


test_that("data_match works with missing data", {
  skip_if_not_installed("poorman")

  # "OR" works
  x1 <- length(data_match(
    efc,
    data.frame(c172code = 1, e16sex = 2),
    match = "or",
    return_indices = TRUE
  ))
  x2 <- nrow(poorman::filter(efc, c172code == 1 | e16sex == 2))
  expect_identical(x1, x2)

  # "AND" works
  x1 <- length(data_match(
    efc,
    data.frame(c172code = 1, e16sex = 2),
    match = "and",
    return_indices = TRUE
  ))
  x2 <- nrow(poorman::filter(efc, c172code == 1, e16sex == 2))
  expect_identical(x1, x2)

  # "NOT" works
  x1 <- length(data_match(
    efc,
    data.frame(c172code = 1, e16sex = 2),
    match = "not",
    return_indices = TRUE
  ))
  x2 <- nrow(poorman::filter(efc, c172code != 1, e16sex != 2))
  expect_identical(x1, x2)

  # remove NA
  x1 <- length(data_match(
    efc,
    data.frame(c172code = 1, e16sex = 2),
    match = "not",
    return_indices = TRUE,
    remove_na = FALSE
  ))
  expect_identical(x1, 41L)
  x1 <- length(data_match(
    efc,
    data.frame(c172code = 1, e16sex = 2),
    match = "not",
    return_indices = TRUE,
    remove_na = TRUE
  ))
  expect_identical(x1, 36L)
})


test_that("data_match and data_filter work similar", {
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "not")
  out2 <- data_filter(mtcars, vs != 0 & am != 1)
  expect_equal(out1, out2, ignore_attr = TRUE)

  # using a data frame re-orders rows!
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "or")
  out2 <- data_filter(mtcars, vs == 0 | am == 1)
  expect_equal(
    out1[order(out1$vs, out1$am), ],
    out2[order(out2$vs, out2$am), ],
    ignore_attr = TRUE
  )

  # string representation is working
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "or")
  out2 <- data_filter(mtcars, "vs == 0 | am == 1")
  expect_equal(
    out1[order(out1$vs, out1$am), ],
    out2[order(out2$vs, out2$am), ],
    ignore_attr = TRUE
  )
})


test_that("data_filter works", {
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "not")
  out2 <- data_filter(mtcars, vs != 0 & am != 1)
  out3 <- subset(mtcars, vs != 0 & am != 1)
  out4 <- data_filter(mtcars, vs != 0, am != 1)
  expect_equal(out1, out2, ignore_attr = TRUE)
  expect_equal(out1, out3, ignore_attr = TRUE)
  expect_equal(out2, out4, ignore_attr = TRUE)
})


test_that("data_filter works with string representation", {
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "not")
  out2 <- data_filter(mtcars, "vs != 0 & am != 1")
  out3 <- subset(mtcars, vs != 0 & am != 1)
  out4 <- data_filter(mtcars, c("vs != 0", "am != 1"))
  expect_equal(out1, out2, ignore_attr = TRUE)
  expect_equal(out1, out3, ignore_attr = TRUE)
  expect_equal(out2, out3, ignore_attr = TRUE)
  expect_equal(out2, out4, ignore_attr = TRUE)
})


test_that("data_filter works like slice", {
  out <- data_filter(mtcars, 5:10)
  expect_equal(out, mtcars[5:10, ], ignore_attr = TRUE)
  out <- data_filter(mtcars, "5:10")
  expect_equal(out, mtcars[5:10, ], ignore_attr = TRUE)
  slc <- 5:10
  out <- data_filter(mtcars, slc)
  expect_equal(out, mtcars[5:10, ], ignore_attr = TRUE)
  slc <- "5:10"
  out <- data_filter(mtcars, slc)
  expect_equal(out, mtcars[5:10, ], ignore_attr = TRUE)
})


test_that("data_filter gives informative message on errors", {
  expect_error(
    data_filter(mtcars, mpg = 10),
    "`==`"
  )
  expect_error(
    data_filter(mtcars, "mpg > 10 || cyl = 4"),
    "`==`"
  )
  expect_error(
    data_filter(mtcars, mpg > 10 || cyl == 4),
    "`||`"
  )
  expect_error(
    data_filter(mtcars, mpg > 10 && cyl == 4),
    "`&&`"
  )
  ## TODO: need to check why this fails on R 4.1
  skip_if(getRversion() < "4.2.0")
  expect_error(
    data_filter(mtcars, mpg > 10 ? cyl == 4),
    "syntax"
  )
  expect_error(
    data_filter(mtcars, mgp > 10 ? cyl == 4),
    "Variable \"mgp\""
  )
})


test_that("data_filter gives informative message on errors", {
  data(mtcars)
  expect_error(
    data_filter(mtcars, cxl == 6),
    regex = "Variable \"cxl\""
  )
  expect_error(
    data_filter(mtcars, "cxl == 6"),
    regex = "Variable \"cxl\""
  )
})


test_that("data_filter works with >= or <=", {
  expect_identical(
    data_filter(mtcars, "mpg >= 30.4"),
    subset(mtcars, mpg >= 30.4)
  )
  expect_identical(
    data_filter(mtcars, mpg >= 30.4),
    subset(mtcars, mpg >= 30.4)
  )
  expect_identical(
    data_filter(mtcars, "mpg <= 30.4"),
    subset(mtcars, mpg <= 30.4)
  )
  expect_identical(
    data_filter(mtcars, mpg <= 30.4),
    subset(mtcars, mpg <= 30.4)
  )
  mpgl30 <- "mpg <= 30.4"
  expect_identical(
    data_filter(mtcars, mpgl30),
    subset(mtcars, mpg <= 30.4)
  )

  expect_identical(
    data_filter(mtcars, "mpg >= 30.4 & hp == 66"),
    subset(mtcars, mpg >= 30.4 & hp == 66)
  )
  expect_identical(
    data_filter(mtcars, mpg <= 30.4 & hp == 66),
    subset(mtcars, mpg <= 30.4 & hp == 66)
  )
  mpgl30hp66 <- "mpg >= 30.4 & hp == 66"
  expect_identical(
    data_filter(mtcars, mpgl30hp66),
    subset(mtcars, mpg >= 30.4 & hp == 66)
  )
})


test_that("programming with data_filter", {
  # One arg ------------

  foo <- function(var) {
    data_filter(mtcars, var)
  }
  expect_identical(
    foo("mpg >= 30"),
    data_filter(mtcars, "mpg >= 30")
  )

  foo2 <- function(data) {
    var2 <- "mpg >= 30"
    data_filter(data, var2)
  }
  expect_identical(
    foo2(mtcars),
    data_filter(mtcars, "mpg >= 30")
  )

  foo3 <- function(data) {
    var <- "mpg >= 30"
    data_filter(data, var)
  }
  expect_identical(
    foo3(mtcars),
    data_filter(mtcars, "mpg >= 30")
  )

  # Two args -----------

  foo4 <- function(data, var3) {
    data_filter(data, var3)
  }
  expect_identical(
    foo4(mtcars, "mpg >= 30 & hp <= 66"),
    data_filter(mtcars, "mpg >= 30 & hp <= 66")
  )
})


test_that("programming with data_filter with variables", {
  var4 <- "mpg >= 30 & hp <= 66"
  expect_identical(
    data_filter(mtcars, var4),
    data_filter(mtcars, "mpg >= 30 & hp <= 66")
  )
  var <- "mpg >= 30 & hp <= 66"
  expect_identical(
    data_filter(mtcars, var),
    data_filter(mtcars, "mpg >= 30 & hp <= 66")
  )
})


test_that("data_filter works with groups", {
  test <- data.frame(
    id = c(1, 1, 2, 2),
    x = c(0, 1, 3, 4),
    y = c(1, 2, 3, 4)
  )
  test <- data_group(test, "id")

  expected <- data.frame(id = c(1, 2), x = c(0, 3), y = c(1, 3))
  class(expected) <- c("grouped_df", "data.frame")
  attributes(expected)$groups <- attributes(test)$groups

  expect_equal(
    data_filter(test, x == min(x)),
    expected,
    ignore_attr = TRUE
  )
})


test_that("data_filter programming works with groups", {
  test <- data.frame(
    id = c(1, 1, 2, 2),
    x = c(0, 1, 3, 4),
    y = c(1, 2, 3, 4)
  )
  test <- data_group(test, "id")

  expected <- data.frame(id = c(1, 2), x = c(0, 3), y = c(1, 3))
  class(expected) <- c("grouped_df", "data.frame")
  attributes(expected)$groups <- attributes(test)$groups

  expect_equal(
    data_filter(test, "x == min(x)"),
    expected,
    ignore_attr = TRUE
  )

  foo_gr1 <- function(data, var) {
    data_filter(data, var)
  }
  out <- foo_gr1(test, "x == min(x)")
  expect_equal(out, expected, ignore_attr = TRUE)
})


test_that("data_filter with groups, different ways of dots", {
  grp <- data_group(mtcars, "cyl")
  fli <- "mpg <= 20"
  out1 <- data_filter(grp, mpg <= 20)
  out2 <- data_filter(grp, "mpg <= 20")
  out3 <- data_filter(grp, fli)
  expect_identical(out1, out2)
  expect_identical(out1, out3)
})


test_that("data_filter, slicing works with functions", {
  d <- data.frame(
    a = c("aa", "a1", "bb", "b1", "cc", "c1"),
    b = 1:6,
    stringsAsFactors = FALSE
  )

  rows <- grep("^[A-Za-z][0-9]$", x = d$a)
  out1 <- data_filter(d, rows)
  out2 <- data_filter(d, grep("^[A-Za-z][0-9]$", x = d$a))

  expect_identical(out1, out2)

  out3 <- data_filter(iris, (Sepal.Width == 3.0) & (Species == "setosa"))
  expect_identical(nrow(out3), 6L)

  # styler: off
  expect_error(
    data_filter(iris, (Sepal.Width = 3.0) & (Species = "setosa")), # nolint
    regex = "Filtering did not work"
  )
  # styler: on
})
