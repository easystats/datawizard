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
  skip_if_not_or_load_if_installed("poorman")

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
    drop_na = FALSE
  ))
  expect_identical(x1, 41L)
  x1 <- length(data_match(
    efc,
    data.frame(c172code = 1, e16sex = 2),
    match = "not",
    return_indices = TRUE,
    drop_na = TRUE
  ))
  expect_identical(x1, 36L)
})

test_that("data_match and data_filter work similar", {
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "not")
  out2 <- data_filter(mtcars, vs != 0 & am != 1)
  expect_identical(out1, out2, ignore_attr = TRUE)

  # using a data frame re-orders rows!
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "or")
  out2 <- data_filter(mtcars, vs == 0 | am == 1)
  expect_identical(out1[order(out1$vs, out1$am), ], out2[order(out2$vs, out2$am), ], ignore_attr = TRUE)

  # string representation is working
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "or")
  out2 <- data_filter(mtcars, "vs == 0 | am == 1")
  expect_identical(out1[order(out1$vs, out1$am), ], out2[order(out2$vs, out2$am), ], ignore_attr = TRUE)
})


test_that("data_filter works", {
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "not")
  out2 <- data_filter(mtcars, vs != 0 & am != 1)
  out3 <- subset(mtcars, vs != 0 & am != 1)
  expect_identical(out1, out2, ignore_attr = TRUE)
  expect_identical(out1, out3, ignore_attr = TRUE)
  expect_identical(out2, out3, ignore_attr = TRUE)
})


test_that("data_filter works with string representation", {
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "not")
  out2 <- data_filter(mtcars, "vs != 0 & am != 1")
  out3 <- subset(mtcars, vs != 0 & am != 1)
  expect_identical(out1, out2, ignore_attr = TRUE)
  expect_identical(out1, out3, ignore_attr = TRUE)
  expect_identical(out2, out3, ignore_attr = TRUE)
})


test_that("data_filter works like slice", {
  out <- data_filter(mtcars, 5:10)
  expect_identical(out, mtcars[5:10, ], ignore_attr = TRUE)
})

test_that("data_filter gives informative message on errors", {
  expect_error(
    data_filter(mtcars, "mpg > 10 || cyl = 4"),
    "`==`"
  )
  expect_error(
    data_filter(mtcars, filter = mpg > 10 || cyl == 4),
    "`||`"
  )
  expect_error(
    data_filter(mtcars, filter = mpg > 10 && cyl == 4),
    "`&&`"
  )
  expect_error(
    data_filter(mtcars, filter = mpg > 10 ? cyl == 4),
    "syntax"
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

  expect_identical(
    data_filter(mtcars, "mpg >= 30.4 & hp == 66"),
    subset(mtcars, mpg >= 30.4 & hp == 66)
  )
  expect_identical(
    data_filter(mtcars, mpg <= 30.4 & hp == 66),
    subset(mtcars, mpg <= 30.4 & hp == 66)
  )
})

test_that("programming with data_filter", {
  # One arg ------------

  foo <- function(var) {
    data_filter(mtcars, filter = {
      var
    } > 30)
  }
  expect_identical(
    foo("mpg"),
    data_filter(mtcars, "mpg >= 30")
  )

  foo <- function(var) {
    data_filter(mtcars, filter = "{var} > 30")
  }
  expect_identical(
    foo("mpg"),
    data_filter(mtcars, "mpg >= 30")
  )

  # Two args -----------

  foo <- function(var, var2) {
    data_filter(mtcars, filter = {
      var
    } > 30 & {
      var2
    } <= 66)
  }
  expect_identical(
    foo("mpg", "hp"),
    data_filter(mtcars, "mpg >= 30 & hp <= 66")
  )

  foo <- function(var, var2) {
    data_filter(mtcars, filter = "{var} > 30 & {var2} <= 66")
  }
  expect_identical(
    foo("mpg", "hp"),
    data_filter(mtcars, "mpg >= 30 & hp <= 66")
  )
})

test_that("programming with data_filter in global env", {
  var <- "mpg"
  var2 <- "hp"
  expect_identical(
    data_filter(mtcars, "{var} > 30 & {var2} <= 66"),
    data_filter(mtcars, "mpg >= 30 & hp <= 66")
  )
  expect_identical(
    data_filter(mtcars, {
      var
    } > 30 & {
      var2
    } <= 66),
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

  expect_identical(
    data_filter(test, x == min(x)),
    expected
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

  var <- "x"

  expect_identical(
    data_filter(test, "{var} == min({var})"),
    expected
  )
})
