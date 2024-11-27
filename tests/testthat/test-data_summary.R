test_that("data_summary, single row summary", {
  data(iris)
  out <- data_summary(iris, MW = mean(Sepal.Width), SD = sd(Sepal.Width))
  expect_equal(out$MW, mean(iris$Sepal.Width), tolerance = 1e-4)
  expect_equal(out$SD, sd(iris$Sepal.Width), tolerance = 1e-4)
})


test_that("data_summary, single row summary, string expression", {
  data(iris)
  out <- data_summary(iris, "MW = mean(Sepal.Width)", "SD = sd(Sepal.Width)")
  expect_equal(out$MW, mean(iris$Sepal.Width), tolerance = 1e-4)
  expect_equal(out$SD, sd(iris$Sepal.Width), tolerance = 1e-4)
})


test_that("data_summary, summary for groups", {
  data(iris)
  out <- data_summary(iris, MW = mean(Sepal.Width), SD = sd(Sepal.Width), by = "Species")
  expect_equal(
    out$MW,
    aggregate(iris["Sepal.Width"], list(iris$Species), mean)$Sepal.Width,
    tolerance = 1e-4
  )
  expect_equal(
    out$SD,
    aggregate(iris["Sepal.Width"], list(iris$Species), sd)$Sepal.Width,
    tolerance = 1e-4
  )
})


test_that("data_summary, summary for groups, string expression", {
  data(iris)
  out <- data_summary(
    iris,
    "MW = mean(Sepal.Width)",
    "SD = sd(Sepal.Width)",
    by = "Species"
  )
  expect_equal(
    out$MW,
    aggregate(iris["Sepal.Width"], list(iris$Species), mean)$Sepal.Width,
    tolerance = 1e-4
  )
  expect_equal(
    out$SD,
    aggregate(iris["Sepal.Width"], list(iris$Species), sd)$Sepal.Width,
    tolerance = 1e-4
  )
})


test_that("data_summary, grouped data frames", {
  data(iris)
  d <- data_group(iris, "Species")
  out <- data_summary(d, MW = mean(Sepal.Width), SD = sd(Sepal.Width))
  expect_equal(
    out$MW,
    aggregate(iris["Sepal.Width"], list(iris$Species), mean)$Sepal.Width,
    tolerance = 1e-4
  )
  expect_equal(
    out$SD,
    aggregate(iris["Sepal.Width"], list(iris$Species), sd)$Sepal.Width,
    tolerance = 1e-4
  )
  # "by" overrides groups
  data(mtcars)
  d <- data_group(mtcars, "gear")
  out <- data_summary(d, MW = mean(mpg), SD = sd(mpg), by = "am")
  expect_identical(
    out$MW,
    aggregate(mtcars["mpg"], list(mtcars$am), mean)$mpg
  )
})


test_that("data_summary, summary for multiple groups", {
  data(mtcars)
  out <- data_summary(mtcars, MW = mean(mpg), SD = sd(mpg), by = c("am", "gear"))
  expect_equal(
    out$MW,
    aggregate(mtcars["mpg"], list(mtcars$am, mtcars$gear), mean)$mpg,
    tolerance = 1e-4
  )
  expect_equal(
    out$SD,
    aggregate(mtcars["mpg"], list(mtcars$am, mtcars$gear), sd)$mpg,
    tolerance = 1e-4
  )
  x <- data_group(mtcars, c("am", "gear"))
  out <- data_summary(x, MW = mean(mpg), SD = sd(mpg))
  expect_equal(
    out$MW,
    aggregate(mtcars["mpg"], list(mtcars$am, mtcars$gear), mean)$mpg,
    tolerance = 1e-4
  )
  expect_equal(
    out$SD,
    aggregate(mtcars["mpg"], list(mtcars$am, mtcars$gear), sd)$mpg,
    tolerance = 1e-4
  )
})


test_that("data_summary, errors", {
  data(iris)
  data(mtcars)
  # "by" must be character
  expect_error(
    data_summary(iris, MW = mean(Sepal.Width), SD = sd(Sepal.Width), by = 5),
    regex = "Argument `by` must be a character string"
  )
  # "by" must be in data
  expect_error(
    data_summary(iris, MW = mean(Sepal.Width), SD = sd(Sepal.Width), by = "Speceis"),
    regex = "Variable \"Speceis\" not"
  )
  # by for multiple variables
  expect_error(
    data_summary(mtcars, MW = mean(mpg), SD = sd(mpg), by = c("bam", "gear")),
    regex = "Variable \"bam\" not"
  )
  expect_error(
    data_summary(mtcars, MW = mean(mpg), SD = sd(mpg), by = c("bam", "geas")),
    regex = "Did you mean one of \"am\" or \"gear\"?"
  )
  # not a data frame
  expect_error(
    data_summary(iris$Sepal.Width, MW = mean(Sepal.Width), SD = sd(Sepal.Width)),
    regex = "only works for"
  )
  # no expressions
  expect_error(
    data_summary(iris, by = "Species"),
    regex = "No expressions for calculating"
  )
  # wrong expression
  expect_error(
    data_summary(mtcars, mw = mesn(mpg), by = "am"),
    regex = "There was an error"
  )
  # wrong variable name
  expect_error(
    data_summary(mtcars, n = max(mpeg)),
    regex = "There was an error"
  )
  # expression returns more than one value
  expect_error(
    data_summary(mtcars, n = unique(mpg), j = c(min(am), max(am)), by = c("am", "gear")),
    regex = "Each expression must return"
  )
})


test_that("data_summary, values_at", {
  data(mtcars)
  out <- data_summary(mtcars, pos1 = mpg[1], pos_end = mpg[length(mpg)], by = c("am", "gear"))
  # same as:
  # dplyr::summarise(mtcars, pos1 = dplyr::first(mpg), pos_end = dplyr::last(mpg), .by = c("am", "gear"))
  expect_equal(out$pos1, c(21.4, 24.4, 21, 26), tolerance = 1e-3)
  expect_equal(out$pos_end, c(19.2, 17.8, 21.4, 15), tolerance = 1e-3)
})


test_that("data_summary, print", {
  data(mtcars)
  out <- data_summary(mtcars, MW = mean(mpg), SD = sd(mpg), by = c("am", "gear"))
  expect_snapshot(print(out))
})


test_that("data_summary, with NA", {
  data(efc, package = "datawizard")
  out <- data_summary(efc, MW = mean(c12hour, na.rm = TRUE), by = "c172code")
  expect_snapshot(print(out))
  out <- data_summary(efc, MW = mean(c12hour, na.rm = TRUE), by = "c172code", remove_na = TRUE)
  expect_snapshot(print(out))
  # sorting for multiple groups
  out <- data_summary(efc, MW = mean(c12hour, na.rm = TRUE), by = c("e42dep", "c172code"))
  expect_snapshot(print(out))
})


test_that("data_summary, inside functions", {
  foo1 <- function(x, ...) {
    datawizard::data_summary(x, ..., by = "Species")
  }

  foo2 <- function(x, by, ...) {
    datawizard::data_summary(x, ..., by = by)
  }

  foo3 <- function(x, by) {
    datawizard::data_summary(x, MW = mean(Sepal.Width), by = by)
  }

  data(iris)
  out1 <- foo1(iris, MW = mean(Sepal.Width))
  out2 <- foo2(iris, by = "Species", MW = mean(Sepal.Width))
  out3 <- foo3(iris, "Species")
  expect_equal(out1$MW, out2$MW, tolerance = 1e-4)
  expect_equal(out1$MW, out3$MW, tolerance = 1e-4)
})


test_that("data_summary, expression as variable", {
  data(mtcars)
  a <- "MW = mean(mpg)"
  b <- "SD = sd(mpg)"
  out <- data_summary(mtcars, a, by = c("am", "gear"))
  expect_named(out, c("am", "gear", "MW"))
  expect_equal(out$MW, aggregate(mtcars["mpg"], list(mtcars$am, mtcars$gear), mean)$mpg, tolerance = 1e-4)
  expect_error(
    data_summary(mtcars, a, b, by = c("am", "gear")),
    regex = "You cannot mix"
  )
  out <- data_summary(mtcars, c(a, b), by = c("am", "gear"))
  expect_named(out, c("am", "gear", "MW", "SD"))
  expect_equal(out$SD, aggregate(mtcars["mpg"], list(mtcars$am, mtcars$gear), sd)$mpg, tolerance = 1e-4)
})


test_that("data_summary, extra functions", {
  data(mtcars)
  # n()
  out <- data_summary(mtcars, n = n(), by = c("am", "gear"))
  expect_identical(out$n, c(15L, 4L, 8L, 5L))
})


test_that("data_summary, bayestestR::ci", {
  skip_if_not_installed("bayestesR")
  data(mtcars)
  out <- data_summary(
    mtcars,
    mean_value = mean(mpg),
    ci = bayestestR::ci(mpg),
    by = c("am", "gear")
  )
  expect_snapshot(out)
  expect_error(
    data_summary(
      mtcars,
      mw = mean(mpg),
      test = bayestestR::ci(mpg),
      yolo = c(mean(mpg), sd(mpg)),
      by = c("am", "gear")
    ),
    regex = "Each expression"
  )
})
