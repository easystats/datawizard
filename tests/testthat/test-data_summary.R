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
  out <- data_summary(
    iris,
    MW = mean(Sepal.Width),
    SD = sd(Sepal.Width),
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
  out <- data_summary(
    mtcars,
    MW = mean(mpg),
    SD = sd(mpg),
    by = c("am", "gear")
  )
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
    data_summary(
      iris,
      MW = mean(Sepal.Width),
      SD = sd(Sepal.Width),
      by = "Speceis"
    ),
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
    data_summary(
      iris$Sepal.Width,
      MW = mean(Sepal.Width),
      SD = sd(Sepal.Width)
    ),
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
    data_summary(
      mtcars,
      n = unique(mpg),
      j = c(min(am), max(am)),
      by = c("am", "gear")
    ),
    regex = "Each expression must return"
  )
})


test_that("data_summary, values_at", {
  data(mtcars)
  out <- data_summary(
    mtcars,
    pos1 = mpg[1],
    pos_end = mpg[length(mpg)],
    by = c("am", "gear")
  )
  # same as:
  # dplyr::summarise(mtcars, pos1 = dplyr::first(mpg), pos_end = dplyr::last(mpg), .by = c("am", "gear"))
  expect_equal(out$pos1, c(21.4, 24.4, 21, 26), tolerance = 1e-3)
  expect_equal(out$pos_end, c(19.2, 17.8, 21.4, 15), tolerance = 1e-3)
})


test_that("data_summary, print", {
  data(mtcars)
  out <- data_summary(
    mtcars,
    MW = mean(mpg),
    SD = sd(mpg),
    by = c("am", "gear")
  )
  expect_snapshot(print(out))
})


test_that("data_summary, with NA", {
  data(efc, package = "datawizard")
  out <- data_summary(efc, MW = mean(c12hour, na.rm = TRUE), by = "c172code")
  expect_snapshot(print(out))
  out <- data_summary(
    efc,
    MW = mean(c12hour, na.rm = TRUE),
    by = "c172code",
    remove_na = TRUE
  )
  expect_snapshot(print(out))
  # sorting for multiple groups
  out <- data_summary(
    efc,
    MW = mean(c12hour, na.rm = TRUE),
    by = c("e42dep", "c172code")
  )
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
  expect_equal(
    out$MW,
    aggregate(mtcars["mpg"], list(mtcars$am, mtcars$gear), mean)$mpg,
    tolerance = 1e-4
  )
  expect_error(
    data_summary(mtcars, a, b, by = c("am", "gear")),
    regex = "You cannot mix"
  )
  out <- data_summary(mtcars, c(a, b), by = c("am", "gear"))
  expect_named(out, c("am", "gear", "MW", "SD"))
  expect_equal(
    out$SD,
    aggregate(mtcars["mpg"], list(mtcars$am, mtcars$gear), sd)$mpg,
    tolerance = 1e-4
  )
})


test_that("data_summary, extra functions", {
  data(mtcars)
  # n()
  out <- data_summary(mtcars, n = n(), by = c("am", "gear"))
  expect_identical(out$n, c(15L, 4L, 8L, 5L))
})


test_that("data_summary, bayestestR::ci", {
  skip_if_not_installed("bayestestR")
  data(mtcars)
  out <- data_summary(
    mtcars,
    mean_value = mean(mpg),
    ci = bayestestR::ci(mpg),
    by = c("am", "gear")
  )
  expect_named(out, c("am", "gear", "mean_value", "CI", "CI_low", "CI_high"))
  expect_snapshot(out)
  out <- data_summary(
    mtcars,
    mw = mean(mpg),
    test = bayestestR::ci(mpg),
    yolo = c(mean(mpg), sd(mpg)),
    by = c("am", "gear")
  )
  expect_named(
    out,
    c("am", "gear", "mw", "CI", "CI_low", "CI_high", "yolo_1", "yolo_2")
  )
})

test_that("no warning when variable name and function in global env clash, #583", {
  dat <- data.frame(rt = 1:10)
  expect_silent(data_summary(dat, rt = mean(rt)))
})


test_that("allow multiple columns for expressions", {
  set.seed(123)
  d <- data.frame(
    x = rnorm(100, 1, 1),
    y = rnorm(100, 2, 2),
    groups = rep(1:4, each = 25)
  )

  out <- data_summary(
    d,
    quant_x = quantile(x, c(0.25, 0.75)),
    quant_y = quantile(y, c(0.25, 0.75)),
    suffix = c("Q1", "Q3")
  )
  expect_equal(
    out$quant_xQ1,
    0.50615,
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  expect_named(out, c("quant_xQ1", "quant_xQ3", "quant_yQ1", "quant_yQ3"))

  # multiple column suffixes
  out <- data_summary(
    d,
    quant_x = quantile(x, c(0.25, 0.75)),
    quant_y = quantile(y, c(0.1, 0.9)),
    suffix = list(c("Q1", "Q3"), c("10perc", "90perc"))
  )
  expect_named(
    out,
    c("quant_xQ1", "quant_xQ3", "quant_y10perc", "quant_y90perc")
  )

  # test with grouped data
  out <- data_summary(
    d,
    quant_x = quantile(x, c(0.25, 0.75)),
    quant_y = quantile(y, c(0.1, 0.9)),
    suffix = list(c("Q1", "Q3"), c("10perc", "90perc")),
    by = "groups"
  )
  expect_named(
    out,
    c("groups", "quant_xQ1", "quant_xQ3", "quant_y10perc", "quant_y90perc")
  )
  expect_equal(
    out$quant_xQ1,
    c(0.37496, 0.59712, 0.49768, 0.71523),
    tolerance = 1e-3
  )

  out <- data_summary(
    d,
    quant_x = quantile(x, c(0.25, 0.75)),
    quant_y = quantile(y, c(0.25, 0.5, 0.75)),
    suffix = list(c("Q1", "Q3"), c("_Q1", "_Q2", "_Q3"))
  )
  expect_equal(
    out,
    data.frame(
      quant_xQ1 = 0.506145762180273,
      quant_xQ3 = 1.69181916596599,
      quant_y_Q1 = 0.397791122797595,
      quant_y_Q2 = 1.54833992341721,
      quant_y_Q3 = 2.93568710177563
    ),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  out <- data_summary(
    d,
    quant_x = quantile(x, c(0.25, 0.75)),
    quant_y = quantile(y, c(0.25, 0.5, 0.75)),
    suffix = list(c("Q1", "Q3"), c("_Q1", "_Q2", "_Q3")),
    by = "groups"
  )
  expect_equal(
    out,
    data.frame(
      groups = c(1, 2, 3, 4),
      quant_xQ1 = c(0.375, 0.5971, 0.4977, 0.7152),
      quant_xQ3 = c(1.4609, 1.8216, 1.4482, 1.9935),
      quant_y_Q1 = c(0.4302, -0.8878, 1.2508, 0.269),
      quant_y_Q2 = c(1.3049, 1.4756, 2.1306, 1.5274),
      quant_y_Q3 = c(2.2353, 3.3758, 3.2731, 2.621)
    ),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  # automatic suffixes
  out <- data_summary(
    d,
    quant_x = quantile(x, c(0.25, 0.75)),
    quant_y = quantile(y, c(0.1, 0.9)),
    suffix = NULL
  )
  expect_named(out, c("quant_x_1", "quant_x_2", "quant_y_1", "quant_y_2"))

  # use own suffix only for one expression - other expressions are
  # suffixed with `_1`, `_2`, etc.
  out <- data_summary(
    d,
    quant_x = quantile(x, c(0.25, 0.75)),
    quant_y = quantile(y, c(0.25, 0.5, 0.75)),
    mean_x = mean(x),
    suffix = list(quant_y = c("_Q1", "_Q2", "_Q3")),
    by = "groups"
  )
  expect_named(
    out,
    c(
      "groups",
      "quant_x_1",
      "quant_x_2",
      "quant_y_Q1",
      "quant_y_Q2",
      "quant_y_Q3",
      "mean_x"
    )
  )

  # errors ------------------------------------------------------------------

  # number of elements in suffix must match number of new columns
  expect_error(
    data_summary(
      d,
      quant_x = quantile(x, c(0.25, 0.75)),
      quant_y = quantile(y, c(0.1, 0.9)),
      suffix = c("a", "b", "c")
    ),
    regex = "Argument `suffix` must have the same length",
    fixed = TRUE
  )

  # if suffix is a list, number of elements in suffix must match number of
  # expressions
  expect_error(
    data_summary(
      d,
      quant_x = quantile(x, c(0.25, 0.75)),
      quant_y = quantile(y, c(0.1, 0.9)),
      suffix = list(c("a", "b"), c("c", "d"), c("e", "f"))
    ),
    regex = "If `suffix` is a list of character vectors",
    fixed = TRUE
  )
})
