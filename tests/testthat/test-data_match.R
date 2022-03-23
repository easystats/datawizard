data(efc, package = "datawizard")

test_that("data_match works as expected", {
  matching_rows <- data_match(mtcars, data.frame(vs = 0, am = 1), return_indices = TRUE)
  df1 <- mtcars[matching_rows, ]
  expect_equal(unique(df1$vs), 0)
  expect_equal(unique(df1$am), 1)

  matching_rows <- data_match(mtcars, data.frame(vs = 0, am = c(0, 1)), return_indices = TRUE)
  df2 <- mtcars[matching_rows, ]
  expect_equal(unique(df2$vs), 0)
  expect_equal(unique(df2$am), c(1, 0))
})

test_that("data_match works with missing data", {
  skip_if_not_installed("poorman")

  # "OR" works
  x1 <- length(data_match(efc, data.frame(c172code = 1, e16sex = 2), match = "or", return_indices = TRUE))
  x2 <- nrow(poorman::filter(efc, c172code == 1 | e16sex == 2))
  expect_equal(x1, x2)

  # "AND" works
  x1 <- length(data_match(efc, data.frame(c172code = 1, e16sex = 2), match = "and", return_indices = TRUE))
  x2 <- nrow(poorman::filter(efc, c172code == 1, e16sex == 2))
  expect_equal(x1, x2)

  # "NOT" works
  x1 <- length(data_match(efc, data.frame(c172code = 1, e16sex = 2), match = "not", return_indices = TRUE))
  x2 <- nrow(poorman::filter(efc, c172code != 1, e16sex != 2))
  expect_equal(x1, x2)
})

test_that("data_match works with logical expressions", {
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "not")
  out2 <- data_match(mtcars, vs != 0 & am != 1)
  expect_equal(out1, out2, ignore_attr = TRUE)

  # using a data frame re-orders rows!
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "or")
  out2 <- data_match(mtcars, vs == 0 | am == 1)
  expect_equal(out1[order(out1$vs, out1$am), ], out2[order(out2$vs, out2$am), ], ignore_attr = TRUE)
})


test_that("data_filter works", {
  out1 <- data_match(mtcars, data.frame(vs = 0, am = 1), match = "not")
  out2 <- data_match(mtcars, vs != 0 & am != 1)
  out3 <- data_filter(mtcars, vs != 0 & am != 1)
  expect_equal(out1, out2, ignore_attr = TRUE)
  expect_equal(out1, out3, ignore_attr = TRUE)
  expect_equal(out2, out3, ignore_attr = TRUE)

  out1 <- data_match(mtcars, vs == 0 | am == 1)
  out2 <- data_filter(mtcars, vs == 0 | am == 1)
  out3 <- subset(mtcars, subset = vs == 0 | am == 1)
  expect_equal(out1, out2, ignore_attr = TRUE)
  expect_equal(out1, out3, ignore_attr = TRUE)
  expect_equal(out2, out3, ignore_attr = TRUE)
})
