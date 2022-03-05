test_that("data_match works as expected", {
  matching_rows <- data_match(mtcars, data.frame(vs = 0, am = 1))
  df1 <- mtcars[matching_rows, ]
  expect_equal(unique(df1$vs), 0)
  expect_equal(unique(df1$am), 1)

  matching_rows <- data_match(mtcars, data.frame(vs = 0, am = c(0, 1)))
  df2 <- mtcars[matching_rows, ]
  expect_equal(unique(df2$vs), 0)
  expect_equal(unique(df2$am), c(1, 0))
})


if (require("poorman")) {
  data(efc, package = "datawizard")
  test_that("data_match works with missing data", {
    # "OR" works
    x1 <- length(data_match(efc, data.frame(c172code = 1, e16sex = 2), match = "or"))
    x2 <- nrow(poorman::filter(efc, c172code == 1 | e16sex == 2))
    expect_equal(x1, x2)

    # "AND" works
    x1 <- length(data_match(efc, data.frame(c172code = 1, e16sex = 2), match = "and"))
    x2 <- nrow(poorman::filter(efc, c172code == 1, e16sex == 2))
    expect_equal(x1, x2)

    # "NOT" works
    x1 <- length(data_match(efc, data.frame(c172code = 1, e16sex = 2), match = "not"))
    x2 <- nrow(poorman::filter(efc, c172code != 1, e16sex != 2))
    expect_equal(x1, x2)
  })
}

