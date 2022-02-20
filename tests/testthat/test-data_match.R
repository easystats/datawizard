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
