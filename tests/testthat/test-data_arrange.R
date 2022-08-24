library(dplyr)
df <- head(mtcars)
df$character <- c("a", "b", "b", "c", "c", "a")

test_that("data_arrange works with one numeric column", {
  expect_equal(
    arrange(df, carb),
    data_arrange(df, "carb")
  )
  expect_equal(
    arrange(df, -carb),
    data_arrange(df, "-carb")
  )
})

test_that("data_arrange works with one character column", {
  expect_equal(
    arrange(df, character),
    data_arrange(df, "character")
  )
  expect_equal(
    arrange(df, desc(character)),
    data_arrange(df, "-character")
  )
})

test_that("data_arrange works with several columns", {
  expect_equal(
    arrange(df, carb, gear),
    data_arrange(df, c("carb", "gear"))
  )
  expect_equal(
    arrange(df, -carb, gear),
    data_arrange(df, c("-carb", "gear"))
  )
  expect_equal(
    arrange(df, -carb, desc(character)),
    data_arrange(df, c("-carb", "-character"))
  )
})

test_that("data_arrange works without columns", {
  expect_equal(data_arrange(df), df)
})

test_that("data_arrange ignores wrong names if safe = TRUE", {
  expect_equal(data_arrange(df, "foo"), df)
  expect_equal(
    data_arrange(df, c("gear", "foo")),
    data_arrange(df, "gear")
  )
})

test_that("data_arrange errors if safe = FALSE", {
  expect_error(data_arrange(df, "foo", safe = FALSE))
})

test_that("data_arrange errors if not coercable to data frame", {
  expect_error(data_arrange(list(a = 1:5, b = letters[1:3]), select = "b"))
  expect_equal(
    data_arrange(list(a = 1:5, b = letters[5:1]), select = "b"),
    structure(list(a = 5:1, b = c("a", "b", "c", "d", "e")), row.names = 5:1, class = "data.frame"),
    ignore_attr = TRUE
  )
})
