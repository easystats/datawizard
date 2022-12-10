# Preparations

df1 <- data.frame(
  id = c(1, 2, 3, 1, 3),
  year = c(2022, 2022, 2022, 2022, 2000),
  item1 = c(NA, 1, 1, 2, 3),
  item2 = c(NA, 1, 1, 2, 3),
  item3 = c(NA, 1, 1, 2, 3)
)

expected1 <- data.frame(
  Row = c(1, 4, 3, 5),
  id = c(1, 1, 3, 3),
  year = c(2022, 2022, 2022, 2000),
  item1 = c(NA, 2, 1, 3),
  item2 = c(NA, 2, 1, 3),
  item3 = c(NA, 2, 1, 3),
  count_na = c(3, 0, 0, 0)
)

expected2 <- data.frame(
  Row = c(1, 4),
  id = c(1, 1),
  year = c(2022, 2022),
  item1 = c(NA, 2),
  item2 = c(NA, 2),
  item3 = c(NA, 2),
  count_na = c(3, 0)
)

# Testing

test_that("data_duplicated basic", {
  x <- data_duplicated(df1, select = "id")
  rownames(x) <- NULL
  expect_equal(
    x,
    expected1
  )
})

test_that("data_duplicated unquoted", {
  x <- data_duplicated(df1, select = id)
  rownames(x) <- NULL
  expect_equal(
    x,
    expected1
  )
})

test_that("data_duplicated vector", {
  x <- data_duplicated(df1, select = 1)
  rownames(x) <- NULL
  expect_equal(
    x,
    expected1
  )
})

test_that("data_duplicated select-helper", {
  x <- data_duplicated(df1, select = contains("id"))
  rownames(x) <- NULL
  expect_equal(
    x,
    expected1
  )
})

test_that("data_duplicated multiple IDs", {
  x <- data_duplicated(df1, select = c("id", "year"))
  rownames(x) <- NULL
  expect_equal(
    x,
    expected2
  )
})

test_that("data_duplicated multiple IDs formula", {
  x <- data_duplicated(df1, select = ~ id + year)
  rownames(x) <- NULL
  expect_equal(
    x,
    expected2
  )
})

test_that("data_duplicated multiple IDs vector", {
  x <- data_duplicated(df1, select = 1:2)
  rownames(x) <- NULL
  expect_equal(
    x,
    expected2
  )
})

test_that("data_unique works with groups", {
  df <- data.frame(
    g = c(1, 1, 2, 2),
    x = c(1, 1, 2, 1)
  )
  df <- data_group(df, "g")

  expected <- data.frame(
    Row = 1:2,
    g = c(1, 1),
    x = c(1, 1),
    count_na = c(0, 0)
  )
  expected <- data_group(expected, "g")

  expect_identical(data_duplicated(df, "x"), expected, ignore_attr = TRUE)
})
