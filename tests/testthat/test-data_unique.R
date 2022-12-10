# Preparations

df1 <- data.frame(
  id = c(1, 2, 3, 1, 3),
  year = c(2022, 2022, 2022, 2022, 2000),
  item1 = c(NA, 1, 1, 2, 3),
  item2 = c(NA, 1, 1, 2, 3),
  item3 = c(NA, 1, 1, 2, 3)
)

expected1 <- data.frame(
  id = c(1, 2, 3),
  year = c(2022, 2022, 2022),
  item1 = c(2, 1, 1),
  item2 = c(2, 1, 1),
  item3 = c(2, 1, 1)
)

expected2 <- data.frame(
  id = c(1, 2, 3),
  year = c(2022, 2022, 2022),
  item1 = c(NA, 1, 1),
  item2 = c(NA, 1, 1),
  item3 = c(NA, 1, 1)
)

expected3 <- data.frame(
  id = c(1, 2, 3),
  year = c(2022, 2022, 2000),
  item1 = c(2, 1, 3),
  item2 = c(2, 1, 3),
  item3 = c(2, 1, 3)
)

expected4 <- data.frame(
  id = c(1, 2, 3, 3),
  year = c(2022, 2022, 2022, 2000),
  item1 = c(2, 1, 1, 3),
  item2 = c(2, 1, 1, 3),
  item3 = c(2, 1, 1, 3)
)

# Testing

test_that("data_unique returns original data if no duplicates", {
  test <- data.frame(x = c(1, 2), y = c(3, 4))
  expect_identical(
    data_unique(test, c("x", "y")),
    test
  )
  expect_identical(
    data_unique(test, "x"),
    test
  )
})

test_that("data_unique basic", {
  expect_equal(
    data_unique(df1, select = "id"),
    expected1
  )
})

test_that("data_unique basic method best", {
  expect_equal(
    data_unique(df1, select = "id", keep = "best"),
    expected1
  )
})

test_that("data_unique basic method first", {
  expect_equal(
    data_unique(df1, select = "id", keep = "first"),
    expected2
  )
})

test_that("data_unique basic method last", {
  expect_equal(
    data_unique(df1, select = "id", keep = "last"),
    expected3
  )
})

test_that("data_unique unquoted", {
  expect_equal(
    data_unique(df1, select = id),
    expected1
  )
})

test_that("data_unique vector", {
  expect_equal(
    data_unique(df1, select = 1),
    expected1
  )
})

test_that("data_unique select-helper", {
  expect_equal(
    data_unique(df1, select = contains("id")),
    expected1
  )
})

test_that("data_unique multiple IDs", {
  x <- data_unique(df1, select = c("id", "year"))
  rownames(x) <- NULL
  expect_equal(
    x,
    expected4
  )
})

test_that("data_unique multiple IDs formula", {
  x <- data_unique(df1, select = ~ id + year)
  rownames(x) <- NULL
  expect_equal(
    x,
    expected4
  )
})

test_that("data_unique multiple IDs vector", {
  x <- data_unique(df1, select = 1:2)
  rownames(x) <- NULL
  expect_equal(
    x,
    expected4
  )
})

test_that("data_unique preserve attributes", {
  attr(df1, "testing") <- "custom.attribute"
  x <- attributes(data_unique(df1, id))
  expect_equal(
    x$testing,
    "custom.attribute"
  )
})

test_that("data_unique, arg 'verbose' works", {
  expect_silent(
    data_unique(df1, select = ~ id + year, verbose = FALSE)
  )
})

test_that("data_unique works with groups", {
  df <- data.frame(
    g = c(1, 1, 2, 2),
    x = c(1, 1, 2, 1)
  )
  df <- data_group(df, "g")

  expected <- data.frame(
    g = c(1, 2, 2),
    x = c(1, 2, 1)
  )
  expected <- data_group(expected, "g")

  x <- data_unique(df, "x")
  expect_identical(x, expected, ignore_attr = TRUE)

  y <- attributes(x)

  expect_identical(attributes(df)$class, y$class)
  expect_identical(attributes(df)$groups, y$groups)
})
