test <- head(iris)

# basic tests --------------

test_that("data_rename works with one or several replacements", {
  expect_equal(
    names(data_rename(test, "Sepal.Length", "length")),
    c("length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
  expect_equal(
    names(data_rename(
      test, c("Sepal.Length", "Sepal.Width"),
      c("length", "width")
    )),
    c("length", "width", "Petal.Length", "Petal.Width", "Species")
  )
})

test_that("data_rename returns a dataframe", {
  x <- data_rename(test, "Sepal.Length", "length")
  expect_true(class(x) == "data.frame")
})

test_that("data_rename: pattern must be of type character", {
  expect_error(
    data_rename(test, pattern = 1),
    regexp = "Argument 'pattern' must be of type character."
  )
  expect_error(
    data_rename(test, pattern = TRUE),
    regexp = "Argument 'pattern' must be of type character."
  )
})

# replacement -------------

test_that("data_rename uses indices when no replacement", {
  x <- data_rename(test, pattern = c("Sepal.Length", "Petal.Length"))
  expect_equal(dim(test), dim(x))
  expect_equal(names(x), c("1", "Sepal.Width", "2", "Petal.Width", "Species"))
})

test_that("data_rename works when too many names in 'replacement'", {
  expect_message(
    x <- data_rename(test, replacement = paste0("foo", 1:6)),
    "There are more names in 'replacement' than in 'pattern'. The last 1 names of
  'replacement' are not used."
  )
  expect_equal(dim(test), dim(x))
  expect_equal(names(x), paste0("foo", 1:5))
})

test_that("data_rename works when not enough names in 'replacement'", {
  expect_message(
    x <- data_rename(test, replacement = paste0("foo", 1:2)),
    "There are more names in 'pattern' than in 'replacement'. The last 3 names of
  'pattern' are not modified."
  )
  expect_equal(dim(test), dim(x))
  expect_equal(names(x), c("foo1", "foo2", "Petal.Length", "Petal.Width", "Species"))
})


# no pattern --------------

test_that("data_rename uses the whole dataset when pattern = NULL", {
  x1 <- data_rename(test)
  x2 <- data_rename(test, pattern = names(test))
  expect_equal(dim(test), dim(x1))
  expect_identical(x1, x2)

  x3 <- data_rename(test, replacement = paste0("foo", 1:5))
  x4 <- data_rename(test, pattern = names(test), replacement = paste0("foo", 1:5))
  expect_equal(dim(test), dim(x3))
  expect_identical(x3, x4)
})


# other --------------

test_that("data_rename: argument 'safe' works", {
  expect_silent(
    data_rename(iris, "FakeCol", "length", safe = TRUE)
  )
  expect_error(
    data_rename(iris, "FakeCol", "length", safe = FALSE),
    "Variable 'FakeCol' is not in your dataframe"
  )
})

test_that("data_rename deals correctly with duplicated replacement", {
  x <- data_rename(test,
    pattern = names(test)[1:4],
    replacement = c("foo", "bar", "foo", "bar")
  )
  expect_equal(dim(test), dim(x))
  expect_equal(names(x)[1:4], c("foo", "bar", "foo.2", "bar.2"))
})

test_that("data_rename doesn't change colname if invalid pattern", {
  expect_equal(
    names(data_rename(test, "FakeCol", "length")),
    names(test)
  )
})
