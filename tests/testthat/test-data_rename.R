test <- head(iris)

# no pattern --------------

test_that("data_rename works when no pattern and no replacement", {
  x <- data_rename(test)
  expect_equal(dim(test), dim(x))
  expect_equal(names(x), paste0("col", 1:5))
})

test_that("data_rename works when no pattern and not enough replacement", {
  expect_message(
    x <- data_rename(test, replacement = c("foo", "bar")),
    "There are more names in 'pattern' than in 'replacement'. The last 3 names of 'pattern' are not modified."
  )
  expect_equal(dim(test), dim(x))
  expect_equal(names(x[, 1:2]), c("foo", "bar"))
  expect_equal(names(x[, 3:5]), names(test[, 3:5]))
})

test_that("data_rename works when no pattern and too many replacement", {
  expect_message(
    x <- data_rename(test, replacement = c("foo", "bar")),
    "There are more names in 'pattern' than in 'replacement'. The last 3 names of 'pattern' are not modified."
  )
  expect_equal(dim(test), dim(x))
  expect_equal(names(x[, 1:2]), c("foo", "bar"))
  expect_equal(names(x[, 3:5]), names(test[, 3:5]))
})






expect_error(
  data_rename(iris, "FakeCol", "length", safe = FALSE),
  "Variable 'FakeCol' is not in your dataframe"
)

expect_equal(
  names(data_rename(iris, "Sepal.Length", "length")),
  c("length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
)

expect_equal(
  names(data_rename(iris, "FakeCol", "length")),
  names(iris)
)

expect_equal(
  names(data_rename(iris, c("Sepal.Length", "Sepal.Width"), c("length", "width"))),
  c("length", "width", "Petal.Length", "Petal.Width", "Species")
)

expect_equal(
  names(data_rename(iris, NULL)),
  c("1", "2", "3", "4", "5")
)

expect_equal(
  names(data_rename(iris, paste0("Var", 1:5))),
  paste0("Var", 1:5)
)

expect_error(
  data_rename(iris, replacement = "foo"),
  regexp = "The 'replacement' names must be of the same length than the variable names."
)
