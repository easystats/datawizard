foo <- function(data, select = NULL, exclude = NULL) {
  .select_nse(select, data, exclude = exclude, ignore_case = FALSE)
}

test_that(".select_nse: arg 'select' works", {
  expect_equal(
    foo(iris, select = NULL),
    names(iris)
  )
  expect_equal(
    foo(iris, c("Petal.Length", "Sepal.Width")),
    c("Petal.Length", "Sepal.Width")
  )
  expect_equal(
    foo(iris, c(3, 2)),
    c("Petal.Length", "Sepal.Width")
  )
  expect_equal(
    foo(iris, 1:5),
    names(iris)
  )
  expect_equal(
    foo(iris, starts_with("Petal")),
    c("Petal.Length", "Petal.Width")
  )
  expect_equal(
    foo(iris, ends_with("Length")),
    c("Sepal.Length", "Petal.Length")
  )
  expect_equal(
    foo(iris, contains("Length")),
    c("Sepal.Length", "Petal.Length")
  )
  expect_equal(
    foo(iris, regex("Length$")),
    c("Sepal.Length", "Petal.Length")
  )
})


test_that(".select_nse: arg 'exclude' works", {
  expect_equal(
    foo(iris, exclude = c("Petal.Length", "Sepal.Width")),
    c("Sepal.Length", "Petal.Width", "Species")
  )
  expect_equal(
    foo(iris, exclude = c(3, 2)),
    c("Sepal.Length", "Petal.Width", "Species")
  )
  expect_equal(
    foo(iris, exclude = starts_with("Petal")),
    c("Sepal.Length", "Sepal.Width", "Species")
  )
  expect_equal(
    foo(iris, exclude = ends_with("Length")),
    c("Sepal.Width", "Petal.Width", "Species")
  )
  expect_equal(
    foo(iris, exclude = contains("Length")),
    c("Sepal.Width", "Petal.Width", "Species")
  )
  expect_equal(
    foo(iris, exclude = regex("Length$")),
    c("Sepal.Width", "Petal.Width", "Species")
  )
})

test_that(".select_nse: args 'select' and 'exclude' at the same time", {
  expect_equal(
    foo(iris, select = contains("Length"), exclude = starts_with("Petal")),
    c("Sepal.Length")
  )
  expect_equal(
    foo(iris, select = contains("Length"), exclude = contains("Length")),
    character(0)
  )
})
