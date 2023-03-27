test_that("assign_labels, unnamed values", {
  x <- 1:3
  # labelling by providing required number of labels
  out <- assign_labels(
    x,
    variable = "My x",
    values = c("one", "two", "three")
  )
  expect_identical(attributes(out)$label, "My x")
  expect_identical(attributes(out)$labels, structure(1:3, names = c("one", "two", "three")))
})

test_that("assign_labels, named values", {
  # labelling using named vectors
  x <- factor(letters[1:3])
  out <- assign_labels(
    x,
    variable = "Labelled factor",
    values = c(`a` = "low", `b` = "mid", `c` = "high")
  )
  expect_identical(attributes(out)$label, "Labelled factor")
  expect_identical(attributes(out)$labels, c(low = "a", mid = "b", high = "c"))
})

test_that("assign_labels, partially named values", {
  x <- 1:5
  out <- assign_labels(
    x,
    variable = "My x",
    values = c(`1` = "lowest", `5` = "highest"),
    verbose = FALSE
  )
  expect_identical(attributes(out)$label, "My x")
  expect_identical(attributes(out)$labels, c(lowest = 1, highest = 5))
})

test_that("assign_labels, errors", {
  x <- 1:5
  expect_error(assign_labels(x, values = c(`1` = "lowest", `6` = "highest")))
  expect_error(assign_labels(x, variable = 1, values = c(`1` = "lowest", `6` = "highest")))
  expect_error(assign_labels(x, values = c("a", "b", "c")))
})

test_that("assign_labels, data frame", {
  data(iris)
  out <- assign_labels(iris, "Species", values = c("a", "b", "c"))
  expect_identical(attributes(out$Species)$labels, c(a = "setosa", b = "versicolor", c = "virginica"))

  data(mtcars)
  out <- assign_labels(mtcars, select = c("am", "vs"), values = c("low", "high"))
  expect_identical(attributes(out$am)$labels, c(low = 0, high = 1))
  expect_identical(attributes(out$vs)$labels, c(low = 0, high = 1))
  expect_null(attributes(out$gear)$labels)
  expect_null(attributes(out$cyl)$labels)
})
