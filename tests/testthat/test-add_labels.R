test_that("add_labels, unnamed values", {
  x <- 1:3
  # labelling by providing required number of labels
  out <- add_labels(
    x,
    variable = "My x",
    values = c("one", "two", "three")
  )
  expect_identical(attributes(out)$label, "My x")
  expect_identical(attributes(out)$labels, structure(1:3, names = c("one", "two", "three")))
})

test_that("add_labels, named values", {
  # labelling using named vectors
  x <- factor(letters[1:3])
  out <- add_labels(
    x,
    variable = "Labelled factor",
    values = c(`a` = "low", `b` = "mid", `c` = "high")
  )
  expect_identical(attributes(out)$label, "Labelled factor")
  expect_identical(attributes(out)$labels, c(low = "a", mid = "b", high = "c"))
})

test_that("add_labels, partially named values", {
  x <- 1:5
  out <- add_labels(
    x,
    variable = "My x",
    values = c(`1` = "lowest", `5` = "highest"),
    verbose = FALSE
  )
  expect_identical(attributes(out)$label, "My x")
  expect_identical(attributes(out)$labels, c(lowest = 1, highest = 5))
})

test_that("add_labels, messages", {
  x <- 1:5
  expect_message(add_labels(x, values = c(`1` = "lowest", `6` = "highest")))
  expect_message(add_labels(x, variable = 1, values = c(`1` = "lowest", `6` = "highest")))
  expect_message(add_labels(x, values = c("a", "b", "c")))
})

test_that("add_labels, data frame", {
  data(iris)
  out <- add_labels(iris, "Species", values = c("a", "b", "c"))
  expect_identical(attributes(out$Species)$labels, c(a = "setosa", b = "versicolor", c = "virginica"))
})
