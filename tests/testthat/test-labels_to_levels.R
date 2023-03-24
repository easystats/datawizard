test_that("labels_to_levels, numeric", {
  expect_error(
    labels_to_levels(1:10),
    regex = "Converting into factors"
  )
})

test_that("labels_to_levels, factor", {
  data(efc)
  x <- as.factor(efc$c172code)
  attr(x, "labels") <- c("low" = 1, "mid" = 2, "high" = 3)
  x <- datawizard::labels_to_levels(x)
  expect_identical(levels(x), c("low", "mid", "high"))
  expect_equal(table(x), table(efc$c172code), ignore_attr = TRUE)
})

test_that("labels_to_levels, factor, error on no labels", {
  data(efc)
  x <- as.factor(efc$c172code)
  expect_error(labels_to_levels(x), regex = "Could not change factor")
})
