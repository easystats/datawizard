test_that("labels_to_levels, numeric", {
  expect_message(
    labels_to_levels(1:10),
    regex = "only works"
  )
})

test_that("labels_to_levels, factor", {
  data(efc)
  x <- as.factor(efc$c172code)
  attr(x, "labels") <- c("low" = 1, "mid" = 2, "high" = 3)
  x <- datawizard::labels_to_levels(x)
  expect_identical(levels(x), c("low", "mid", "high"))
  expect_equal(table(x), table(efc$c172code), ignore_attr = TRUE)

  x <- as.ordered(efc$c172code)
  attr(x, "labels") <- c("low" = 1, "mid" = 2, "high" = 3)
  x <- datawizard::labels_to_levels(x)
  expect_identical(levels(x), c("low", "mid", "high"))
  expect_s3_class(x, "ordered")
})

test_that("labels_to_levels, factor, error on no labels", {
  data(efc)
  data(iris)
  x <- as.factor(efc$c172code)
  expect_error(labels_to_levels(x), regex = "Could not change factor")
  expect_error(labels_to_levels(iris), regex = "Could not change factor")
})

test_that("labels_to_levels, factor, data frame", {
  data(efc)
  out <- labels_to_levels(efc)
  expect_identical(
    levels(out$e42dep),
    c(
      "independent", "slightly dependent", "moderately dependent",
      "severely dependent"
    )
  )
  expect_identical(sum(vapply(efc, is.factor, TRUE)), 1L)
})
