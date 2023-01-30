context("sjlabelled, as_numeric")

library(sjlabelled)

test_that("as_numeric", {
  expect_equal(as_numeric(factor(c(0,1,2)), keep.labels = FALSE), c(0,1,2))
  expect_equal(as_numeric(factor(c(2,3,4)), keep.labels = FALSE), c(2,3,4))
  expect_equal(as_numeric(factor(c("a", "b", "c")), keep.labels = FALSE), c(1,2,3))
  expect_equal(as_numeric(factor(c("d", "e", "f")), keep.labels = FALSE), c(1,2,3))
})

test_that("as_numeric", {
  expect_equal(as_numeric(factor(c(0,1,2)), start.at = 4, keep.labels = FALSE), c(4,5,6))
  expect_equal(as_numeric(factor(c(2,3,4)), start.at = 4, keep.labels = FALSE), c(4,5,6))
  expect_equal(as_numeric(factor(c("a", "b", "c")), start.at = 4, keep.labels = FALSE), c(4,5,6))
  expect_equal(as_numeric(factor(c("d", "e", "f")), start.at = 4, keep.labels = FALSE), c(4,5,6))
})
