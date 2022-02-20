test_that("describe_distribution - numeric", {
  x <- describe_distribution(rnorm(100))
  expect_equal(c(nrow(x), ncol(x)), c(1, 9))
})

test_that("describe_distribution - factor", {
  expect_snapshot(describe_distribution(factor(substring("statistics", 1:10, 1:10))))
})

test_that("describe_distribution - character", {
  expect_snapshot(describe_distribution(as.character(ToothGrowth$supp)))
})
