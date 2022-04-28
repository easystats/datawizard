test_that("unnormalize work as expected", {
  x <- normalize(c(0, 1, 5, -5, -2))
  expect_equal(
    unnormalize(x),
    c(0, 1, 5, -5, -2),
    ignore_attr = TRUE
  )
  expect_warning(expect_equal(unnormalize(c(0, 1, 5, -5, -2)), c(0, 1, 5, -5, -2), ignore_attr = TRUE))
})
