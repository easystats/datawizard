test_that("normalize work as expected", {
  expect_equal(
    normalize(c(0, 1, 5, -5, -2)),
    c(0.5, 0.6, 1, 0, 0.3)
  )

  expect_equal(
    normalize(c(0, 1, 5, -5, -2), include_bounds = FALSE),
    c(0.5, 0.58, 0.9, 0.1, 0.34)
  )

  expect_snapshot(head(normalize(trees)))
})
