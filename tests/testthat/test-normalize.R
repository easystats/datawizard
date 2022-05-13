test_that("normalize work as expected", {
  expect_equal(
    normalize(c(0, 1, 5, -5, -2)),
    c(0.5, 0.6, 1, 0, 0.3),
    ignore_attr = TRUE
  )

  expect_equal(
    normalize(c(0, 1, 5, -5, -2), include_bounds = FALSE),
    c(0.5, 0.58, 0.9, 0.1, 0.34),
    ignore_attr = TRUE
  )

  expect_snapshot(head(normalize(trees)))
})


test_that("normalize: only NAs", {
  expect_equal(
    normalize(c(NA_real_, NA_real_)),
    c(NA_real_, NA_real_),
    ignore_attr = TRUE
  )
})

test_that("normalize: only one value", {
  foo <- c(1)
  expect_warning(
    normalize(x = foo),
    regexp = "Variable `foo` contains only one unique value and will"
  )
  expect_warning(
    y <- normalize(x = 12),
    regexp = "Variable `12` contains only one unique value and will"
  )
  expect_equal(y, 12, ignore_attr = TRUE)

  expect_silent(normalize(x = foo, verbose = FALSE))
  expect_equal(normalize(x = foo, verbose = FALSE), 1, ignore_attr = TRUE)
})

test_that("normalize: only two values", {
  expect_warning(
    y <- normalize(x = c(1, 2))
  )
  expect_equal(y, c(0, 1), ignore_attr = TRUE)

  expect_silent(normalize(x = c(1, 2), verbose = FALSE))
  expect_equal(normalize(x = c(1, 2), verbose = FALSE), c(0, 1), ignore_attr = TRUE)
})

test_that("normalize: factor", {
  expect_equal(
    normalize(factor(c(1:3))),
    factor(1:3)
  )
})

test_that("normalize: matrix", {
  expect_equal(
    normalize(matrix(1:4, ncol = 2)),
    matrix(seq(0, 1, by = 0.3333), ncol = 2),
    tolerance = 1e-3
  )
})
