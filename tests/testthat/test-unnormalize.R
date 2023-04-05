test_that("unnormalize work as expected", {
  x <- normalize(c(0, 1, 5, -5, -2))
  expect_equal(
    unnormalize(x),
    c(0, 1, 5, -5, -2),
    ignore_attr = TRUE
  )
  expect_warning(expect_equal(unnormalize(c(0, 1, 5, -5, -2)), c(0, 1, 5, -5, -2), ignore_attr = TRUE))
})

test_that("unnormalize error if not supported", {
  expect_error(
    unnormalize(c("a", "b")),
    "can't be unnormalized"
  )
})

test_that("unnormalize and unstandardized x 4", {
  set.seed(123)
  x <- rnorm(6, 4, 10)

  z <- standardise(x)
  expect_named(attributes(z), c("center", "scale", "robust", "class"))
  expect_equal(attributes(z)$center, 8.47, tolerance = 0.01)
  expect_equal(unstandardise(z), x, ignore_attr = TRUE)

  z <- center(x)
  expect_named(attributes(z), c("center", "scale", "robust", "class"))
  expect_equal(unstandardise(z), x, ignore_attr = TRUE)


  z <- normalize(x)
  expect_named(attributes(z), c(
    "include_bounds", "flag_bounds", "min_value",
    "vector_length", "range_difference", "class"
  ))
  expect_equal(unnormalize(z), x, ignore_attr = TRUE)

  z <- change_scale(x, to = c(-3, 14.5))
  expect_named(attributes(z), c(
    "min_value", "max_value", "new_min", "new_max",
    "range_difference", "to_range", "class"
  ))
  expect_equal(unnormalize(z), x, ignore_attr = TRUE)

  z <- change_scale(x, range = c(-100, 100))
  expect_named(attributes(z), c(
    "min_value", "max_value", "new_min", "new_max",
    "range_difference", "to_range", "class"
  ))
  expect_equal(unnormalize(z), x, ignore_attr = TRUE)
})

# select helpers ------------------------------
test_that("unnormalize regex", {
  x <- normalize(mtcars, select = "mpg")
  expect_equal(
    unnormalize(x, select = "pg", regex = TRUE),
    unnormalize(x, select = "mpg")
  )
})
