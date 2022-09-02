test_that("unnormalize work as expected", {
  x <- normalize(c(0, 1, 5, -5, -2))
  expect_equal(
    unnormalize(x),
    c(0, 1, 5, -5, -2),
    ignore_attr = TRUE
  )
  expect_warning(expect_equal(unnormalize(c(0, 1, 5, -5, -2)), c(0, 1, 5, -5, -2), ignore_attr = TRUE))
})


test_that("unnormalize and unstandardized x 4", {
  x <- rnorm(6, 4, 10)

  z <- standardise(x)
  # attributes(z)
  expect_equal(unstandardise(z), x, ignore_attr = TRUE)

  z <- center(x)
  # attributes(z)
  expect_equal(unstandardise(z), x, ignore_attr = TRUE)


  z <- normalize(x)
  # attributes(z)
  expect_equal(unnormalize(z), x, ignore_attr = TRUE)

  z <- change_scale(x, to = c(-3, 14.5))
  # attributes(z)
  expect_equal(unnormalize(z), x, ignore_attr = TRUE)

  z <- change_scale(x, range = c(-100, 100))
  # attributes(z)
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
