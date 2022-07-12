# numeric
test_that("data_shift", {
  x <- c(10, 11, 12)
  expect_equal(data_shift(x), c(0, 1, 2))

  x <- c(10, 11, 12)
  expect_equal(data_shift(x, lowest = 10), x)

  x <- c(10, 11, 12)
  expect_equal(data_shift(x, lowest = 1), c(1, 2, 3))

  x <- c(10, 11, NA, 12)
  expect_equal(data_shift(x, lowest = 1), c(1, 2, NA, 3))
})

# factor
test_that("data_shift", {
  data(efc)
  expect_message(expect_equal(data_shift(efc$e42dep), efc$e42dep))
})

# data frame
test_that("data_shift", {
  data(iris)
  out <- data_shift(iris)
  expect_equal(out$Species, iris$Species)
  expect_equal(range(out$Sepal.Length), c(0, 3.6), tolerance = 1e-2)
})
