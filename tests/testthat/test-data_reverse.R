# explanation of how data_reverse works:
# https://github.com/easystats/datawizard/issues/106#issuecomment-1066628399

test_that("data_reverse works with numeric", {
  expect_equal(
    data_reverse(1:5),
    5:1
  )
  expect_equal(
    data_reverse(-2:2),
    2:-2
  )
})

test_that("data_reverse works with factor", {
  expect_equal(
    data_reverse(factor(1:5)),
    factor(5:1)
  )
  expect_equal(
    data_reverse(factor(-2:2)),
    factor(2:-2)
  )
})

test_that("data_reverse works with dataframe", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    data_reverse(test, select = "x"),
    data.frame(
      x = 5:1,
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_equal(
    data_reverse(test, exclude = "x"),
    data.frame(
      x = 1:5,
      y = c(6, 1, 7, 4, 8)
    )
  )
  expect_equal(
    data_reverse(test),
    data.frame(
      x = 5:1,
      y = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("data_reverse: arg 'select' works with formula", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    data_reverse(test, select = ~x),
    data.frame(
      x = 5:1,
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_equal(
    data_reverse(test, select = ~ x + y),
    data.frame(
      x = 5:1,
      y = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("data_reverse: arg 'exclude' works with formula", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    data_reverse(test, exclude = ~x),
    data.frame(
      x = 1:5,
      y = c(6, 1, 7, 4, 8)
    )
  )
  expect_equal(
    data_reverse(test, exclude = ~ x + y),
    test
  )
})

test_that("data_reverse: argument 'range' works", {
  expect_equal(
    data_reverse(c(1, 3, 4), range = c(0, 4)),
    c(3, 1, 0)
  )
  expect_equal(
    data_reverse(factor(c(1, 2, 3, 4, 5)), range = 0:10),
    factor(9:5, levels = 0:10)
  )

  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    data_reverse(test, select = "x", range = c(0, 8)),
    data.frame(
      x = 7:3,
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_equal(
    data_reverse(test, range = c(0, 8)),
    data.frame(
      x = 7:3,
      y = c(5, 0, 6, 3, 7)
    )
  )
})

test_that("data_reverse ignores NA", {
  expect_equal(
    data_reverse(c(1, 2, 8, NA)),
    c(8, 7, 1, NA)
  )
})

test_that("data_reverse returns NA if only NA provided", {
  expect_equal(
    data_reverse(c(NA_real_, NA_real_)),
    c(NA_real_, NA_real_)
  )
  expect_equal(
    data_reverse(factor(c(NA, NA))),
    factor(c(NA, NA))
  )
})

test_that("data_reverse warns if single value to reverse", {
  expect_warning(
    data_reverse(1),
    regexp = "A `range` must be provided for data with only one unique value."
  )
  expect_warning(
    data_reverse(factor(1)),
    regexp = "A `range` must be provided for data with only one unique value."
  )
})

test_that("data_reverse msg for unsupported", {
  expect_message(data_reverse(as.Date(c("2022-04-24", "2022-04-23"))))
})





# Same tests with reverse_scale (alias) --------------------------

test_that("reverse_scale works with numeric", {
  expect_equal(
    reverse_scale(1:5),
    5:1
  )
  expect_equal(
    reverse_scale(-2:2),
    2:-2
  )
})

test_that("reverse_scale works with factor", {
  expect_equal(
    reverse_scale(factor(1:5)),
    factor(5:1)
  )
  expect_equal(
    reverse_scale(factor(-2:2)),
    factor(2:-2)
  )
})

test_that("reverse_scale works with dataframe", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    reverse_scale(test, select = "x"),
    data.frame(
      x = 5:1,
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_equal(
    reverse_scale(test, exclude = "x"),
    data.frame(
      x = 1:5,
      y = c(6, 1, 7, 4, 8)
    )
  )
  expect_equal(
    reverse_scale(test),
    data.frame(
      x = 5:1,
      y = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("reverse_scale: arg 'select' works with formula", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    reverse_scale(test, select = ~x),
    data.frame(
      x = 5:1,
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_equal(
    reverse_scale(test, select = ~ x + y),
    data.frame(
      x = 5:1,
      y = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("reverse_scale: arg 'exclude' works with formula", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    reverse_scale(test, exclude = ~x),
    data.frame(
      x = 1:5,
      y = c(6, 1, 7, 4, 8)
    )
  )
  expect_equal(
    reverse_scale(test, exclude = ~ x + y),
    test
  )
})

test_that("reverse_scale: argument 'range' works", {
  expect_equal(
    reverse_scale(c(1, 3, 4), range = c(0, 4)),
    c(3, 1, 0)
  )
  expect_equal(
    reverse_scale(factor(c(1, 2, 3, 4, 5)), range = 0:10),
    factor(9:5, levels = 0:10)
  )

  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    reverse_scale(test, select = "x", range = c(0, 8)),
    data.frame(
      x = 7:3,
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_equal(
    reverse_scale(test, range = c(0, 8)),
    data.frame(
      x = 7:3,
      y = c(5, 0, 6, 3, 7)
    )
  )
})

test_that("reverse_scale ignores NA", {
  expect_equal(
    reverse_scale(c(1, 2, 8, NA)),
    c(8, 7, 1, NA)
  )
})

test_that("reverse_scale returns NA if only NA provided", {
  expect_equal(
    reverse_scale(c(NA_real_, NA_real_)),
    c(NA_real_, NA_real_)
  )
  expect_equal(
    reverse_scale(factor(c(NA, NA))),
    factor(c(NA, NA))
  )
})

test_that("reverse_scale warns if single value to reverse", {
  expect_warning(
    reverse_scale(1),
    regexp = "A `range` must be provided for data with only one unique value."
  )
  expect_warning(
    reverse_scale(factor(1)),
    regexp = "A `range` must be provided for data with only one unique value."
  )
})


test_that("reverse_scale select helpers", {
  data(iris)
  out <- data_rescale(iris, to = list(
    "Sepal.Length" = c(0, 1),
    "Petal.Length" = c(-1, 0)
  ), select = ends_with("length"))

  expect_equal(out$Sepal.Length, iris$Sepal.Length, tolerance = 1e-3)

  out <- data_rescale(iris, to = list(
    "Sepal.Length" = c(0, 1),
    "Petal.Length" = c(-1, 0)
  ), select = ends_with("length"), ignore_case = TRUE)

  expect_equal(head(out$Sepal.Length), c(0.22222, 0.16667, 0.11111, 0.08333, 0.19444, 0.30556), tolerance = 1e-3)
})
