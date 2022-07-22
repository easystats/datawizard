suppressPackageStartupMessages(library(poorman, warn.conflicts = FALSE))

# explanation of how reverse works:
# https://github.com/easystats/datawizard/issues/106#issuecomment-1066628399

test_that("reverse works with numeric", {
  expect_equal(
    reverse(1:5),
    5:1
  )
  expect_equal(
    reverse(-2:2),
    2:-2
  )
})

test_that("reverse works with factor", {
  expect_equal(
    reverse(factor(1:5)),
    factor(5:1)
  )
  expect_equal(
    reverse(factor(-2:2)),
    factor(2:-2)
  )
})

test_that("reverse works with data frame", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    reverse(test, select = "x"),
    data.frame(
      x = 5:1,
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_equal(
    reverse(test, exclude = "x"),
    data.frame(
      x = 1:5,
      y = c(6, 1, 7, 4, 8)
    )
  )
  expect_equal(
    reverse(test),
    data.frame(
      x = 5:1,
      y = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("reverse: arg 'select' works with formula", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    reverse(test, select = ~x),
    data.frame(
      x = 5:1,
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_equal(
    reverse(test, select = ~ x + y),
    data.frame(
      x = 5:1,
      y = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("reverse: arg 'exclude' works with formula", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    reverse(test, exclude = ~x),
    data.frame(
      x = 1:5,
      y = c(6, 1, 7, 4, 8)
    )
  )
  expect_equal(
    reverse(test, exclude = ~ x + y),
    test
  )
})

test_that("reverse: argument 'range' works", {
  expect_equal(
    reverse(c(1, 3, 4), range = c(0, 4)),
    c(3, 1, 0)
  )
  expect_equal(
    reverse(factor(c(1, 2, 3, 4, 5)), range = 0:10),
    factor(9:5, levels = 0:10)
  )

  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_equal(
    reverse(test, select = "x", range = c(0, 8)),
    data.frame(
      x = 7:3,
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_equal(
    reverse(test, range = c(0, 8)),
    data.frame(
      x = 7:3,
      y = c(5, 0, 6, 3, 7)
    )
  )
})

test_that("reverse ignores NA", {
  expect_equal(
    reverse(c(1, 2, 8, NA)),
    c(8, 7, 1, NA)
  )
})

test_that("reverse returns NA if only NA provided", {
  expect_equal(
    reverse(c(NA_real_, NA_real_)),
    c(NA_real_, NA_real_)
  )
  expect_equal(
    reverse(factor(c(NA, NA))),
    factor(c(NA, NA))
  )
})

test_that("reverse warns if single value to reverse", {
  expect_warning(
    reverse(1),
    regexp = "A `range` must be provided for data with only one unique value."
  )
  expect_warning(
    reverse(factor(1)),
    regexp = "A `range` must be provided for data with only one unique value."
  )
})

test_that("reverse msg for unsupported", {
  expect_message(reverse(as.Date(c("2022-04-24", "2022-04-23"))))
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

test_that("reverse_scale works with data frame", {
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
  out <- rescale(iris, to = list(
    "Sepal.Length" = c(0, 1),
    "Petal.Length" = c(-1, 0)
  ), select = ends_with("length"))

  expect_equal(out$Sepal.Length, iris$Sepal.Length, tolerance = 1e-3)

  out <- rescale(iris, to = list(
    "Sepal.Length" = c(0, 1),
    "Petal.Length" = c(-1, 0)
  ), select = ends_with("length"), ignore_case = TRUE)

  expect_equal(head(out$Sepal.Length), c(0.22222, 0.16667, 0.11111, 0.08333, 0.19444, 0.30556), tolerance = 1e-3)
})




# with grouped data -------------------------------------------

set.seed(123)
value1 <- sample(1:10, 6, replace = TRUE)
set.seed(456)
value2 <- sample(1:10, 6, replace = TRUE)

test_df <- data.frame(
  id = rep(c("A", "B"), each = 3),
  value1 = value1,
  value2 = value2
)

test_that("reverse works with data frames (grouped data)", {
  expect_equal(
    test_df %>%
      group_by(id) %>%
      reverse(exclude = "id") %>%
      ungroup(),
    data.frame(
      id = rep(c("A", "B"), each = 3),
      value1 = c(10, 10, 3, 6, 2, 3),
      value2 = c(4, 6, 3, 10, 6, 5)
    )
  )
})


set.seed(789)
value1 <- sample(c(1:10, NA), 6, replace = TRUE)
set.seed(10)
value2 <- sample(c(1:10, NA), 6, replace = TRUE)

test_df <- data.frame(
  id = rep(c("A", "B"), each = 3),
  value1 = value1,
  value2 = value2
)

test_that("reverse works with data frames containing NAs (grouped data)", {
  expect_equal(
    test_df %>%
      group_by(id) %>%
      reverse(exclude = "id") %>%
      ungroup(),
    data.frame(
      id = rep(c("A", "B"), each = 3),
      value1 = c(10, 4, 4, 5, 3, 4),
      value2 = c(NA, 10, 9, 7, 6, 8)
    )
  )
})
