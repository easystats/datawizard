# explanation of how reverse works:
# https://github.com/easystats/datawizard/issues/106#issuecomment-1066628399

test_that("reverse works with numeric", {
  expect_identical(
    reverse(1:5),
    as.double(5:1)
  )
  expect_identical(
    reverse(-2:2),
    as.double(2:-2)
  )
})

test_that("reverse works with factor", {
  expect_identical(
    reverse(factor(1:5)),
    factor(5:1)
  )
  expect_identical(
    reverse(factor(-2:2)),
    factor(2:-2)
  )
})

test_that("reverse works with data frame", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_identical(
    reverse(test, select = "x"),
    data.frame(
      x = as.double(5:1),
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_identical(
    reverse(test, exclude = "x"),
    data.frame(
      x = 1:5,
      y = c(6, 1, 7, 4, 8)
    )
  )
  expect_identical(
    reverse(test),
    data.frame(
      x = as.double(5:1),
      y = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("reverse works with data frame and append", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_identical(
    reverse(test, select = "x", append = TRUE),
    data.frame(
      x = 1:5,
      y = c(3, 8, 2, 5, 1),
      x_r = as.double(5:1)
    )
  )
  expect_identical(
    reverse(test, append = TRUE),
    data.frame(
      x = 1:5,
      y = c(3, 8, 2, 5, 1),
      x_r = as.double(5:1),
      y_r = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("reverse: arg 'select' works with formula", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_identical(
    reverse(test, select = ~x),
    data.frame(
      x = as.double(5:1),
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_identical(
    reverse(test, select = ~ x + y),
    data.frame(
      x = as.double(5:1),
      y = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("reverse: arg 'exclude' works with formula", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_identical(
    reverse(test, exclude = ~x),
    data.frame(
      x = 1:5,
      y = c(6, 1, 7, 4, 8)
    )
  )
  expect_identical(
    reverse(test, exclude = ~ x + y),
    test
  )
})

test_that("reverse: argument 'range' works", {
  expect_identical(
    reverse(c(1, 3, 4), range = c(0, 4)),
    c(3, 1, 0)
  )
  expect_identical(
    reverse(factor(c(1, 2, 3, 4, 5)), range = 0:10),
    factor(9:5, levels = 0:10)
  )

  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_identical(
    reverse(test, select = "x", range = c(0, 8)),
    data.frame(
      x = as.double(7:3),
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_identical(
    reverse(test, range = c(0, 8)),
    data.frame(
      x = as.double(7:3),
      y = c(5, 0, 6, 3, 7)
    )
  )
})

test_that("reverse ignores NA", {
  expect_identical(
    reverse(c(1, 2, 8, NA)),
    c(8, 7, 1, NA)
  )
})

test_that("reverse returns NA if only NA provided", {
  expect_identical(
    reverse(c(NA_real_, NA_real_)),
    c(NA_real_, NA_real_)
  )
  expect_identical(
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
  expect_identical(
    reverse_scale(1:5),
    as.double(5:1)
  )
  expect_identical(
    reverse_scale(-2:2),
    as.double(2:-2)
  )
})

test_that("reverse_scale works with factor", {
  expect_identical(
    reverse_scale(factor(1:5)),
    factor(5:1)
  )
  expect_identical(
    reverse_scale(factor(-2:2)),
    factor(2:-2)
  )
})

test_that("reverse_scale works with data frame", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_identical(
    reverse_scale(test, select = "x"),
    data.frame(
      x = as.double(5:1),
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_identical(
    reverse_scale(test, exclude = "x"),
    data.frame(
      x = 1:5,
      y = c(6, 1, 7, 4, 8)
    )
  )
  expect_identical(
    reverse_scale(test),
    data.frame(
      x = as.double(5:1),
      y = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("reverse_scale: arg 'select' works with formula", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_identical(
    reverse_scale(test, select = ~x),
    data.frame(
      x = as.double(5:1),
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_identical(
    reverse_scale(test, select = ~ x + y),
    data.frame(
      x = as.double(5:1),
      y = c(6, 1, 7, 4, 8)
    )
  )
})

test_that("reverse_scale: arg 'exclude' works with formula", {
  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_identical(
    reverse_scale(test, exclude = ~x),
    data.frame(
      x = 1:5,
      y = c(6, 1, 7, 4, 8)
    )
  )
  expect_identical(
    reverse_scale(test, exclude = ~ x + y),
    test
  )
})

test_that("reverse_scale: argument 'range' works", {
  expect_identical(
    reverse_scale(c(1, 3, 4), range = c(0, 4)),
    c(3, 1, 0)
  )
  expect_identical(
    reverse_scale(factor(c(1, 2, 3, 4, 5)), range = 0:10),
    factor(9:5, levels = 0:10)
  )

  test <- data.frame(
    x = 1:5,
    y = c(3, 8, 2, 5, 1)
  )
  expect_identical(
    reverse_scale(test, select = "x", range = c(0, 8)),
    data.frame(
      x = as.double(7:3),
      y = c(3, 8, 2, 5, 1)
    )
  )
  expect_identical(
    reverse_scale(test, range = c(0, 8)),
    data.frame(
      x = as.double(7:3),
      y = c(5, 0, 6, 3, 7)
    )
  )
})

test_that("reverse_scale ignores NA", {
  expect_identical(
    reverse_scale(c(1, 2, 8, NA)),
    c(8, 7, 1, NA)
  )
})

test_that("reverse_scale returns NA if only NA provided", {
  expect_identical(
    reverse_scale(c(NA_real_, NA_real_)),
    c(NA_real_, NA_real_)
  )
  expect_identical(
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
    Sepal.Length = c(0, 1),
    Petal.Length = c(-1, 0)
  ), select = ends_with("length"))

  expect_identical(out$Sepal.Length, iris$Sepal.Length, tolerance = 1e-3)

  out <- rescale(iris,
    to = list(
      Sepal.Length = c(0, 1),
      Petal.Length = c(-1, 0)
    ),
    select = ends_with("length"),
    ignore_case = TRUE
  )

  expect_identical(head(out$Sepal.Length), c(0.22222, 0.16667, 0.11111, 0.08333, 0.19444, 0.30556), tolerance = 1e-3)
})




# with grouped data -------------------------------------------

set.seed(123)
value1 <- sample(1:10, 6, replace = TRUE)
set.seed(456)
value2 <- sample(1:10, 6, replace = TRUE)

test_df <- data.frame(
  id = rep(c("A", "B"), each = 3),
  value1 = value1,
  value2 = value2,
  stringsAsFactors = FALSE
)

test_that("reverse works with data frames (grouped data)", {
  skip_if_not_installed("poorman")

  expect_identical(
    test_df %>%
      poorman::group_by(id) %>%
      reverse(exclude = "id") %>%
      poorman::ungroup(),
    data.frame(
      id = rep(c("A", "B"), each = 3),
      value1 = c(10, 10, 3, 6, 2, 3),
      value2 = c(4, 6, 3, 10, 6, 5),
      stringsAsFactors = FALSE
    )
  )
})


test_that("reverse works with grouped data frames and append", {
  skip_if_not_installed("poorman")

  test <- data.frame(
    x = 1:6,
    y = c(3, 8, 2, 5, 1, 4),
    grp = rep(c("a", "b"), 3),
    stringsAsFactors = FALSE
  )
  expect_identical(
    test %>%
      poorman::group_by(grp) %>%
      reverse(append = TRUE) %>%
      poorman::ungroup(),
    data.frame(
      x = 1:6,
      y = c(3, 8, 2, 5, 1, 4),
      grp = rep(c("a", "b"), 3),
      x_r = as.double(c(5, 6, 3, 4, 1, 2)),
      y_r = as.double(c(1, 4, 2, 7, 3, 8)),
      stringsAsFactors = FALSE
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
  value2 = value2,
  stringsAsFactors = FALSE
)

test_that("reverse works with data frames containing NAs (grouped data)", {
  skip_if_not_installed("poorman")

  expect_identical(
    test_df %>%
      poorman::group_by(id) %>%
      reverse(exclude = "id") %>%
      poorman::ungroup(),
    data.frame(
      id = rep(c("A", "B"), each = 3),
      value1 = c(10, 4, 4, 5, 3, 4),
      value2 = c(NA, 10, 9, 7, 6, 8),
      stringsAsFactors = FALSE
    )
  )
})

# select helpers ------------------------------
test_that("reverse regex", {
  expect_identical(
    reverse(mtcars, select = "arb", regex = TRUE),
    reverse(mtcars, select = "carb")
  )
})


# work or give informative errors / warnings (#380) ------------------
test_that("reverse, larger range", {
  # works
  expect_identical(
    reverse(c(1, 3, 4), range = c(0, 4)),
    c(3, 1, 0)
  )
  expect_identical(
    reverse(factor(c(1, 3, 4)), range = 0:4),
    structure(c(4L, 2L, 1L), levels = c("0", "1", "2", "3", "4"), class = "factor")
  )
  expect_identical(
    reverse(factor(c(1, 3, 4)), range = c(0, 4)),
    structure(c(4L, 2L, 1L), levels = c("0", "1", "2", "3", "4"), class = "factor")
  )

  # errors on invalid input
  expect_error(reverse(c(1, 3, 4), range = 0:4))
  expect_error(reverse(factor(c(1, 3, 4, 5)), range = c(0, 2, 4)))
  # errors on invalid input (NA in range)
  expect_error(reverse(c(1, 3, 4), range = c(1, NA)), regex = "missing")
  expect_error(reverse(factor(letters[1:3]), range = c(1, NA)), regex = "missing")

  # warns
  expect_warning(
    reverse(factor(c("a", "b", "c")), range = c(1, 3, 5, 7)),
    regex = "No current"
  )
  expect_warning(
    reverse(factor(c(9, 10, 11)), range = c(1, 3, 5, 7)),
    regex = "No current"
  )
  expect_warning(
    reverse(factor(c(1, 3, 11)), range = c(1, 3, 5, 7)),
    regex = "Not all"
  )

  # silent
  expect_silent(reverse(factor(c("a", "b", "c")), range = c(1, 3, 5, 7), verbose = FALSE))
  expect_silent(reverse(factor(c(9, 10, 11)), range = c(1, 3, 5, 7), verbose = FALSE))
  expect_silent(reverse(factor(c(1, 3, 11)), range = c(1, 3, 5, 7), verbose = FALSE))

  # works as intended
  expect_identical(
    reverse(factor(c(1, 3, 11)), range = c(1, 3, 5, 7), verbose = FALSE),
    structure(c(4L, 3L, NA), levels = c("1", "3", "5", "7"), class = "factor")
  )
})
