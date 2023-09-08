test_that("labels_to_levels, numeric", {
  expect_message(
    labels_to_levels(1:10),
    regex = "only works"
  )
})

test_that("labels_to_levels, factor", {
  data(efc)
  x <- as.factor(efc$c172code)
  attr(x, "labels") <- c(low = 1, mid = 2, high = 3)
  x <- labels_to_levels(x)
  expect_identical(levels(x), c("low", "mid", "high"))
  expect_equal(table(x), table(efc$c172code), ignore_attr = TRUE)

  x <- as.ordered(efc$c172code)
  attr(x, "labels") <- c(low = 1, mid = 2, high = 3)
  x <- labels_to_levels(x)
  expect_identical(levels(x), c("low", "mid", "high"))
  expect_s3_class(x, "ordered")
})

test_that("labels_to_levels, factor, error on no labels", {
  data(efc)
  data(iris)
  x <- as.factor(efc$c172code)
  expect_error(labels_to_levels(x), regex = "Could not change factor")
  expect_error(labels_to_levels(iris), regex = "Could not change factor")
})

test_that("labels_to_levels, factor, data frame", {
  data(efc)
  out <- labels_to_levels(efc)
  expect_identical(
    levels(out$e42dep),
    c(
      "independent", "slightly dependent", "moderately dependent",
      "severely dependent"
    )
  )
  expect_identical(sum(vapply(efc, is.factor, TRUE)), 1L)
})

test_that("labels_to_levels, factor, with random value numbers (no sequential order)", {
  x <- c(5, 5, 1, 3, 1, 7)
  attr(x, "labels") <- c(no = 7, yes = 1, maybe = 3, `don't know` = 5)
  out <- to_factor(x, labels_to_levels = TRUE)
  expect_identical(as.character(out), c("don't know", "don't know", "yes", "maybe", "yes", "no"))
  expect_identical(levels(out), c("yes", "maybe", "don't know", "no"))

  x <- c(4, 4, 1, 2, 1, 3)
  attr(x, "labels") <- c(a = 1, b = 2, c = 3, d = 4)
  out <- to_factor(x, labels_to_levels = TRUE)
  expect_identical(as.character(out), c("d", "d", "a", "b", "a", "c"))
  expect_identical(levels(out), c("a", "b", "c", "d"))

  x <- c(4, 4, 1, 2, 1, 3)
  attr(x, "labels") <- c(d = 1, c = 2, b = 3, a = 4)
  out <- to_factor(x, labels_to_levels = TRUE)
  expect_identical(as.character(out), c("a", "a", "d", "c", "d", "b"))
  expect_identical(levels(out), c("d", "c", "b", "a"))

  x <- c(5, 5, 1, 3, 1, 7)
  attr(x, "labels") <- c(no = 7, yes = 1, maybe = 3, `don't know` = 5)
  out <- to_factor(x, labels_to_levels = TRUE)
  expect_identical(
    out,
    structure(
      c(3L, 3L, 1L, 2L, 1L, 4L),
      levels = c("yes", "maybe", "don't know", "no"),
      class = "factor"
    )
  )
  expect_identical(
    as.character(out),
    c("don't know", "don't know", "yes", "maybe", "yes", "no")
  )

  x <- c(5, 5, 1, 3, 1, 7, 4)
  attr(x, "labels") <- c(no = 7, yes = 1, maybe = 3, `don't know` = 5)
  expect_message(
    {
      out <- to_factor(x, labels_to_levels = TRUE)
    },
    regex = "Not all factor levels"
  )
  expect_identical(
    out,
    structure(
      c(4L, 4L, 1L, 2L, 1L, 5L, 3L),
      levels = c("yes", "maybe", "4", "don't know", "no"),
      class = "factor"
    )
  )
  expect_identical(
    as.character(out),
    c("don't know", "don't know", "yes", "maybe", "yes", "no", "4")
  )

  x <- c(5, 5, 1, 3, 1, 7)
  attr(x, "labels") <- c(no = 7, yes = 1, maybe = 4, `don't know` = 5)
  expect_message({
    out <- to_factor(x, labels_to_levels = TRUE)
  })
  expect_identical(
    out,
    structure(
      c(3L, 3L, 1L, 2L, 1L, 4L),
      levels = c("yes", "3", "don't know", "no"),
      class = "factor"
    )
  )
  expect_identical(
    as.character(out),
    c("don't know", "don't know", "yes", "3", "yes", "no")
  )

  x <- c(5, 5, 1, 3, 1, 7, 6)
  attr(x, "labels") <- c(no = 7, yes = 1, maybe = 4, `don't know` = 5)
  expect_message({
    out <- to_factor(x, labels_to_levels = TRUE)
  })
  expect_identical(
    out,
    structure(
      c(3L, 3L, 1L, 2L, 1L, 5L, 4L),
      levels = c("yes", "3", "don't know", "6", "no"),
      class = "factor"
    )
  )
  expect_identical(
    as.character(out),
    c("don't know", "don't know", "yes", "3", "yes", "no", "6")
  )
})
