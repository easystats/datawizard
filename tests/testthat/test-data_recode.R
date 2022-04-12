set.seed(123)
x <- sample(c(1:4, NA), 15, TRUE)

test_that("recode numeric", {
  out <- data_recode(x, list(`0` = 1, `1` = 2:3, `2` = 4))
  expect_equal(out, c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2), ignore_attr = TRUE)
  out <- data_recode(x, list(`0` = 1, `1` = 2:3, `2` = 4, `9` = NA))
  expect_equal(out, c(1, 1, 1, 1, 1, 9, 2, 0, 1, 1, 9, 1, 1, 0, 2), ignore_attr = TRUE)
})

set.seed(123)
x <- as.factor(sample(c("a", "b", "c"), 15, TRUE))

test_that("recode factor", {
  out <- data_recode(x, list(x = "a", y = "b, c"))
  expect_equal(
    out,
    structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L,
                2L, 2L), .Label = c("x", "y"), class = "factor"),
    ignore_attr = TRUE
  )
  out <- data_recode(x, list(x = "a", y = c("b", "c")))
  expect_equal(
    out,
    structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L,
                2L, 2L), .Label = c("x", "y"), class = "factor"),
    ignore_attr = TRUE
  )
})


set.seed(123)
d <- data.frame(
  x = sample(c(1:4, NA), 15, TRUE),
  y = as.factor(sample(c("a", "b", "c"), 15, TRUE)),
  stringsAsFactors = FALSE
)

test_that("recode data.frame", {
  out <- data_recode(
    d,
    list(`0` = 1, `1` = 2:3, `2` = 4, x = "a", y = "b, c"),
    force = TRUE
  )
  expect_equal(
    out,
    structure(list(
      x = c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2),
      y = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L),
                    .Label = c("x", "y"),
                    class = "factor")),
      row.names = c(NA, 15L), class = "data.frame"),
    ignore_attr = TRUE
  )

  out <- data_recode(
    d,
    list(`0` = 1, `1` = 2:3, `2` = 4, x = "a", y = c("b", "c")),
    force = TRUE
  )
  expect_equal(
    out,
    structure(list(
      x = c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2),
      y = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L),
                    .Label = c("x", "y"),
                    class = "factor")),
      row.names = c(NA, 15L), class = "data.frame"),
    ignore_attr = TRUE
  )

  out <- data_recode(
    d,
    list(`0` = 1, `1` = 2:3, `2` = 4, x = "a", y = c("b", "c")),
    force = FALSE
  )
  expect_equal(
    out,
    structure(list(
      x = c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2),
      y = structure(c(1L, 1L, 1L, 3L, 2L, 3L, 2L, 1L, 2L, 3L, 2L, 1L, 3L, 3L, 1L),
                    .Label = c("a", "b", "c"),
                    class = "factor")),
      row.names = c(NA, 15L), class = "data.frame"),
    ignore_attr = TRUE
  )
})
