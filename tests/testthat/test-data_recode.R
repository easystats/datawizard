# set recode pattern old=new --------------

options(data_recode_pattern = "old=new")



# numeric -----------------------

set.seed(123)
x <- sample(c(1:4, NA), 15, TRUE)

test_that("recode numeric", {
  out <- data_recode(x, list(`1` = 0, `2:3` = 1, `4` = 2))
  expect_equal(out, c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2), ignore_attr = TRUE)
  out <- data_recode(x, list(`1` = 0, `2:3` = 1, `4` = 2, `NA` = 9), preserve_na = FALSE)
  expect_equal(out, c(1, 1, 1, 1, 1, 9, 2, 0, 1, 1, 9, 1, 1, 0, 2), ignore_attr = TRUE)
  out <- data_recode(x, list(`1` = 0, `2:3` = 1, `4` = 2, `NA` = 9))
  expect_equal(out, c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2), ignore_attr = TRUE)
  out <- data_recode(x, list(`1` = 0, `2` = 1), default = 99, preserve_na = FALSE)
  expect_equal(out, c(99, 99, 1, 1, 99, 99, 99, 0, 1, 99, 99, 99, 99, 0, 99), ignore_attr = TRUE)
  out <- data_recode(x, list(`1` = 0, `2` = 1), default = 99)
  expect_equal(out, c(99, 99, 1, 1, 99, NA, 99, 0, 1, 99, NA, 99, 99, 0, 99), ignore_attr = TRUE)
})



# Date -----------------------

set.seed(123)
x <- as.Date("2022-01-01")

test_that("recode date", {
  expect_message(data_recode(x))
})



# factor -----------------------

set.seed(123)
x <- as.factor(sample(c("a", "b", "c"), 15, TRUE))

test_that("recode factor", {
  out <- data_recode(x, list(a = "x", `b, c` = "y"))
  expect_equal(
    out,
    structure(c(
      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L,
      2L, 2L
    ), .Label = c("x", "y"), class = "factor"),
    ignore_attr = TRUE
  )
  out <- data_recode(x, list(a = "x", `b, c` = "y"))
  expect_equal(
    out,
    structure(c(
      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L,
      2L, 2L
    ), .Label = c("x", "y"), class = "factor"),
    ignore_attr = TRUE
  )
})

set.seed(123)
x <- as.factor(sample(c("a", "b", "c", NA_character_), 15, TRUE))

test_that("recode factor", {
  out <- data_recode(x, list(a = "x", `b, c` = "y"))
  expect_equal(
    as.character(out),
    c("y", "y", "y", "y", "y", "y", "y", "y", "y", "x", NA, "y", "y", "x", "y"),
    ignore_attr = TRUE
  )
  out <- data_recode(x, list(a = "x", b = NA))
  expect_equal(
    as.character(out),
    c("c", "c", "c", NA, "c", NA, NA, NA, "c", "x", NA, NA, NA, "x", NA),
    ignore_attr = TRUE
  )
  out <- data_recode(x, list(a = "x", b = "y"), default = "zz")
  expect_equal(
    as.character(out),
    c("zz", "zz", "zz", "y", "zz", "y", "y", "y", "zz", "x", NA, "y", "y", "x", "y"),
    ignore_attr = TRUE
  )
  out <- data_recode(x, list(a = "x", b = "y"), default = "zz", preserve_na = FALSE)
  expect_equal(
    as.character(out),
    c("zz", "zz", "zz", "y", "zz", "y", "y", "y", "zz", "x", "zz", "y", "y", "x", "y"),
    ignore_attr = TRUE
  )
})



# character -----------------------

set.seed(123)
x <- as.character(sample(c("a", "b", "c"), 15, TRUE))

test_that("recode character", {
  out <- data_recode(x, list(a = "x", `b, c` = "y"))
  expect_equal(
    out,
    c("y", "y", "y", "y", "y", "y", "y", "y", "y", "x", "y", "y", "x", "y", "y"),
    ignore_attr = TRUE
  )
})



# data frame -----------------------

set.seed(123)
d <- data.frame(
  x = sample(c(1:4, NA), 15, TRUE),
  y = as.factor(sample(c("a", "b", "c"), 15, TRUE)),
  stringsAsFactors = FALSE
)

test_that("recode data.frame", {
  out <- data_recode(
    d,
    recode = list(`1` = 0, `2:3` = 1, `4` = 2, a = "x", `b, c` = "y"),
  )
  expect_equal(
    out,
    structure(list(
      x = c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2),
      y = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L),
        .Label = c("x", "y"),
        class = "factor"
      )
    ),
    row.names = c(NA, 15L), class = "data.frame"
    ),
    ignore_attr = TRUE
  )

  out <- data_recode(
    d,
    recode = list(`1` = 0, `2:3` = 1, `4` = 2, a = "x", `b, c` = "y"),
  )
  expect_equal(
    out,
    structure(list(
      x = c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2),
      y = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L),
        .Label = c("x", "y"),
        class = "factor"
      )
    ),
    row.names = c(NA, 15L), class = "data.frame"
    ),
    ignore_attr = TRUE
  )

  out <- data_recode(
    d,
    recode = list(`1` = 0, `2:3` = 1, `4` = 2, a = "x", `b, c` = "y"),
    select = is.numeric()
  )
  expect_equal(
    out,
    structure(list(
      x = c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2),
      y = structure(c(1L, 1L, 1L, 3L, 2L, 3L, 2L, 1L, 2L, 3L, 2L, 1L, 3L, 3L, 1L),
        .Label = c("a", "b", "c"),
        class = "factor"
      )
    ),
    row.names = c(NA, 15L), class = "data.frame"
    ),
    ignore_attr = TRUE
  )
})



# set recode pattern back to default --------------

options(data_recode_pattern = NULL)

set.seed(123)
x <- sample(c(1:4, NA), 15, TRUE)

test_that("recode numeric", {
  out <- data_recode(x, list(`0` = 1, `1` = 2:3, `2` = 4))
  expect_equal(out, c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2), ignore_attr = TRUE)
  out <- data_recode(x, list(`0` = 1, `1` = 2:3, `2` = 4, `9` = NA), preserve_na = FALSE)
  expect_equal(out, c(1, 1, 1, 1, 1, 9, 2, 0, 1, 1, 9, 1, 1, 0, 2), ignore_attr = TRUE)
  out <- data_recode(x, list(`0` = 1, `1` = 2:3, `2` = 4, `9` = NA), preserve_na = TRUE)
  expect_equal(out, c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2), ignore_attr = TRUE)
})

test_that("recode, recode-arg is named list", {
  expect_warning(expect_equal(data_recode(x, recode = c(`0` = 1, `1` = 2:3, `2` = 4)), x))
})


set.seed(123)
x <- as.factor(sample(c("a", "b", "c"), 15, TRUE))

test_that("recode factor", {
  out <- data_recode(x, list(x = "a", y = "b, c"))
  expect_equal(
    out,
    structure(c(
      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L,
      2L, 2L
    ), .Label = c("x", "y"), class = "factor"),
    ignore_attr = TRUE
  )
  out <- data_recode(x, list(x = "a", y = c("b", "c")))
  expect_equal(
    out,
    structure(c(
      2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 1L,
      2L, 2L
    ), .Label = c("x", "y"), class = "factor"),
    ignore_attr = TRUE
  )
})

test_that("recode, recode-arg is named list", {
  expect_warning(expect_equal(data_recode(x, recode = c(x = "a", y = "b, c")), x))
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
    recode = list(`0` = 1, `1` = 2:3, `2` = 4, x = "a", y = "b, c"),
  )
  expect_equal(
    out,
    structure(list(
      x = c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2),
      y = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L),
        .Label = c("x", "y"),
        class = "factor"
      )
    ),
    row.names = c(NA, 15L), class = "data.frame"
    ),
    ignore_attr = TRUE
  )

  out <- data_recode(
    d,
    recode = list(`0` = 1, `1` = 2:3, `2` = 4, x = "a", y = c("b", "c")),
  )
  expect_equal(
    out,
    structure(list(
      x = c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2),
      y = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 2L, 2L, 1L),
        .Label = c("x", "y"),
        class = "factor"
      )
    ),
    row.names = c(NA, 15L), class = "data.frame"
    ),
    ignore_attr = TRUE
  )

  out <- data_recode(
    d,
    recode = list(`0` = 1, `1` = 2:3, `2` = 4, x = "a", y = c("b", "c")),
    select = is.numeric()
  )
  expect_equal(
    out,
    structure(list(
      x = c(1, 1, 1, 1, 1, NA, 2, 0, 1, 1, NA, 1, 1, 0, 2),
      y = structure(c(1L, 1L, 1L, 3L, 2L, 3L, 2L, 1L, 2L, 3L, 2L, 1L, 3L, 3L, 1L),
        .Label = c("a", "b", "c"),
        class = "factor"
      )
    ),
    row.names = c(NA, 15L), class = "data.frame"
    ),
    ignore_attr = TRUE
  )
})
