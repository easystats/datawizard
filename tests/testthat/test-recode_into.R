test_that("recode_into", {
  x <<- 1:10
  out <- recode_into(
    x > 5 ~ "a",
    x > 2 & x <= 5 ~ "b",
    default = "c"
  )
  expect_identical(out, c("c", "c", "b", "b", "b", "a", "a", "a", "a", "a"))
})

test_that("recode_into, check mixed types", {
  x <<- 1:10
  expect_error(
    {
      out <- recode_into(
        x > 5 ~ 1,
        x > 2 & x <= 5 ~ "b"
      )
    },
    regexp = "Recoding not carried out"
  )
})

test_that("recode_into, complain about default = NULL", {
  x <<- 1:10
  expect_warning(
    {
      out <- recode_into(
        x > 5 ~ "c",
        x > 2 & x <= 5 ~ "b",
        default = NULL
      )
    },
    regexp = "Default value"
  )
  expect_identical(out, c(NA, NA, "b", "b", "b", "c", "c", "c", "c", "c"))
})

test_that("recode_into, data frame", {
  data(mtcars)
  out <- recode_into(
    mtcars$mpg > 20 & mtcars$cyl == 6 ~ 1,
    mtcars$mpg <= 20 ~ 2,
    default = 0
  )
  expect_identical(
    out,
    c(
      1, 1, 0, 1, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0,
      0, 2, 2, 2, 2, 0, 0, 0, 2, 2, 2, 0
    )
  )
  d <<- mtcars
  out <- recode_into(
    mpg > 20 & cyl == 6 ~ 1,
    mpg <= 20 ~ 2,
    default = 0,
    data = d
  )
  expect_identical(
    out,
    c(
      1, 1, 0, 1, 2, 2, 2, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0,
      0, 2, 2, 2, 2, 0, 0, 0, 2, 2, 2, 0
    )
  )
})

test_that("recode_into, works inside functions", {
  test <- function() {
    set.seed(123)
    d <- data.frame(
      x = sample(1:5, 30, TRUE),
      y = sample(letters[1:5], 30, TRUE),
      stringsAsFactors = FALSE
    )
    recode_into(
      x %in% 1:3 & y %in% c("a", "b") ~ 1,
      x > 3 ~ 2,
      data = d,
      default = 0
    )
  }
  expect_identical(
    test(),
    c(
      1, 1, 1, 0, 0, 2, 2, 0, 1, 1, 2, 0, 0, 0, 2, 1, 1, 2, 1, 0,
      1, 1, 0, 2, 0, 1, 2, 2, 1, 2
    )
  )
})

test_that("recode_into, check differen input length", {
  x <<- 1:10
  y <<- 10:30
  expect_error(
    {
      out <- recode_into(
        x > 5 ~ 1,
        y > 10 ~ 2
      )
    },
    regexp = "matching conditions"
  )
})

test_that("recode_into, check differen input length", {
  x <<- 1:5
  y <<- c(5, 2, 3, 1, 4)
  expect_warning(
    {
      out <- recode_into(
        x == 2 ~ 1,
        y == 2 & x == 2 ~ 2,
        default = 0
      )
    },
    regexp = "Several recode patterns"
  )
})
