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
  expect_warning(out <- recode_into( # nolint
    x > 5 ~ 1,
    x > 2 & x <= 5 ~ "b"
  ))
  expect_identical(out, c(NA, NA, "b", "b", "b", "1", "1", "1", "1", "1"))
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
