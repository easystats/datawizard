skip_if_not_installed("poorman")

x <<- 1:30
test_that("recode_into", {
  out1 <- recode_into(
    x > 15 ~ "a",
    x > 10 & x <= 15 ~ "b",
    ~"c"
  )
  out2 <- poorman::case_when(
    x > 15 ~ "a",
    x > 10 & x <= 15 ~ "b",
    TRUE ~ "c"
  )
  expect_identical(out1, out2)
  out1 <- recode_into(
    x > 15 ~ "a",
    x > 10 & x <= 15 ~ "b",
    default = "c"
  )
  expect_identical(out1, out2)
})

test_that("recode_into, data frame", {
  data(mtcars)
  out1 <- recode_into(
    mtcars$mpg > 20 & mtcars$cyl == 6 ~ 1,
    mtcars$mpg <= 20 ~ 2,
    ~0
  )
  out2 <- poorman::case_when(
    mtcars$mpg > 20 & mtcars$cyl == 6 ~ 1,
    mtcars$mpg <= 20 ~ 2,
    TRUE ~ 0
  )
  expect_identical(out1, out2)
  d <<- mtcars
  out1 <- recode_into(
    mpg > 20 & cyl == 6 ~ 1,
    mpg <= 20 ~ 2,
    default = 0,
    data = d
  )
  expect_identical(out1, out2)
})
