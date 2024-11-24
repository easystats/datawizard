test_that("recode_into", {
  x <- 1:10
  out <- recode_into(
    x > 5 ~ "a",
    x > 2 & x <= 5 ~ "b",
    default = "c"
  )
  expect_identical(out, c("c", "c", "b", "b", "b", "a", "a", "a", "a", "a"))
})

test_that("recode_into, overwrite", {
  x <- 1:30
  expect_warning(
    recode_into(
      x > 1 ~ "a",
      x > 10 & x <= 15 ~ "b",
      default = "c",
      overwrite = TRUE
    ),
    regex = "overwritten"
  )
  # validate results
  x <- 1:10
  expect_silent({
    out <- recode_into(
      x >= 3 & x <= 7 ~ 1,
      x > 5 ~ 2,
      default = 0,
      verbose = FALSE
    )
  })
  expect_identical(out, c(0, 0, 1, 1, 1, 2, 2, 2, 2, 2))
  expect_warning(
    recode_into(
      x >= 3 & x <= 7 ~ 1,
      x > 5 ~ 2,
      default = 0
    ),
    regex = "case 6"
  )

  x <- 1:10
  expect_silent({
    out <- recode_into(
      x >= 3 & x <= 7 ~ 1,
      x > 5 ~ 2,
      default = 0,
      overwrite = FALSE,
      verbose = FALSE
    )
  })
  expect_identical(out, c(0, 0, 1, 1, 1, 1, 1, 2, 2, 2))
  expect_warning(
    recode_into(
      x >= 3 & x <= 7 ~ 1,
      x > 5 ~ 2,
      default = 0,
      overwrite = FALSE
    ),
    regex = "case 6"
  )
})

test_that("recode_into, don't overwrite", {
  x <- 1:30
  expect_warning(
    recode_into(
      x > 1 ~ "a",
      x > 10 & x <= 15 ~ "b",
      default = "c",
      overwrite = FALSE
    ),
    regex = "altered"
  )
})

test_that("recode_into, check mixed types", {
  x <- 1:10
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
  x <- 1:10
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
  d <- mtcars
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
      x = sample.int(5, 30, TRUE),
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
  x <- 1:10
  y <- 10:30
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

test_that("recode_into, check different input length", {
  x <- 1:5
  y <- c(5, 2, 3, 1, 4)
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

test_that("recode_into, make sure recode works with missing in original variable", {
  data(mtcars)
  mtcars$mpg[c(3, 10, 12, 15, 16)] <- NA
  mtcars$cyl[c(2, 15, 16)] <- NA
  d_recode_na <<- as.data.frame(mtcars)
  out1_recoded_na <- recode_into(
    d_recode_na$mpg > 20 & d_recode_na$cyl == 6 ~ 1,
    d_recode_na$mpg <= 20 ~ 2,
    d_recode_na$cyl == 4 ~ 3,
    default = 0,
    preserve_na = TRUE
  )
  out2_recoded_na <- recode_into(
    d_recode_na$mpg > 20 & d_recode_na$cyl == 6 ~ 1,
    d_recode_na$mpg <= 20 ~ 2,
    default = 0,
    preserve_na = TRUE
  )
  expect_message(
    {
      out3_recoded_na <- recode_into(
        d_recode_na$mpg > 20 & d_recode_na$cyl == 6 ~ 1,
        d_recode_na$mpg <= 20 ~ 2,
        d_recode_na$cyl == 4 ~ 3,
        default = 0,
        preserve_na = FALSE
      )
    },
    regex = "Missing values in original variable"
  )
  expect_message(
    {
      out4_recoded_na <- recode_into(
        d_recode_na$mpg > 20 & d_recode_na$cyl == 6 ~ 1,
        d_recode_na$mpg <= 20 ~ 2,
        default = 0,
        preserve_na = FALSE
      )
    },
    regex = "Missing values in original variable"
  )
  # one NA in mpg is overwritten by valid value from cyl, total 5 NA
  expect_identical(
    out1_recoded_na,
    c(
      1, NA, 3, 1, 2, 2, 2, 3, 3, NA, 2, NA, 2, 2, NA, NA, 2, 3,
      3, 3, 3, 2, 2, 2, 2, 3, 3, 3, 2, 2, 2, 3
    )
  )
  # total 6 NA
  expect_identical(
    out2_recoded_na,
    c(
      1, NA, NA, 1, 2, 2, 2, 0, 0, NA, 2, NA, 2, 2, NA, NA, 2, 0,
      0, 0, 0, 2, 2, 2, 2, 0, 0, 0, 2, 2, 2, 0
    )
  )
  # NA is preserved, set to default if not overwritten by other recodes
  expect_identical(
    out3_recoded_na,
    c(
      1, 0, 3, 1, 2, 2, 2, 3, 3, 0, 2, 0, 2, 2, 0, 0, 2, 3, 3, 3,
      3, 2, 2, 2, 2, 3, 3, 3, 2, 2, 2, 3
    )
  )
  expect_identical(
    out4_recoded_na,
    c(
      1, 0, 0, 1, 2, 2, 2, 0, 0, 0, 2, 0, 2, 2, 0, 0, 2, 0, 0, 0,
      0, 2, 2, 2, 2, 0, 0, 0, 2, 2, 2, 0
    )
  )
})

test_that("recode_into, NA doesn't need to be of exact type", {
  data(mtcars)
  x1 <- recode_into(
    mpg > 10 ~ 1,
    gear == 5 ~ NA_real_,
    data = mtcars,
    verbose = FALSE
  )
  x2 <- recode_into(
    mpg > 10 ~ 1,
    gear == 5 ~ NA,
    data = mtcars,
    verbose = FALSE
  )
  expect_identical(x1, x2)
})
