# numeric --------------------------

test_that("convert_na_to - numeric: works", {
  expect_equal(
    convert_na_to(c(1, 2, 3, NA), replacement = 4),
    1:4
  )

  expect_equal(
    convert_na_to(c(1, 2, 3, NA), replacement = NULL, verbose = FALSE),
    c(1, 2, 3, NA)
  )
})

test_that("convert_na_to - numeric: arg 'replacement' can only be numeric", {
  expect_warning(
    convert_na_to(c(1, 2, 3, NA), replacement = "a"),
    regexp = "`replacement` needs to be a numeric vector."
  )
  expect_warning(
    convert_na_to(c(1, 2, 3, NA), replacement = factor(8)),
    regexp = "`replacement` needs to be a numeric vector."
  )
})

test_that("convert_na_to - numeric: returns original vector if 'replacement' not good", {
  expect_equal(
    convert_na_to(c(1, 2, 3, NA), replacement = "a", verbose = FALSE),
    c(1, 2, 3, NA)
  )
  expect_equal(
    convert_na_to(c(1, 2, 3, NA), replacement = factor(8), verbose = FALSE),
    c(1, 2, 3, NA)
  )
})



# character --------------------------

test_that("convert_na_to - character: works", {
  expect_equal(
    convert_na_to(c("a", "b", "c", NA), replacement = "d"),
    c("a", "b", "c", "d")
  )
  expect_equal(
    convert_na_to(c("a", "b", "c", NA), replacement = NULL, verbose = FALSE),
    c("a", "b", "c", NA)
  )
})

test_that("convert_na_to - character: arg 'replacement' can only be character", {
  expect_warning(
    convert_na_to(c("a", "b", "c", NA), replacement = 1),
    regexp = "`replacement` needs to be a character vector."
  )
  expect_warning(
    convert_na_to(c("a", "b", "c", NA), replacement = factor(8)),
    regexp = "`replacement` needs to be a character vector."
  )
})

test_that("convert_na_to - character: returns original vector if 'replacement' not good", {
  expect_equal(
    convert_na_to(c("a", "b", "c", NA), replacement = 1, verbose = FALSE),
    c("a", "b", "c", NA)
  )
  expect_equal(
    convert_na_to(c("a", "b", "c", NA), replacement = factor(8), verbose = FALSE),
    c("a", "b", "c", NA)
  )
})





# factor --------------------------

test_that("convert_na_to - factor: works when 'replacement' is numeric ", {
  x <- convert_na_to(factor(c(1, 2, 3, NA)), replacement = 4)
  expect_equal(
    x,
    factor(1:4)
  )
  expect_equal(levels(x), as.character(1:4))
  expect_equal(
    convert_na_to(factor(c(1, 2, 3, NA)), replacement = NULL, verbose = FALSE),
    factor(c(1, 2, 3, NA))
  )
})

test_that("convert_na_to - factor: works when 'replacement' is character", {
  x <- convert_na_to(factor(c(1, 2, 3, NA)), replacement = "d")
  expect_equal(
    x,
    factor(c(1:3, "d"))
  )
  expect_equal(levels(x), as.character(c(1:3, "d")))
})







# dataframe --------------------------

test <- data.frame(
  x = c(1, 2, NA),
  y = c("a", "b", NA),
  z = factor(c("a", "b", NA)),
  x2 = c(4, 5, NA)
)

test_that("convert_na_to - dataframe: works", {
  expect_equal(
    convert_na_to(test, replacement_num = 4, replacement_char = "e",
                  replacement_fac = 8),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", 8)),
      x2 = c(4, 5, 4)
    )
  )
})

test_that("convert_na_to - dataframe: only modifies numeric if only numeric specified", {
  expect_equal(
    convert_na_to(test, replacement_num = 4),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, 4)
    )
  )
})

test_that("convert_na_to - dataframe: only modifies character if only character specified", {
  expect_equal(
    convert_na_to(test, replacement_char = "e"),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, NA)
    )
  )
})

test_that("convert_na_to - dataframe: only modifies factor if only factor specified", {
  expect_equal(
    convert_na_to(test, replacement_num = 4, replacement_char = "e",
                  replacement_fac = 8),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", 8)),
      x2 = c(4, 5, 4)
    )
  )
})

test_that("convert_na_to - dataframe: arg 'select' works", {
  expect_equal(
    convert_na_to(test, replacement_num = 4, replacement_char = "e",
                  replacement_fac = 8, select = "x"),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, NA)
    )
  )

  expect_equal(
    convert_na_to(test, replacement_num = 4, replacement_char = "e",
                  replacement_fac = 8, select = ~ x),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, NA)
    )
  )
})


test_that("convert_na_to - dataframe: arg 'exclude' works", {
  expect_equal(
    convert_na_to(test, replacement_num = 4, replacement_char = "e",
                  replacement_fac = 8, exclude = "x"),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", 8)),
      x2 = c(4, 5, 4)
    )
  )

  expect_equal(
    convert_na_to(test, replacement_num = 4, replacement_char = "e",
                  replacement_fac = 8, exclude = ~ x),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", 8)),
      x2 = c(4, 5, 4)
    )
  )
})

