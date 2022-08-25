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

test_that("convert_na_to - numeric: arg 'replacement' must be of length one", {
  expect_warning(
    convert_na_to(c(1, 2, 3, NA), replacement = c(1, 2)),
    regexp = "`replacement` needs to be of length one."
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
    convert_na_to(c("a", "b", "c", NA), replacement = mtcars),
    regexp = "`replacement` needs to be a character or numeric vector."
  )
  expect_warning(
    convert_na_to(c("a", "b", "c", NA), replacement = factor(8)),
    regexp = "`replacement` needs to be a character or numeric vector."
  )
})

test_that("convert_na_to - numeric: arg 'replacement' must be of length one", {
  expect_warning(
    convert_na_to(c("a", "b", "c", NA), replacement = c("d", "e")),
    regexp = "`replacement` needs to be of length one."
  )
})

test_that("convert_na_to - character: returns original vector if 'replacement' not good", {
  expect_equal(
    convert_na_to(c("a", "b", "c", NA), replacement = 1, verbose = FALSE),
    c("a", "b", "c", 1)
  )
  expect_equal(
    convert_na_to(c("a", "b", "c", NA), replacement = mtcars, verbose = FALSE),
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







# data frame --------------------------

test <- data.frame(
  x = c(1, 2, NA),
  y = c("a", "b", NA),
  z = factor(c("a", "b", NA)),
  x2 = c(4, 5, NA),
  stringsAsFactors = FALSE
)

test_that("convert_na_to - data frame: works with replace_* args", {
  expect_equal(
    convert_na_to(test, replace_num = 4, replace_char = "e", replace_fac = 8),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", "8"), levels = c("a", "b", "8")),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )
})

test_that("convert_na_to - data frame: only modifies numeric if only numeric specified", {
  expect_equal(
    convert_na_to(test, replace_num = 4),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )
})

test_that("convert_na_to - data frame: only modifies character if only character specified", {
  expect_equal(
    convert_na_to(test, replace_char = "e"),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, NA),
      stringsAsFactors = FALSE
    )
  )
})

test_that("convert_na_to - data frame: only modifies factor if only factor specified", {
  expect_equal(
    convert_na_to(test, replace_fac = 8),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", NA),
      z = factor(c("a", "b", "8"), levels = c("a", "b", "8")),
      x2 = c(4, 5, NA),
      stringsAsFactors = FALSE
    )
  )
})

test_that("convert_na_to - data frame: arg 'select' works", {
  expect_equal(
    convert_na_to(test,
      replace_num = 4, replace_char = "e",
      replace_fac = 8, select = "x"
    ),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, NA),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    convert_na_to(test,
      replace_num = 4, replace_char = "e",
      replace_fac = 8, select = ~x
    ),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, NA),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    convert_na_to(test,
      replace_num = 4, replace_char = "e",
      replace_fac = 8, select = starts_with("x")
    ),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    convert_na_to(test,
      replace_num = 4, replace_char = "e",
      replace_fac = 8, select = ends_with("2")
    ),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    convert_na_to(test,
      replace_num = 4, replace_char = "e",
      replace_fac = 8, select = contains("x")
    ),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    convert_na_to(test,
      replace_num = 4, replace_char = "e",
      replace_fac = 8, select = 1:3
    ),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", "8"), levels = c("a", "b", "8")),
      x2 = c(4, 5, NA),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    convert_na_to(test,
      replace_num = 4, replace_char = "e",
      replace_fac = 8, select = regex("2$")
    ),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )
})


test_that("convert_na_to - data frame: arg 'exclude' works", {
  expect_equal(
    convert_na_to(test,
      replace_num = 4, replace_char = "e",
      replace_fac = 8, exclude = "x"
    ),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", "8"), levels = c("a", "b", "8")),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    convert_na_to(test,
      replace_num = 4, replace_char = "e",
      replace_fac = 8, exclude = ~x
    ),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", "8"), levels = c("a", "b", "8")),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    convert_na_to(test,
      replace_num = 4, replace_char = "e",
      replace_fac = 8, select = starts_with("x"), exclude = ~x
    ),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )
})

test_that("convert_na_to - data frame: works when arg 'select' is a list", {
  # numeric
  expect_equal(
    convert_na_to(test, replace_num = 4, select = list(x = 0)),
    data.frame(
      x = c(1, 2, 0),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )

  # character
  expect_equal(
    convert_na_to(test, replace_char = "e", select = list(y = "d")),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", "d"),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, NA),
      stringsAsFactors = FALSE
    )
  )

  # only named list can override replace_*
  expect_equal(
    convert_na_to(test, replace_num = 4, select = list(0)),
    data.frame(
      x = c(1, 2, 4),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )

  expect_equal(
    convert_na_to(test, replace_char = "e", select = list("d")),
    data.frame(
      x = c(1, 2, NA),
      y = c("a", "b", "e"),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, NA),
      stringsAsFactors = FALSE
    )
  )

  # no problem if put a variable that doesn't exist in list
  expect_equal(
    convert_na_to(test, replace_num = 4, select = list(x = 0, foo = 5)),
    data.frame(
      x = c(1, 2, 0),
      y = c("a", "b", NA),
      z = factor(c("a", "b", NA)),
      x2 = c(4, 5, 4),
      stringsAsFactors = FALSE
    )
  )
})



# preserve attributes --------------------------

test_that("data_rename preserves attributes", {
  skip_if_not_installed("parameters")

  m <- lm(Sepal.Length ~ Species, data = iris)
  out <- parameters::parameters(m)
  a1 <- attributes(out)

  out2 <- convert_na_to(out, replace_num = 5)
  a2 <- attributes(out2)

  expect_equal(names(a1)[1:28], names(a2)[1:28])
})

# select helpers ------------------------------
test_that("convert_na_to regex", {
  expect_equal(
    convert_na_to(airquality, replacement = 0, select = "zone", regex = TRUE),
    convert_na_to(airquality, replacement = 0, select = "Ozone")
  )
  expect_equal(
    convert_na_to(airquality, replacement = 0, select = "zone$", regex = TRUE),
    convert_na_to(airquality, replacement = 0, select = "Ozone")
  )
})
