test <- head(iris)

# basic tests --------------

test_that("data_rename works with one or several replacements", {
  expect_named(
    data_rename(test, "Sepal.Length", "length"),
    c("length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )
  expect_named(
    data_rename(
      test, c("Sepal.Length", "Sepal.Width"),
      c("length", "width")
    ),
    c("length", "width", "Petal.Length", "Petal.Width", "Species")
  )
  expect_named(
    data_rename(test, c(length = "Sepal.Length", width = "Sepal.Width")),
    c("length", "width", "Petal.Length", "Petal.Width", "Species")
  )
})

test_that("data_rename cannot have a partially named vector", {
  expect_error(
    data_rename(test, c(length = "Sepal.Length", "Sepal.Width")),
    "all elements must"
  )
})

test_that("data_rename returns a data frame", {
  x <- data_rename(test, "Sepal.Length", "length")
  expect_s3_class(x, "data.frame")
})

test_that("data_rename: multiple selection types", {
  expect_named(
    data_rename(test, select = 1, "foo"),
    c("foo", names(iris)[2:5])
  )
  expect_named(
    data_rename(test, select = regex("tal"), c("foo1", "foo2")),
    c("Sepal.Length", "Sepal.Width", "foo1", "foo2", "Species")
  )
})

test_that("data_rename: replacement not allowed to have NA or empty strings", {
  expect_error(
    data_rename(
      test,
      select = c("Species", "Sepal.Length"),
      replacement = c("foo", NA_character_)
    ),
    regexp = "`replacement` is not allowed"
  )
})

# replacement -------------

test_that("data_rename errors when no replacement", {
  expect_error(
    data_rename(test, select = c("Sepal.Length", "Petal.Length")),
    "There are more names in `select` than in `replacement`"
  )
})

test_that("data_rename errors when too many names in 'replacement'", {
  expect_error(
    data_rename(test, replacement = paste0("foo", 1:6)),
    "There are more names in `replacement` than in `select`"
  )
})

test_that("data_rename works when not enough names in 'replacement'", {
  expect_error(
    data_rename(test, replacement = paste0("foo", 1:2)),
    "There are more names in `select` than in `replacement`"
  )
})


# no select --------------

test_that("data_rename errors when select = NULL", {
  expect_error(
    data_rename(test),
    "more names in `select`"
  )
})


# other --------------

test_that("data_rename: argument 'safe' is deprecated", {
  expect_error(
    data_rename(iris, "FakeCol", "length", verbose = FALSE),
    "were not found"
  )
  expect_error(
    expect_warning(
      data_rename(iris, "FakeCol", "length", safe = FALSE, verbose = FALSE),
      "used"
    )
  )
})

test_that("data_rename deals correctly with duplicated replacement", {
  x <- data_rename(test,
    select = names(test)[1:4],
    replacement = c("foo", "bar", "foo", "bar")
  )
  expect_identical(dim(test), dim(x))
  expect_named(x[1:4], c("foo", "bar", "foo.2", "bar.2"))
})


# preserve attributes --------------------------

test_that("data_rename preserves attributes", {
  skip_if_not_installed("parameters")

  m <- lm(Sepal.Length ~ Species, data = iris)
  out <- parameters::parameters(m)
  a1 <- attributes(out)

  out2 <- data_rename(out, "p", "p-val")
  a2 <- attributes(out2)

  expect_named(a1, names(a2))
})


# glue-styled select --------------------------

test_that("data_rename glue-style", {
  data(mtcars)
  out <- data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "formerly_{col}")
  expect_named(out, c("formerly_mpg", "formerly_cyl", "formerly_disp"))
  out <- data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "{col}_is_column_{n}")
  expect_named(out, c("mpg_is_column_1", "cyl_is_column_2", "disp_is_column_3"))
  out <- data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "new_{letter}")
  expect_named(out, c("new_a", "new_b", "new_c"))
})

test_that("data_rename enough letters", {
  data(efc, package = "datawizard")
  data(mtcars)
  data(iris)
  data(ChickWeight)
  data(ToothGrowth)
  data(USArrests)
  data(airquality)
  x <- cbind(
    mtcars[1:5, ], iris[1:5, ], efc[1:5, ], ChickWeight[1:5, ], ToothGrowth[1:5, ],
    USArrests[1:5, ], airquality[1:5, ]
  )
  expect_named(
    data_rename(x, replacement = "long_letter_{letter}"),
    c(
      "long_letter_a1", "long_letter_b1", "long_letter_c1", "long_letter_d1",
      "long_letter_e1", "long_letter_f1", "long_letter_g1", "long_letter_h1",
      "long_letter_i1", "long_letter_j1", "long_letter_k1", "long_letter_l1",
      "long_letter_m1", "long_letter_n1", "long_letter_o1", "long_letter_p1",
      "long_letter_q1", "long_letter_r1", "long_letter_s1", "long_letter_t1",
      "long_letter_u1", "long_letter_v1", "long_letter_w1", "long_letter_x1",
      "long_letter_y1", "long_letter_z1", "long_letter_a2", "long_letter_b2",
      "long_letter_c2", "long_letter_d2", "long_letter_e2", "long_letter_f2",
      "long_letter_g2", "long_letter_h2", "long_letter_i2", "long_letter_j2",
      "long_letter_k2", "long_letter_l2"
    )
  )
})

skip_if_not_installed("withr")
withr::with_environment(
  new.env(),
  test_that("data_rename glue-style, environment", {
    data(mtcars)
    x <- c("hi", "there", "!")
    out <- data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "col_{x}")
    expect_named(out, c("col_hi", "col_there", "col_!"))
    expect_error(
      data_rename(mtcars[1:3], c("mpg", "disp"), "col_{x}"),
      regex = "The number of values"
    )
  })
)

withr::with_environment(
  new.env(),
  test_that("data_rename glue-style, object not in environment", {
    data(mtcars)
    expect_error(
      data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "col_{x}"),
      regex = "The object"
    )
  })
)

withr::with_environment(
  new.env(),
  test_that("data_rename glue-style, function in environment", {
    data(mtcars)
    my_fun <- function(cols_to_rename) {
      data_rename(head(mtcars)[, 1:6], cols_to_rename, "new_{col}")
    }
    expect_named(
      my_fun(c("mpg", "drat")),
      c("new_mpg", "cyl", "disp", "hp", "new_drat", "wt")
    )
    expect_named(
      my_fun("mpg"),
      c("new_mpg", "cyl", "disp", "hp", "drat", "wt")
    )
  })
)

test_that("Argument `pattern` is deprecated", {
  expect_warning(
    head(data_rename(iris, pattern = "Sepal.Length", "length")),
    "Argument `pattern` is deprecated. Please use `select` instead.",
    fixed = TRUE
  )
})

test_that("works with lists", {
  result <- list(x = 1, y = 2)
  expect_error(
    data_rename(result, select = names(result), replacement = c("a", "b")),
    regex = "must be a data frame"
  )
})
