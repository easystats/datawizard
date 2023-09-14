skip_on_cran()
skip_if_offline()

skip_if_not_installed("httr")
skip_if_not_installed("haven")
skip_if_not_installed("readr")

# prepare data set ---------------

data(efc)
d <- data_filter(efc, 1:5)
d$e42dep <- droplevels(d$e42dep)



# SPSS -------------------------------------

tmp <- tempfile(fileext = ".sav")
on.exit(unlink(tmp))

test_that("data_write, SPSS", {
  expect_message(data_write(d, tmp))
  d2 <- data_read(tmp, verbose = FALSE)

  expect_equal(
    to_factor(d, select = c("e16sex", "c172code")),
    d2,
    ignore_attr = TRUE
  )
})


tmp <- tempfile(fileext = ".sav")
on.exit(unlink(tmp))

test_that("data_write, SPSS, mixed types of labelled vectors", {
  d <- data.frame(
    a = 1:3,
    b = letters[1:3],
    c = factor(letters[1:3]),
    d = as.Date(c("2022-01-01", "2022-02-01", "2022-03-01")),
    e = c(TRUE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )

  # Date and Logical cannot be labelled
  d$a <- assign_labels(d$a, variable = "First", values = c("one", "two", "three"))
  d$b <- assign_labels(d$b, variable = "Second", values = c("A", "B", "C"))
  d$c <- assign_labels(d$c, variable = "Third", values = c("ey", "bee", "see"))

  # expect message, but no error
  expect_message(data_write(d, "test.sav"), regex = "Preparing")
})



# Stata -------------------------------------

tmp <- tempfile(fileext = ".dta")
on.exit(unlink(tmp))

test_that("data_write, Stata", {
  data_write(d, tmp, verbose = FALSE)
  d2 <- data_read(tmp, verbose = FALSE)

  expect_equal(
    to_factor(d, select = c("e16sex", "c172code")),
    d2,
    ignore_attr = TRUE
  )
})



# csv -------------------------

tmp <- tempfile(fileext = ".csv")
on.exit(unlink(tmp))

test_that("data_write, CSV, keep numeric", {
  data_write(d, tmp)
  d2 <- data_read(tmp)

  expect_equal(
    to_numeric(d, dummy_factors = FALSE, preserve_levels = TRUE),
    d2,
    ignore_attr = TRUE
  )
})

test_that("data_write, CSV, convert to factor", {
  data_write(d, tmp, convert_factors = TRUE)
  d2 <- data_read(tmp)
  out <- to_factor(d, select = c("e16sex", "c172code"))
  out$e16sex <- as.character(out$e16sex)
  out$c172code <- as.character(out$c172code)
  out$e42dep <- as.numeric(as.character(out$e42dep))

  expect_equal(out, d2, ignore_attr = TRUE)
})

# main file
tmp <- tempfile(fileext = ".csv")
on.exit(unlink(tmp))

# file for labels
fpath <- dirname(tmp)
fname <- sub("\\.csv$", "", basename(tmp))
tmp2 <- paste0(fpath, .Platform$file.sep, fname, "_labels.csv")
on.exit(unlink(tmp2))

test_that("data_write, CSV, create labels file", {
  data(efc)
  expect_message(data_write(efc, tmp, save_labels = TRUE))
  d <- data_read(tmp2)

  expect_identical(d$variable[2], "e16sex")
  expect_identical(d$label[2], "elder's gender")
  expect_identical(d$labels[2], "1=male; 2=female")

  expect_message(data_write(efc, tmp, save_labels = TRUE, delimiter = ";"))
  d <- data_read(tmp2)
  expect_identical(d$variable[2], "e16sex")
  expect_identical(d$label[2], "elder's gender")
  expect_identical(d$labels[2], "1=male; 2=female")
})


# invalid file type -------------------------

test_that("data_write, no file extension", {
  expect_error(data_write(d, "mytestfile"))
  expect_error(data_write(d, NULL))
})


# writing character vector works for missing value labels ------------------

tmp <- tempfile(fileext = ".sav")
on.exit(unlink(tmp))

test_that("data_write, existing variable label but missing value labels", {
  d <- data.frame(
    a = letters[1:3],
    stringsAsFactors = FALSE
  )
  d$a <- assign_labels(d$a, variable = "First")
  # expect message, but no error
  expect_message(data_write(d, tmp), regex = "Preparing")

  # check if data is really the same
  d2 <- data_read(tmp)
  expect_identical(d2, d)
})
