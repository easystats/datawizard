skip_on_cran()
skip_if_offline()

skip_if_not_installed("httr")
skip_if_not_installed("haven")
skip_if_not_installed("readr")

# prepare data set ---------------

data(efc)
d <- data_filter(efc, filter = 1:5)
d$e42dep <- droplevels(d$e42dep)



# SPSS -------------------------------------

tmp <- tempfile(fileext = ".sav")
on.exit(unlink(tmp))

test_that("data_write, SPSS", {
  data_write(d, tmp)
  d2 <- data_read(tmp, verbose = FALSE)

  expect_equal(
    to_factor(d, select = c("e16sex", "c172code")),
    d2,
    ignore_attr = TRUE
  )
})



# Stata -------------------------------------

tmp <- tempfile(fileext = ".dta")
on.exit(unlink(tmp))

test_that("data_write, Stata", {
  data_write(d, tmp)
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

test_that("data_write, CSV, add labels as row", {
  data_write(d, tmp, save_variable_labels = TRUE)
  d2 <- data_read(tmp)

  expect_equal(
    d2$c12hour,
    c("average number of hours of care per week", "16", "148", "70", NA, "168"),
    ignore_attr = TRUE
  )
})
