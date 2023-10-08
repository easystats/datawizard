skip_if_not_installed("httr")
skip_if_not_installed("haven")
skip_if_not_installed("readr")

skip_on_cran()

skip_if_not_installed("curl")
skip_if_offline()

# prepare data set ---------------

data(efc)
d <- data_filter(efc, 1:5)
d$e42dep <- droplevels(d$e42dep)



# SPSS -------------------------------------

test_that("data_write, SPSS", {
  skip_if_not_installed("withr")
  withr::with_tempfile("tmp", fileext = ".sav", code = {
    expect_message(data_write(d, tmp))
    d2 <- data_read(tmp, verbose = FALSE)
    expect_equal(
      to_factor(d, select = c("e16sex", "c172code")),
      d2,
      ignore_attr = TRUE
    )
  })
})


test_that("data_write, SPSS, mixed types of labelled vectors", {
  skip_if_not_installed("withr")
  withr::with_tempfile("tmp", fileext = ".sav", code = {
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

    expect_message(data_write(d, tmp), regex = "Preparing")
  })
})



# Stata -------------------------------------

test_that("data_write, Stata", {
  skip_if_not_installed("withr")
  withr::with_tempfile("tmp", fileext = ".dta", code = {
    data_write(d, tmp, verbose = FALSE)
    d2 <- data_read(tmp, verbose = FALSE)

    expect_equal(
      to_factor(d, select = c("e16sex", "c172code")),
      d2,
      ignore_attr = TRUE
    )
  })
})



# csv -------------------------

test_that("data_write, CSV, keep numeric", {
  skip_if_not_installed("withr")
  withr::with_tempfile("tmp", fileext = ".csv", code = {
    data_write(d, tmp)
    d2 <- data_read(tmp)

    expect_equal(
      to_numeric(d, dummy_factors = FALSE, preserve_levels = TRUE),
      d2,
      ignore_attr = TRUE
    )
  })
})

test_that("data_write, CSV, convert to factor", {
  skip_if_not_installed("withr")
  withr::with_tempfile("tmp", fileext = ".csv", code = {
    data_write(d, tmp, convert_factors = TRUE)
    d2 <- data_read(tmp)
    out <- to_factor(d, select = c("e16sex", "c172code"))
    out$e16sex <- as.character(out$e16sex)
    out$c172code <- as.character(out$c172code)
    out$e42dep <- as.numeric(as.character(out$e42dep))
    expect_equal(out, d2, ignore_attr = TRUE)
  })
})

test_that("data_write, CSV, create labels file", {
  skip_if_not_installed("withr")
  withr::with_tempfile("tmp", fileext = ".csv", code = {
    # file for labels
    fpath <- dirname(tmp)
    fname <- sub("\\.csv$", "", basename(tmp))
    tmp2 <- paste0(fpath, .Platform$file.sep, fname, "_labels.csv")
    on.exit(unlink(tmp2))

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
})


# invalid file type -------------------------

test_that("data_write, no file extension", {
  expect_error(data_write(d, "mytestfile"))
  expect_error(data_write(d, NULL))
})


# writing character vector works for missing value labels ------------------

test_that("data_write, existing variable label but missing value labels", {
  skip_if_not_installed("withr")
  withr::with_tempfile("tmp", fileext = ".sav", code = {
    d <- data.frame(
      a = letters[1:3],
      stringsAsFactors = FALSE
    )
    d$a <- assign_labels(d$a, variable = "First")
    # expect message, but no error
    expect_message(data_write(d, tmp), regex = "Preparing")

    # check if data is really the same
    d2 <- data_read(tmp, verbose = FALSE)
    expect_identical(d2, d)
  })
})
