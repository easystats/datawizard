skip_on_cran()
skip_if_offline()

skip_if_not_installed("httr")
skip_if_not_installed("readxl")
skip_if_not_installed("haven")
skip_if_not_installed("readr")
skip_if_not_installed("data.table")
skip_if_not_installed("rio")


# csv -------------------------

test_that("data_read", {
  d <- data_read(
    "https://raw.githubusercontent.com/easystats/circus/master/data/bootstrapped.csv",
    verbose = FALSE
  )
  expect_identical(dim(d), c(10000L, 4L))
})



# csv -------------------------

test_that("data_read, skip_empty", {
  d <- data_read(
    "https://raw.githubusercontent.com/easystats/circus/master/data/test_skip_empty.csv",
    verbose = FALSE
  )
  expect_identical(ncol(d), 3L)
  expect_identical(colnames(d), c("Var1", "Var2", "Var3"))
})



# tsv -------------------------

temp_file <- tempfile(fileext = ".tsv")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/sample1.tsv")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(
    temp_file,
    verbose = FALSE
  )
  expect_identical(nrow(d), 3L)
  expect_identical(colnames(d), c("a", "b", "c"))
  expect_identical(sum(vapply(d, is.numeric, FUN.VALUE = logical(1L))), 2L)
  expect_identical(sum(vapply(d, is.character, FUN.VALUE = logical(1L))), 1L)
})

unlink(temp_file)



# excel -------------------------

temp_file <- tempfile(fileext = ".xlsx")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/sample1.xlsx")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(
    temp_file,
    verbose = FALSE
  )
  expect_identical(nrow(d), 3L)
  expect_identical(colnames(d), c("a", "b", "c"))
  expect_identical(sum(vapply(d, is.numeric, FUN.VALUE = logical(1L))), 2L)
  expect_identical(sum(vapply(d, is.character, FUN.VALUE = logical(1L))), 1L)
})

unlink(temp_file)



# Stata file -----------------------------------

temp_file <- tempfile(fileext = ".dta")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/stata_test.dta")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(
    temp_file,
    verbose = FALSE
  )
  expect_identical(
    d,
    data.frame(
      mpg = c(21, 21, 22.8),
      cyl = c(6, 6, 4),
      disp = c(160, 160, 108)
    )
  )
})

unlink(temp_file)



# SAS file -----------------------------------

temp_file <- tempfile(fileext = ".sas7bdat")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/sas_test.sas7bdat")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(
    temp_file,
    verbose = FALSE
  )
  expect_identical(
    d,
    data.frame(
      mpg = c(21, 21, 22.8),
      cyl = c(6, 6, 4),
      disp = c(160, 160, 108)
    )
  )
})

unlink(temp_file)



# SPSS file -----------------------------------

temp_file <- tempfile(fileext = ".sav")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/EFC.sav")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(
    temp_file,
    verbose = FALSE
  )
  expect_identical(sum(vapply(d, is.factor, FUN.VALUE = logical(1L))), 15L)
  expect_identical(sum(vapply(d, is.numeric, FUN.VALUE = logical(1L))), 11L)
  expect_identical(
    levels(d$c172code),
    c(
      "low level of education",
      "intermediate level of education",
      "high level of education"
    )
  )
  expect_identical(
    attr(d$n4pstu, "labels"),
    c(
      `spouse/partner` = 1,
      child = 2,
      sibling = 3,
      `daughter or son -in-law` = 4
    )
  )
})

unlink(temp_file)



# SPSS file. 2 ---------------------------------

temp_file <- tempfile(fileext = ".sav")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/spss_test.sav")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(
    temp_file,
    verbose = FALSE
  )
  expect_identical(
    d,
    structure(
      list(
        V1 = structure(
          1:4,
          levels = c("Eins", "Zwei", "Drei", "Vier"),
          class = "factor", label = "Variable 1"
        ),
        V2 = structure(
          c(2, 3, 4, 1),
          labels = c(Eins = 1, Zwei = 2, Drei = 3),
          label = "Variable 2"
        ),
        V3 = structure(
          c(3L, 2L, 1L, 4L),
          levels = c("Eins", "Zwei", "Drei", "Vier"),
          class = "factor", label = "Variable 3"
        )
      ),
      row.names = c(NA, -4L), class = "data.frame"
    )
  )
})

unlink(temp_file)



# zipped SPSS file -----------------------------------

temp_file <- tempfile(fileext = ".zip")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/EFC.zip")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(
    temp_file,
    verbose = FALSE
  )
  expect_identical(sum(vapply(d, is.factor, FUN.VALUE = logical(1L))), 15L)
  expect_identical(sum(vapply(d, is.numeric, FUN.VALUE = logical(1L))), 11L)
})

test_that("data_read, convert_factors", {
  d <- data_read(
    temp_file,
    convert_factors = FALSE,
    verbose = FALSE
  )
  expect_identical(sum(vapply(d, is.factor, FUN.VALUE = logical(1L))), 0L)
  expect_identical(sum(vapply(d, is.numeric, FUN.VALUE = logical(1L))), 26L)
})

unlink(temp_file)
