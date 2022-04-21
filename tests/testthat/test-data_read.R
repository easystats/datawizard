skip_on_cran()

skip_if_not_installed("httr")
skip_if_not_installed("readxl")
skip_if_not_installed("haven")
skip_if_not_installed("readr")
skip_if_not_installed("data.table")
skip_if_not_installed("rio")


# csv -------------------------

test_that("data_read", {
  d <- data_read("https://stats.idre.ucla.edu/stat/data/binary.csv")
  expect_equal(nrow(d), 400)
})



# tsv -------------------------

temp_file <- tempfile(fileext = ".tsv")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/sample1.tsv")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(temp_file)
  expect_equal(nrow(d), 3)
  expect_equal(colnames(d), c("a", "b", "c"))
  expect_equal(sum(sapply(d, is.numeric)), 2)
  expect_equal(sum(sapply(d, is.character)), 1)
})

unlink(temp_file)



# excel -------------------------

temp_file <- tempfile(fileext = ".xlsx")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/sample1.xlsx")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(temp_file)
  expect_equal(nrow(d), 3)
  expect_equal(colnames(d), c("a", "b", "c"))
  expect_equal(sum(sapply(d, is.numeric)), 2)
  expect_equal(sum(sapply(d, is.character)), 1)
})

unlink(temp_file)



# SPSS file -----------------------------------

temp_file <- tempfile(fileext = ".sav")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/EFC.sav")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(temp_file)
  expect_equal(sum(sapply(d, is.factor)), 15)
  expect_equal(sum(sapply(d, is.numeric)), 11)
  expect_equal(levels(d$c172code), c("low level of education", "intermediate level of education", "high level of education"))
  expect_equal(attr(d$n4pstu, "labels"), c(`spouse/partner` = 1, child = 2, sibling = 3, `daughter or son -in-law` = 4))
})

unlink(temp_file)



# zipped SPSS file -----------------------------------

temp_file <- tempfile(fileext = ".zip")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/EFC.zip")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(temp_file)
  expect_equal(sum(sapply(d, is.factor)), 15)
  expect_equal(sum(sapply(d, is.numeric)), 11)
})

unlink(temp_file)
