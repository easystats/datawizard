skip_on_cran()
skip_if_not_installed("httr")

# SPSS file -----------------------------------

temp_file <- tempfile(fileext = ".sav")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/EFC.sav")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read", {
  d <- data_read(temp_file)
  expect_equal(sum(sapply(d, is.factor)), 15)
  expect_equal(sum(sapply(d, is.numeric)), 11)
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
