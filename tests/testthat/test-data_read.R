skip_on_cran()
skip_if_offline()

skip_if_not_installed("httr")
skip_if_not_installed("readxl")
skip_if_not_installed("haven")
skip_if_not_installed("readr")
skip_if_not_installed("data.table")
skip_if_not_installed("rio")

# csv -------------------------

test_that("data_read - csv", {
  d <- data_read(
    "https://raw.githubusercontent.com/easystats/circus/main/data/bootstrapped.csv",
    verbose = FALSE
  )
  expect_identical(dim(d), c(10000L, 4L))
})



# csv -------------------------

test_that("data_read, skip_empty", {
  d <- data_read(
    "https://raw.githubusercontent.com/easystats/circus/main/data/test_skip_empty.csv",
    verbose = FALSE
  )
  expect_identical(ncol(d), 3L)
  expect_identical(colnames(d), c("Var1", "Var2", "Var3"))
})



# tsv -------------------------

test_that("data_read - tsv", {
  skip_if_not_installed("withr")

  withr::with_tempfile("temp_file", fileext = ".tsv", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/sample1.tsv")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

    d <- data_read(
      temp_file,
      verbose = FALSE
    )
    expect_identical(nrow(d), 3L)
    expect_identical(colnames(d), c("a", "b", "c"))
    expect_identical(sum(vapply(d, is.numeric, FUN.VALUE = logical(1L))), 2L)
    expect_identical(sum(vapply(d, is.character, FUN.VALUE = logical(1L))), 1L)
  })
})



# excel -------------------------

test_that("data_read - excel", {
  skip_if_not_installed("withr")

  withr::with_tempfile("temp_file", fileext = ".xlsx", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/sample1.xlsx")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

    d <- data_read(
      temp_file,
      verbose = FALSE
    )

    expect_identical(nrow(d), 3L)
    expect_identical(colnames(d), c("a", "b", "c"))
    expect_identical(sum(vapply(d, is.numeric, FUN.VALUE = logical(1L))), 2L)
    expect_identical(sum(vapply(d, is.character, FUN.VALUE = logical(1L))), 1L)
  })
})


# Stata file -----------------------------------

test_that("data_read - Stata file", {
  withr::with_tempfile("temp_file", fileext = ".dta", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/stata_test.dta")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

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
})



# SAS file -----------------------------------


test_that("data_read - SAS file", {
  withr::with_tempfile("temp_file", fileext = ".sas7bdat", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/sas_test.sas7bdat")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

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
})




# RDS file, matrix, coercible -----------------------------------

test_that("data_read - RDS file, matrix, coercible", {
  withr::with_tempfile("temp_file", fileext = ".rds", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/matrix_object.rds")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

    expect_message(expect_message(expect_message({
      d <- data_read(
        temp_file,
        verbose = TRUE
      )
    })), regex = "0 out of 5")

    expect_s3_class(d, "data.frame")
    expect_identical(dim(d), c(2L, 5L))
  })
})


# SPSS file -----------------------------------

test_that("data_read - SPSS file", {
  withr::with_tempfile("temp_file", fileext = ".sav", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/EFC.sav")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

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
})




# SPSS file 2 ---------------------------------

test_that("data_read - SPSS file 2", {
  withr::with_tempfile("temp_file", fileext = ".sav", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/spss_test.sav")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

    d <- data_read(
      temp_file,
      verbose = FALSE
    )

    expect_identical(
      d,
      structure(list(
        V1 = structure(1:4,
          levels = c(
            "Eins", "Zwei",
            "Drei", "Vier"
          ),
          class = "factor",
          converted_to_factor = TRUE,
          label = "Variable 1"
        ),
        V2 = structure(c(2, 3, 4, 1),
          labels = c(
            Eins = 1, Zwei = 2,
            Drei = 3
          ),
          label = "Variable 2"
        ),
        V3 = structure(
          c(
            3L, 2L,
            1L, 4L
          ),
          levels = c("Eins", "Zwei", "Drei", "Vier"),
          class = "factor",
          converted_to_factor = TRUE,
          label = "Variable 3"
        )
      ), row.names = c(NA, -4L), class = "data.frame")
    )
  })
})



# zipped SPSS file -----------------------------------

test_that("data_read - zipped SPSS file", {
  withr::with_tempfile("temp_file", fileext = ".zip", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/EFC.zip")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

    d <- data_read(
      temp_file,
      verbose = FALSE
    )
    expect_identical(sum(vapply(d, is.factor, FUN.VALUE = logical(1L))), 15L)
    expect_identical(sum(vapply(d, is.numeric, FUN.VALUE = logical(1L))), 11L)

    d <- data_read(
      temp_file,
      convert_factors = FALSE,
      verbose = FALSE
    )
    expect_identical(sum(vapply(d, is.factor, FUN.VALUE = logical(1L))), 0L)
    expect_identical(sum(vapply(d, is.numeric, FUN.VALUE = logical(1L))), 26L)
  })
})



# SPSS file, many value labels  -----------------------------------

test_that("data_read, convert many labels correctly", {
  # Output validated against SPSS output from original dataset

  withr::with_tempfile("temp_file", fileext = ".sav", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/spss_many_labels.sav")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

    d <- data_read(
      temp_file,
      verbose = FALSE
    )
    # all are factors by default
    expect_identical(
      vapply(d, class, character(1)),
      c(selv1 = "factor", c12 = "factor", c12a = "factor", c12c = "factor")
    )
    expect_identical(
      levels(d$selv1),
      c(
        "Vignette 1 weiblich (Gülsen E. Reinigungskraft B)",
        "Vignette 2 weiblich (Gülsen E. Anwältin B)",
        "Vignette 3 weiblich (Monika E. Reinigungskraft B)",
        "Vignette 4 weiblich (Monika E. Anwältin B)",
        "Vignette 5 männlich (Hasan E. Reinigungskraft B)",
        "Vignette 6 männlich (Hasan E. Anwalt B)",
        "Vignette 7 männlich (Martin E. Reinigungskraft B)",
        "Vignette 8 männlich (Martin E. Anwalt B)",
        "Vignette 9 weiblich (Gülsen E. Reinigungskraft E)",
        "Vignette 10 weiblich (Gülsen E. Anwältin E)",
        "Vignette 11 weiblich (Monika E. Reinigungskraft E)",
        "Vignette 12 weiblich (Monika E. Anwältin E)",
        "Vignette 13 männlich (Hasan E. Reinigungskraft E)",
        "Vignette 14 männlich (Hasan E. Anwalt E)",
        "Vignette 15 männlich (Martin E. Reinigungskraft E)",
        "Vignette 16 männlich (Martin E. Anwalt E)"
      )
    )
    expect_snapshot(data_tabulate(d$selv1))

    expect_identical(levels(d$c12), c("ja", "nein", "keine Angabe"))
    expect_snapshot(data_tabulate(d$c12))

    expect_identical(levels(d$c12a), c("Filter", "ja", "nein", "keine Angabe"))
    expect_snapshot(data_tabulate(d$c12a))
    expect_identical(
      levels(d$c12c),
      c(
        "Filter", "0 = keine", "1", "2", "3", "4", "5", "6", "7", "8",
        "9", "10 = sehr starke", "weiß nicht / keine Angabe"
      )
    )
    expect_snapshot(data_tabulate(d$c12c))

    expect_message(
      expect_message(
        expect_message(
          data_read(temp_file),
          regexp = "Reading"
        ),
        regexp = "Variables where all"
      ),
      regexp = "4 out of 4"
    )

    d <- data_read(
      temp_file,
      convert_factors = FALSE,
      verbose = FALSE
    )
    # all are factors by default
    expect_identical(
      vapply(d, class, character(1)),
      c(selv1 = "numeric", c12 = "numeric", c12a = "numeric", c12c = "numeric")
    )
    expect_snapshot(table(d$selv1))
    expect_identical(
      attributes(d$selv1)$labels,
      c(
        `Vignette 1 weiblich (Gülsen E. Reinigungskraft B)` = 1,
        `Vignette 2 weiblich (Gülsen E. Anwältin B)` = 2,
        `Vignette 3 weiblich (Monika E. Reinigungskraft B)` = 3,
        `Vignette 4 weiblich (Monika E. Anwältin B)` = 4,
        `Vignette 5 männlich (Hasan E. Reinigungskraft B)` = 5,
        `Vignette 6 männlich (Hasan E. Anwalt B)` = 6,
        `Vignette 7 männlich (Martin E. Reinigungskraft B)` = 7,
        `Vignette 8 männlich (Martin E. Anwalt B)` = 8,
        `Vignette 9 weiblich (Gülsen E. Reinigungskraft E)` = 9,
        `Vignette 10 weiblich (Gülsen E. Anwältin E)` = 10,
        `Vignette 11 weiblich (Monika E. Reinigungskraft E)` = 11,
        `Vignette 12 weiblich (Monika E. Anwältin E)` = 12,
        `Vignette 13 männlich (Hasan E. Reinigungskraft E)` = 13,
        `Vignette 14 männlich (Hasan E. Anwalt E)` = 14,
        `Vignette 15 männlich (Martin E. Reinigungskraft E)` = 15,
        `Vignette 16 männlich (Martin E. Anwalt E)` = 16,
        `99` = 99
      )
    )

    expect_snapshot(table(d$c12))
    expect_identical(attributes(d$c12)$labels, c(Filter = -2, ja = 1, nein = 2, `keine Angabe` = 99))

    expect_snapshot(table(d$c12a))
    expect_identical(attributes(d$c12a)$labels, c(Filter = -2, ja = 1, nein = 2, `keine Angabe` = 99))

    expect_snapshot(table(d$c12c))
    expect_identical(
      attributes(d$c12c)$labels,
      c(
        Filter = -2, `0 = keine` = 0, `1` = 1, `2` = 2, `3` = 3, `4` = 4,
        `5` = 5, `6` = 6, `7` = 7, `8` = 8, `9` = 9, `10 = sehr starke` = 10,
        `weiß nicht / keine Angabe` = 99
      )
    )
  })
})




# invalid file type -------------------------

test_that("data_read, no file extension", {
  expect_error(data_read("mytestfile"), regex = "extension")
  expect_error(data_read(NULL, regex = "extension"))
})


# file not exists -------------------------

test_that("data_read, file not exists", {
  expect_error(data_read("thisfileshouldnotexist.csv"), regex = "not exist")
  expect_error(
    suppressMessages(data_read("thisfileshouldnotexist.sav")),
    regex = "not exist"
  )
})

# RDS file, no data frame -----------------------------------

test_that("data_read - RDS file, no data frame", {
  skip_if_not_installed("withr")

  withr::with_tempfile("temp_file", fileext = ".rds", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/model_object.rds")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

    expect_warning(
      {
        d <- data_read(
          temp_file,
          verbose = TRUE
        )
      },
      regex = "no data frame"
    )
    expect_s3_class(d, "lm")
  })
})
