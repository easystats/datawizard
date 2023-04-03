# numeric
test_that("to_factor", {
  x <- c(10, 11, 12)
  expect_identical(
    to_factor(x),
    structure(1:3, .Label = c("10", "11", "12"), class = "factor")
  )

  data(efc)
  x <- to_factor(efc$c172code)
  expect_identical(
    levels(x),
    c(
      "low level of education", "intermediate level of education",
      "high level of education"
    )
  )
  x <- to_factor(efc$c172code, labels_to_levels = FALSE)
  expect_identical(levels(x), c("1", "2", "3"))
})

# numeric, partially labelled
test_that("to_factor", {
  x <- c(10, 11, 12)
  attr(x, "labels") <- c("ten" = 10, "twelve" = 12)
  expect_message(
    expect_identical(
      to_factor(x),
      structure(1:3, levels = c("ten", "11", "twelve"), class = "factor")
    ),
    regexp = "Not all factor levels"
  )
})

# factor
test_that("to_factor", {
  data(efc)
  expect_identical(to_factor(efc$e42dep), efc$e42dep)
})

# data frame
test_that("to_factor", {
  data(iris)
  out <- to_factor(iris)
  expect_identical(out$Species, iris$Species)
  expect_true(all(vapply(out, is.factor, TRUE)))
  expect_identical(
    levels(out$Sepal.Length),
    c(
      "4.3", "4.4", "4.5", "4.6", "4.7", "4.8", "4.9", "5", "5.1",
      "5.2", "5.3", "5.4", "5.5", "5.6", "5.7", "5.8", "5.9", "6",
      "6.1", "6.2", "6.3", "6.4", "6.5", "6.6", "6.7", "6.8", "6.9",
      "7", "7.1", "7.2", "7.3", "7.4", "7.6", "7.7", "7.9"
    )
  )

  out <- to_factor(iris, select = starts_with("Sep"), append = TRUE)
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "Sepal.Length_f", "Sepal.Width_f"
    )
  )
  expect_identical(sum(vapply(out, is.factor, TRUE)), 3L)
})


# select helpers ------------------------------
test_that("to_factor regex", {
  expect_identical(
    to_factor(mtcars, select = "yl", regex = TRUE),
    to_factor(mtcars, select = "cyl")
  )
  expect_identical(
    to_factor(mtcars, select = "yl$", regex = TRUE),
    to_factor(mtcars, select = "cyl")
  )
})



# SPSS file, many value labels  -----------------------------------

skip_on_cran()
skip_if_offline()

skip_if_not_installed("httr")
skip_if_not_installed("haven")

# Output validated against SPSS output from original dataset

test_that("data_read, convert many labels correctly", {
  temp_file <- tempfile(fileext = ".sav")
  request <- httr::GET("https://raw.github.com/easystats/circus/master/data/spss_many_labels.sav")
  httr::stop_for_status(request)
  writeBin(httr::content(request, type = "raw"), temp_file)

  d <- data_read(
    temp_file,
    convert_factors = FALSE,
    verbose = FALSE
  )
  expect_identical(
    levels(to_factor(d$selv1)),
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
  expect_snapshot(data_tabulate(to_factor(d$selv1)))

  expect_identical(levels(to_factor(d$c12)), c("ja", "nein", "keine Angabe"))
  expect_snapshot(data_tabulate(to_factor(d$c12)))

  expect_identical(levels(to_factor(d$c12a)), c("Filter", "ja", "nein", "keine Angabe"))
  expect_snapshot(data_tabulate(to_factor(d$c12a)))
  expect_identical(
    levels(to_factor(d$c12c)),
    c(
      "Filter", "0 = keine", "1", "2", "3", "4", "5", "6", "7", "8",
      "9", "10 = sehr starke", "weiß nicht / keine Angabe"
    )
  )
  expect_snapshot(data_tabulate(to_factor(d$c12c)))
  unlink(temp_file)
})
