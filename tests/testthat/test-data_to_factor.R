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

skip_if_not_or_load_if_installed("httr")
skip_if_not_or_load_if_installed("haven")

# Output validated against SPSS output from original dataset

temp_file <- tempfile(fileext = ".sav")
request <- httr::GET("https://raw.github.com/easystats/circus/master/data/spss_many_labels.sav")
httr::stop_for_status(request)
writeBin(httr::content(request, type = "raw"), temp_file)

test_that("data_read, convert many labels correctly", {
  d <- data_read(
    temp_file,
    convert_factors = FALSE,
    verbose = FALSE
  )
  expect_identical(
    levels(to_factor(d$selv1)),
    c(
      "Vignette 1 weiblich (Gülsen E. Reinigungskraft B)", "Vignette 2 weiblich (Gülsen E. Anwältin B)",
      "Vignette 3 weiblich (Monika E. Reinigungskraft B)", "Vignette 4 weiblich (Monika E. Anwältin B)",
      "Vignette 5 männlich (Hasan E. Reinigungskraft B)", "Vignette 6 männlich (Hasan E. Anwalt B)",
      "Vignette 7 männlich (Martin E. Reinigungskraft B)", "Vignette 8 männlich (Martin E. Anwalt B)",
      "Vignette 9 weiblich (Gülsen E. Reinigungskraft E)", "Vignette 10 weiblich (Gülsen E. Anwältin E)",
      "Vignette 11 weiblich (Monika E. Reinigungskraft E)", "Vignette 12 weiblich (Monika E. Anwältin E)",
      "Vignette 13 männlich (Hasan E. Reinigungskraft E)", "Vignette 14 männlich (Hasan E. Anwalt E)",
      "Vignette 15 männlich (Martin E. Reinigungskraft E)", "Vignette 16 männlich (Martin E. Anwalt E)"
    )
  )
  out <- capture.output(data_tabulate(to_factor(d$selv1)))
  expect_identical(
    out,
    c(
      "to_factor(d$selv1) <categorical>",
      "# total N=2413 valid N=2413",
      "",
      "Value                                              |   N | Raw % | Valid % | Cumulative %",
      "---------------------------------------------------+-----+-------+---------+-------------",
      "Vignette 1 weiblich (Gülsen E. Reinigungskraft B)  | 150 |  6.22 |    6.22 |         6.22",
      "Vignette 2 weiblich (Gülsen E. Anwältin B)         | 150 |  6.22 |    6.22 |        12.43",
      "Vignette 3 weiblich (Monika E. Reinigungskraft B)  | 150 |  6.22 |    6.22 |        18.65",
      "Vignette 4 weiblich (Monika E. Anwältin B)         | 151 |  6.26 |    6.26 |        24.91",
      "Vignette 5 männlich (Hasan E. Reinigungskraft B)   | 151 |  6.26 |    6.26 |        31.16",
      "Vignette 6 männlich (Hasan E. Anwalt B)            | 153 |  6.34 |    6.34 |        37.51",
      "Vignette 7 männlich (Martin E. Reinigungskraft B)  | 150 |  6.22 |    6.22 |        43.72",
      "Vignette 8 männlich (Martin E. Anwalt B)           | 150 |  6.22 |    6.22 |        49.94",
      "Vignette 9 weiblich (Gülsen E. Reinigungskraft E)  | 151 |  6.26 |    6.26 |        56.20",
      "Vignette 10 weiblich (Gülsen E. Anwältin E)        | 150 |  6.22 |    6.22 |        62.41",
      "Vignette 11 weiblich (Monika E. Reinigungskraft E) | 150 |  6.22 |    6.22 |        68.63",
      "Vignette 12 weiblich (Monika E. Anwältin E)        | 151 |  6.26 |    6.26 |        74.89",
      "Vignette 13 männlich (Hasan E. Reinigungskraft E)  | 155 |  6.42 |    6.42 |        81.31",
      "Vignette 14 männlich (Hasan E. Anwalt E)           | 150 |  6.22 |    6.22 |        87.53",
      "Vignette 15 männlich (Martin E. Reinigungskraft E) | 150 |  6.22 |    6.22 |        93.74",
      "Vignette 16 männlich (Martin E. Anwalt E)          | 151 |  6.26 |    6.26 |       100.00",
      "<NA>                                               |   0 |  0.00 |    <NA> |         <NA>"
    )
  )

  expect_identical(levels(to_factor(d$c12)), c("ja", "nein", "keine Angabe"))
  out <- capture.output(data_tabulate(to_factor(d$c12)))
  expect_identical(
    out,
    c(
      "Sind oder waren Sie schon einmal selbst von solchen Beschwerden betroffen? (to_factor(d$c12)) <categorical>",
      "# total N=2413 valid N=2413",
      "",
      "Value        |    N | Raw % | Valid % | Cumulative %",
      "-------------+------+-------+---------+-------------",
      "ja           |  786 | 32.57 |   32.57 |        32.57",
      "nein         | 1616 | 66.97 |   66.97 |        99.54",
      "keine Angabe |   11 |  0.46 |    0.46 |       100.00",
      "<NA>         |    0 |  0.00 |    <NA> |         <NA>"
    )
  )

  expect_identical(levels(to_factor(d$c12a)), c("Filter", "ja", "nein", "keine Angabe"))
  out <- capture.output(data_tabulate(to_factor(d$c12a)))
  expect_identical(
    out,
    c(
      "Haben Sie deswegen Behandlung(en) in Anspruch genommen? (to_factor(d$c12a)) <categorical>",
      "# total N=2413 valid N=2413",
      "",
      "Value        |    N | Raw % | Valid % | Cumulative %",
      "-------------+------+-------+---------+-------------",
      "Filter       | 1627 | 67.43 |   67.43 |        67.43",
      "ja           |  500 | 20.72 |   20.72 |        88.15",
      "nein         |  285 | 11.81 |   11.81 |        99.96",
      "keine Angabe |    1 |  0.04 |    0.04 |       100.00",
      "<NA>         |    0 |  0.00 |    <NA> |         <NA>"
    )
  )
  expect_identical(
    levels(to_factor(d$c12c)),
    c(
      "Filter", "0 = keine", "1", "2", "3", "4", "5", "6", "7", "8",
      "9", "10 = sehr starke", "weiß nicht / keine Angabe"
    )
  )
  out <- capture.output(data_tabulate(to_factor(d$c12c)))
  expect_identical(
    out,
    c(
      "Wie sehr haben diese Behandlung(en) Ihre Beeinträchtigung durch die Beschwerden verbessert? (to_factor(d$c12c)) <categorical>", # nolint
      "# total N=2413 valid N=2413",
      "",
      "Value                     |    N | Raw % | Valid % | Cumulative %",
      "--------------------------+------+-------+---------+-------------",
      "Filter                    | 1913 | 79.28 |   79.28 |        79.28",
      "0 = keine                 |   34 |  1.41 |    1.41 |        80.69",
      "1                         |    2 |  0.08 |    0.08 |        80.77",
      "2                         |   11 |  0.46 |    0.46 |        81.23",
      "3                         |   14 |  0.58 |    0.58 |        81.81",
      "4                         |   19 |  0.79 |    0.79 |        82.59",
      "5                         |   61 |  2.53 |    2.53 |        85.12",
      "6                         |   42 |  1.74 |    1.74 |        86.86",
      "7                         |   63 |  2.61 |    2.61 |        89.47",
      "8                         |   97 |  4.02 |    4.02 |        93.49",
      "9                         |   53 |  2.20 |    2.20 |        95.69",
      "10 = sehr starke          |   99 |  4.10 |    4.10 |        99.79",
      "weiß nicht / keine Angabe |    5 |  0.21 |    0.21 |       100.00",
      "<NA>                      |    0 |  0.00 |    <NA> |         <NA>"
    )
  )
})

unlink(temp_file)
