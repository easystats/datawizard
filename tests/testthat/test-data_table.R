data(efc)

test_that("data_table factor", {
  x <- data_table(efc$e42dep)
  expect_equal(as.vector(x$Value), as.vector(sort(unique(addNA(efc$e42dep)))))
  expect_equal(x$N, as.vector(table(addNA(efc$e42dep))))
  expect_equal(x$`Valid %`, as.vector(c(100 * table(efc$e42dep) / sum(!is.na(efc$e42dep)), NA)), ignore_attr = TRUE, tolerance = 1e-3)
})


test_that("data_table numeric", {
  x <- data_table(efc$neg_c_7)
  expect_equal(as.vector(x$Value), as.vector(sort(unique(addNA(efc$neg_c_7)))))
  expect_equal(x$N, as.vector(table(addNA(efc$neg_c_7))))
  expect_equal(x$`Valid %`, as.vector(c(100 * table(efc$neg_c_7) / sum(!is.na(efc$neg_c_7)), NA)), ignore_attr = TRUE, tolerance = 1e-3)
})


test_that("data_table data.frame", {
  x <- data_table(efc, c("e16sex", "c172code"))
  expect_s3_class(x, "list")
  expect_equal(length(x), 2)
  expect_equal(
    attributes(x[[1]]),
    list(names = c("Value", "N", "Raw %", "Valid %", "Cumulative %"),
         row.names = 1:3, class = c("dw_data_table", "data.frame"),
         type = "numeric", varname = "e16sex", label = "elder's gender",
         object = "x[[i]]", total_n = 100L, valid_n = 100)
  )
  expect_equal(
    attributes(x[[2]]),
    list(names = c("Value", "N", "Raw %", "Valid %", "Cumulative %"),
         row.names = 1:4, class = c("dw_data_table", "data.frame"),
         type = "numeric", varname = "c172code",
         label = "carer's level of education", object = "x[[i]]",
         total_n = 100L, valid_n = 90L)
  )
  table1 <- x[[1]]
  expect_equal(as.vector(table1$Value), as.character(c(sort(unique(efc$e16sex)), NA)))
  expect_equal(table1$N, as.vector(table(addNA(efc$e16sex))))
  expect_equal(table1$`Valid %`, as.vector(c(100 * table(efc$e16sex) / sum(!is.na(efc$e16sex)), NA)), ignore_attr = TRUE, tolerance = 1e-3)
})


test_that("data_table print", {
  x <- data_table(efc$e42dep)
  out <- capture.output(print(x))
  expect_equal(
    out,
    c("elder's dependency (efc$e42dep) <categorical>",
      "# total N=100 valid N=97",
      "",
      "Value |  N | Raw % | Valid % | Cumulative %",
      "------+----+-------+---------+-------------",
      "1     |  2 |  2.00 |    2.06 |         2.06",
      "2     |  4 |  4.00 |    4.12 |         6.19",
      "3     | 28 | 28.00 |   28.87 |        35.05",
      "4     | 63 | 63.00 |   64.95 |       100.00",
      "<NA>  |  3 |  3.00 |    <NA> |         <NA>")
  )
})


skip_if_not_installed("poorman")

test_that("data_table grouped data.frame", {
  x <- data_table(group_by(efc, e16sex), "c172code")
  expect_s3_class(x, "list")
  expect_equal(length(x), 2)
  expect_equal(
    attributes(x[[1]]),
    list(names = c("Value", "N", "Raw %", "Valid %", "Cumulative %"),
         row.names = 1:4, class = c("dw_data_table", "data.frame"),
         type = "numeric", varname = "c172code", object = "x[[i]]",
         group_variable = structure(list(e16sex = 1), .drop = TRUE, row.names = 1L, class = "data.frame"),
         total_n = 46L, valid_n = 41L)
  )
  table1 <- x[[1]]
  expect_equal(as.vector(table1$Value), as.character(c(sort(unique(efc$c172code)), NA)))
  expect_equal(table1$N, as.vector(table(addNA(efc$c172code[efc$e16sex == 1]))))
  expect_equal(table1$`Valid %`, as.vector(c(100 * table(efc$c172code[efc$e16sex == 1]) / sum(!is.na(efc$c172code[efc$e16sex == 1])), NA)), ignore_attr = TRUE, tolerance = 1e-3)
})


test_that("data_table print", {
  x <- data_table(group_by(efc, e16sex), "c172code")
  out <- capture.output(print(x))
  c("c172code <numeric>",
    "Grouped by e16sex (1)",
    "# total N=46 valid N=41",
    "",
    "Value |  N | Raw % | Valid % | Cumulative %",
    "------+----+-------+---------+-------------",
    "1     |  5 | 10.87 |   12.20 |        12.20",
    "2     | 32 | 69.57 |   78.05 |        90.24",
    "3     |  4 |  8.70 |    9.76 |       100.00",
    "<NA>  |  5 | 10.87 |    <NA> |         <NA>",
    "",
    "c172code <numeric>",
    "Grouped by e16sex (2)",
    "# total N=54 valid N=49",
    "", "Value |  N | Raw % | Valid % | Cumulative %",
    "------+----+-------+---------+-------------",
    "1     |  3 |  5.56 |    6.12 |         6.12",
    "2     | 34 | 62.96 |   69.39 |        75.51",
    "3     | 12 | 22.22 |   24.49 |       100.00",
    "<NA>  |  5 |  9.26 |    <NA> |         <NA>"
  )
})
