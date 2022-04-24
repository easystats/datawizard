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
  table1 <- x[[1]]
  expect_equal(as.vector(table1$Value), as.character(c(sort(unique(efc$e16sex)), NA)))
  expect_equal(table1$N, as.vector(table(addNA(efc$e16sex))))
  expect_equal(table1$`Valid %`, as.vector(c(100 * table(efc$e16sex) / sum(!is.na(efc$e16sex)), NA)), ignore_attr = TRUE, tolerance = 1e-3)
})


test_that("data_table print", {
  x <- data_table(efc$e42dep)
  out <- capture.output(print(x))
  c("elder's dependency (efc$e42dep) <categorical>",
    "# total N=100 valid N=100",
    "",
    "Value |  N | Raw % | Valid % | Cumulative %",
    "------+----+-------+---------+-------------",
    "1     |  2 |  2.00 |    2.06 |         2.06",
    "2     |  4 |  4.00 |    4.12 |         6.19",
    "3     | 28 | 28.00 |   28.87 |        35.05",
    "4     | 63 | 63.00 |   64.95 |       100.00",
    "<NA>  |  3 |  3.00 |    <NA> |         <NA>")
})
