data(efc)

test_that("data_tabulate factor", {
  x <- data_tabulate(efc$e42dep)
  expect_equal(as.vector(x$Value), as.vector(sort(unique(
    addNA(efc$e42dep)
  ))))
  expect_equal(x$N, as.vector(table(addNA(efc$e42dep))))
  expect_equal(x$`Valid %`,
    as.vector(c(
      100 * table(efc$e42dep) / sum(!is.na(efc$e42dep)), NA
    )),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_tabulate numeric", {
  x <- data_tabulate(efc$neg_c_7)
  expect_equal(as.vector(x$Value), as.vector(sort(unique(
    addNA(efc$neg_c_7)
  ))))
  expect_equal(x$N, as.vector(table(addNA(efc$neg_c_7))))
  expect_equal(x$`Valid %`,
    as.vector(c(
      100 * table(efc$neg_c_7) / sum(!is.na(efc$neg_c_7)), NA
    )),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_tabulate data.frame", {
  x <- data_tabulate(efc, c("e16sex", "c172code"))
  expect_s3_class(x, "list")
  expect_equal(length(x), 2)
  expect_equal(
    attributes(x[[1]]),
    list(
      names = c(
        "Variable", "Value", "N", "Raw %", "Valid %",
        "Cumulative %"
      ),
      class = c("dw_data_tabulate", "data.frame"),
      row.names = 1:3,
      type = "numeric",
      varname = "e16sex",
      label = "elder's gender",
      object = "e16sex",
      duplicate_varnames = c(FALSE, TRUE, TRUE),
      total_n = 100L,
      valid_n = 100L
    )
  )
  expect_equal(
    attributes(x[[2]]),
    list(
      names = c(
        "Variable", "Value", "N", "Raw %", "Valid %",
        "Cumulative %"
      ),
      class = c("dw_data_tabulate", "data.frame"),
      row.names = 1:4,
      type = "numeric",
      varname = "c172code",
      label = "carer's level of education",
      object = "c172code",
      duplicate_varnames = c(FALSE, TRUE, TRUE, TRUE),
      total_n = 100L,
      valid_n = 90L
    )
  )
  table1 <- x[[1]]
  expect_equal(as.vector(table1$Value), as.character(c(sort(
    unique(efc$e16sex)
  ), NA)))
  expect_equal(table1$N, as.vector(table(addNA(efc$e16sex))))
  expect_equal(table1$`Valid %`,
    as.vector(c(
      100 * table(efc$e16sex) / sum(!is.na(efc$e16sex)), NA
    )),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_tabulate print", {
  set.seed(123)
  x <- sample(1:3, 1e6, TRUE)
  out <- data_tabulate(x, name = "Large Number")
  expect_equal(
    attributes(out),
    list(
      names = c("Variable", "Value", "N", "Raw %", "Valid %", "Cumulative %"),
      class = c("dw_data_tabulate", "data.frame"),
      row.names = 1:4,
      type = "integer",
      varname = "Large Number",
      object = "x",
      duplicate_varnames = c(FALSE, TRUE, TRUE, TRUE),
      total_n = 1000000L,
      valid_n = 1000000L
    )
  )
})


test_that("data_tabulate print", {
  x <- data_tabulate(efc$e42dep)
  out <- capture.output(print(x))
  expect_equal(
    out,
    c(
      "elder's dependency (efc$e42dep) <categorical>",
      "# total N=100 valid N=97",
      "",
      "Value |  N | Raw % | Valid % | Cumulative %",
      "------+----+-------+---------+-------------",
      "1     |  2 |  2.00 |    2.06 |         2.06",
      "2     |  4 |  4.00 |    4.12 |         6.19",
      "3     | 28 | 28.00 |   28.87 |        35.05",
      "4     | 63 | 63.00 |   64.95 |       100.00",
      "<NA>  |  3 |  3.00 |    <NA> |         <NA>"
    )
  )
})


test_that("data_tabulate print multiple", {
  x <- data_tabulate(efc, c("c172code", "e16sex"))
  out <- capture.output(print(x))
  expect_equal(
    out,
    c(
      "carer's level of education (c172code) <numeric>",
      "# total N=100 valid N=90",
      "",
      "Value |  N | Raw % | Valid % | Cumulative %",
      "------+----+-------+---------+-------------",
      "1     |  8 |  8.00 |    8.89 |         8.89",
      "2     | 66 | 66.00 |   73.33 |        82.22",
      "3     | 16 | 16.00 |   17.78 |       100.00",
      "<NA>  | 10 | 10.00 |    <NA> |         <NA>",
      "",
      "elder's gender (e16sex) <numeric>",
      "# total N=100 valid N=100",
      "",
      "Value |  N | Raw % | Valid % | Cumulative %",
      "------+----+-------+---------+-------------",
      "1     | 46 | 46.00 |   46.00 |        46.00",
      "2     | 54 | 54.00 |   54.00 |       100.00",
      "<NA>  |  0 |  0.00 |    <NA> |         <NA>"
    )
  )
})


test_that("data_tabulate big numbers", {
  set.seed(123)
  x <- sample(1:5, size = 1e7, TRUE)
  out <- capture.output(print(data_tabulate(x)))
  expect_equal(
    out,
    c(
      "x <integer>",
      "# total N=10,000,000 valid N=10,000,000",
      "",
      "Value |         N | Raw % | Valid % | Cumulative %",
      "------+-----------+-------+---------+-------------",
      "1     | 1,998,318 | 19.98 |   19.98 |        19.98",
      "2     | 1,998,338 | 19.98 |   19.98 |        39.97",
      "3     | 2,001,814 | 20.02 |   20.02 |        59.98",
      "4     | 1,999,423 | 19.99 |   19.99 |        79.98",
      "5     | 2,002,107 | 20.02 |   20.02 |       100.00",
      "<NA>  |         0 |  0.00 |    <NA> |         <NA>"
    )
  )
  out <- capture.output(print(data_tabulate(x), big_mark = ""))
  expect_equal(
    out,
    c(
      "x <integer>",
      "# total N=10000000 valid N=10000000",
      "",
      "Value |       N | Raw % | Valid % | Cumulative %",
      "------+---------+-------+---------+-------------",
      "1     | 1998318 | 19.98 |   19.98 |        19.98",
      "2     | 1998338 | 19.98 |   19.98 |        39.97",
      "3     | 2001814 | 20.02 |   20.02 |        59.98",
      "4     | 1999423 | 19.99 |   19.99 |        79.98",
      "5     | 2002107 | 20.02 |   20.02 |       100.00",
      "<NA>  |       0 |  0.00 |    <NA> |         <NA>"
    )
  )
  out <- capture.output(print(data_tabulate(x), big_mark = "-"))
  expect_equal(
    out,
    c(
      "x <integer>",
      "# total N=10-000-000 valid N=10-000-000",
      "",
      "Value |         N | Raw % | Valid % | Cumulative %",
      "------+-----------+-------+---------+-------------",
      "1     | 1-998-318 | 19.98 |   19.98 |        19.98",
      "2     | 1-998-338 | 19.98 |   19.98 |        39.97",
      "3     | 2-001-814 | 20.02 |   20.02 |        59.98",
      "4     | 1-999-423 | 19.99 |   19.99 |        79.98",
      "5     | 2-002-107 | 20.02 |   20.02 |       100.00",
      "<NA>  |         0 |  0.00 |    <NA> |         <NA>"
    )
  )
})


if (packageVersion("insight") > "0.17.0") {
  test_that("data_tabulate print multiple, collapse", {
    x <- data_tabulate(efc, c("c172code", "e16sex"), collapse = TRUE)
    out <- capture.output(print(x))
    expect_equal(
      out,
      c(
        "# Frequency Table",
        "",
        "Variable | Value |  N | Raw % | Valid % | Cumulative %",
        "---------+-------+----+-------+---------+-------------",
        "c172code |     1 |  8 |  8.00 |    8.89 |         8.89",
        "         |     2 | 66 | 66.00 |   73.33 |        82.22",
        "         |     3 | 16 | 16.00 |   17.78 |       100.00",
        "         |  <NA> | 10 | 10.00 |    <NA> |         <NA>",
        "---------+-------+----+-------+---------+-------------",
        "e16sex   |     1 | 46 | 46.00 |   46.00 |        46.00",
        "         |     2 | 54 | 54.00 |   54.00 |       100.00",
        "         |  <NA> |  0 |  0.00 |    <NA> |         <NA>",
        "------------------------------------------------------"
      )
    )
  })
}


skip_if_not_installed("poorman")

if (requireNamespace("poorman", quietly = TRUE)) {
  test_that("data_tabulate grouped data.frame", {
    x <- data_tabulate(poorman::group_by(efc, e16sex), "c172code")
    expect_s3_class(x, "list")
    expect_equal(length(x), 2)
    expect_equal(
      attributes(x[[1]]),
      list(
        names = c(
          "Variable",
          "Group",
          "Value",
          "N",
          "Raw %",
          "Valid %",
          "Cumulative %"
        ),
        class = c("dw_data_tabulate", "data.frame"),
        row.names = 1:4,
        type = "numeric",
        varname = "c172code",
        label = "carer's level of education",
        object = "c172code",
        group_variable = structure(
          list(e16sex = 1),
          .drop = TRUE,
          row.names = 1L,
          class = "data.frame"
        ),
        duplicate_varnames = c(FALSE, TRUE, TRUE, TRUE),
        total_n = 46L,
        valid_n = 41L
      )
    )
    table1 <- x[[1]]
    expect_equal(as.vector(table1$Value), as.character(c(sort(
      unique(efc$c172code)
    ), NA)))
    expect_equal(table1$N, as.vector(table(addNA(efc$c172code[efc$e16sex == 1]))))
    expect_equal(table1$`Valid %`,
      as.vector(c(
        100 * table(efc$c172code[efc$e16sex == 1]) / sum(!is.na(efc$c172code[efc$e16sex == 1])), NA
      )),
      ignore_attr = TRUE,
      tolerance = 1e-3
    )
  })


  test_that("data_tabulate print", {
    x <- data_tabulate(poorman::group_by(efc, e16sex), "c172code")
    out <- capture.output(print(x))
    expect_equal(
      out,
      c(
        "carer's level of education (c172code) <numeric>",
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
        "carer's level of education (c172code) <numeric>",
        "Grouped by e16sex (2)",
        "# total N=54 valid N=49",
        "",
        "Value |  N | Raw % | Valid % | Cumulative %",
        "------+----+-------+---------+-------------",
        "1     |  3 |  5.56 |    6.12 |         6.12",
        "2     | 34 | 62.96 |   69.39 |        75.51",
        "3     | 12 | 22.22 |   24.49 |       100.00",
        "<NA>  |  5 |  9.26 |    <NA> |         <NA>"
      )
    )
  })

  test_that("data_tabulate print, collapse groups", {
    x <-
      data_tabulate(poorman::group_by(efc, e16sex), "c172code", collapse = TRUE)
    out <- capture.output(print(x))
    expect_equal(
      out,
      c(
        "# Frequency Table",
        "",
        "Variable |      Group | Value |  N | Raw % | Valid % | Cumulative %",
        "---------+------------+-------+----+-------+---------+-------------",
        "c172code | e16sex (1) |     1 |  5 | 10.87 |   12.20 |        12.20",
        "         |            |     2 | 32 | 69.57 |   78.05 |        90.24",
        "         |            |     3 |  4 |  8.70 |    9.76 |       100.00",
        "         |            |  <NA> |  5 | 10.87 |    <NA> |         <NA>",
        "---------+------------+-------+----+-------+---------+-------------",
        "c172code | e16sex (2) |     1 |  3 |  5.56 |    6.12 |         6.12",
        "         |            |     2 | 34 | 62.96 |   69.39 |        75.51",
        "         |            |     3 | 12 | 22.22 |   24.49 |       100.00",
        "         |            |  <NA> |  5 |  9.26 |    <NA> |         <NA>",
        "-------------------------------------------------------------------"
      )
    )
  })

  test_that("data_tabulate print, collapse groups, dropl evels", {
    x <-
      data_tabulate(
        poorman::group_by(efc, e16sex),
        "e42dep",
        collapse = TRUE,
        drop_levels = TRUE
      )
    out <- capture.output(print(x))
    expect_equal(
      out,
      c(
        "# Frequency Table",
        "",
        "Variable |      Group | Value |  N | Raw % | Valid % | Cumulative %",
        "---------+------------+-------+----+-------+---------+-------------",
        "e42dep   | e16sex (1) |     1 |  2 |  4.35 |    4.44 |         4.44",
        "         |            |     2 |  2 |  4.35 |    4.44 |         8.89",
        "         |            |     3 |  8 | 17.39 |   17.78 |        26.67",
        "         |            |     4 | 33 | 71.74 |   73.33 |       100.00",
        "         |            |  <NA> |  1 |  2.17 |    <NA> |         <NA>",
        "---------+------------+-------+----+-------+---------+-------------",
        "e42dep   | e16sex (2) |     2 |  2 |  3.70 |    3.85 |         3.85",
        "         |            |     3 | 20 | 37.04 |   38.46 |        42.31",
        "         |            |     4 | 30 | 55.56 |   57.69 |       100.00",
        "         |            |  <NA> |  2 |  3.70 |    <NA> |         <NA>",
        "-------------------------------------------------------------------"
      )
    )
  })
}

# select helpers ------------------------------
test_that("data_tabulate regex", {
  expect_equal(
    data_tabulate(mtcars, select = "arb", regex = TRUE),
    data_tabulate(mtcars, select = "carb")
  )
})
