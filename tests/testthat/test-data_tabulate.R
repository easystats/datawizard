data(efc)

test_that("data_tabulate factor", {
  x <- data_tabulate(efc$e42dep)
  expect_identical(as.vector(x$Value), as.vector(sort(unique(
    addNA(efc$e42dep)
  ))))
  expect_identical(x$N, as.vector(table(addNA(efc$e42dep))))
  expect_identical(x$`Valid %`,
    as.vector(c(
      100 * table(efc$e42dep) / sum(!is.na(efc$e42dep)), NA
    )),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_tabulate numeric", {
  x <- data_tabulate(efc$neg_c_7)
  expect_identical(as.vector(x$Value), as.vector(sort(unique(
    addNA(efc$neg_c_7)
  ))))
  expect_identical(x$N, as.vector(table(addNA(efc$neg_c_7))))
  expect_identical(x$`Valid %`,
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
  expect_length(x, 2L)
  expect_identical(
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
  expect_identical(
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
  expect_identical(as.vector(table1$Value), as.character(c(sort(
    unique(efc$e16sex)
  ), NA)))
  expect_identical(table1$N, as.vector(table(addNA(efc$e16sex))))
  expect_identical(table1$`Valid %`,
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
  expect_identical(
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
  expect_snapshot(data_tabulate(efc$e42dep))
})


test_that("data_tabulate print multiple", {
  expect_snapshot(data_tabulate(efc, c("c172code", "e16sex")))
})


test_that("data_tabulate big numbers", {
  set.seed(123)
  x <- sample(1:5, size = 1e7, TRUE)
  expect_snapshot(data_tabulate(x))
  expect_snapshot(print(data_tabulate(x), big_mark = "-"))
})


test_that("data_tabulate print multiple, collapse", {
  skip_if_not(packageVersion("insight") > "0.17.0", "insight must be >= 0.17.0")
  expect_snapshot(data_tabulate(efc, c("c172code", "e16sex"), collapse = TRUE))
})



test_that("data_tabulate grouped data.frame", {
  skip_if_not_installed("poorman")
  x <- data_tabulate(poorman::group_by(efc, e16sex), "c172code")
  expect_s3_class(x, "list")
  expect_length(x, 2L)
  expect_identical(
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
  expect_identical(as.vector(table1$Value), as.character(c(sort(
    unique(efc$c172code)
  ), NA)))
  expect_identical(table1$N, as.vector(table(addNA(efc$c172code[efc$e16sex == 1]))))
  expect_identical(table1$`Valid %`,
    as.vector(c(
      100 * table(efc$c172code[efc$e16sex == 1]) / sum(!is.na(efc$c172code[efc$e16sex == 1])), NA
    )),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_tabulate print grouped data", {
  skip_if_not_installed("poorman")
  expect_snapshot(data_tabulate(poorman::group_by(efc, e16sex), "c172code"))
})

test_that("data_tabulate print, collapse groups", {
  skip_if_not_installed("poorman")
  expect_snapshot(
    data_tabulate(poorman::group_by(efc, e16sex), "c172code", collapse = TRUE)
  )
})

test_that("data_tabulate print, collapse groups, drop levels", {
  skip_if_not_installed("poorman")
  expect_snapshot(
    data_tabulate(
      poorman::group_by(efc, e16sex),
      "e42dep",
      collapse = TRUE,
      drop_levels = TRUE
    )
  )
})


# select helpers ------------------------------
test_that("data_tabulate regex", {
  expect_identical(
    data_tabulate(mtcars, select = "arb", regex = TRUE),
    data_tabulate(mtcars, select = "carb")
  )
})
