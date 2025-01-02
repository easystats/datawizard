test_that("data_tabulate factor", {
  data(efc, package = "datawizard")
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
  data(efc, package = "datawizard")
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


test_that("data_tabulate, HTML", {
  skip_if_not_installed("gt")
  data(efc, package = "datawizard")
  expect_s3_class(print_html(data_tabulate(efc$c172code)), "gt_tbl")
  expect_s3_class(print_html(data_tabulate(efc, "c172code")), "gt_tbl")
})


test_that("data_tabulate, weights", {
  data(efc, package = "datawizard")
  set.seed(123)
  efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
  # vector/factor
  out1 <- data_tabulate(efc$e42dep, weights = efc$weights)
  out2 <- data_tabulate(efc$e42dep)
  expect_equal(out1$N, c(3, 4, 26, 67, 5), ignore_attr = TRUE)
  expect_equal(out2$N, c(2L, 4L, 28L, 63L, 3L), ignore_attr = TRUE)
  expect_equal(
    out1$N,
    round(xtabs(efc$weights ~ efc$e42dep, addNA = TRUE)),
    ignore_attr = TRUE
  )
  # data frames
  out <- data_tabulate(efc, c("e42dep", "e16sex"), weights = efc$weights)
  expect_equal(out[[1]]$N, out1$N, ignore_attr = TRUE)
  # mismatch of lengths
  w <- c(efc$weights, 1)
  expect_error(data_tabulate(efc$e42dep, weights = w), regex = "Length of `weights`")
  # correct table footer
  expect_snapshot(print(data_tabulate(efc$e42dep, weights = efc$weights)))
  expect_snapshot(print_md(data_tabulate(efc$e42dep, weights = efc$weights)))
  # correct table caption
  expect_snapshot(print(data_tabulate(efc, c("e42dep", "e16sex"), collapse = TRUE, weights = efc$weights)))
  expect_snapshot(print_md(data_tabulate(efc, c("e42dep", "e16sex"), weights = efc$weights)))
})


test_that("data_tabulate data.frame", {
  data(efc, package = "datawizard")
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
      class = c("datawizard_table", "data.frame"),
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
      class = c("datawizard_table", "data.frame"),
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


test_that("data_tabulate unsupported class", {
  data(mtcars)
  expect_warning(data_tabulate(lm(mpg ~ hp, data = mtcars)), regex = "Can't compute frequency tables")
})


test_that("data_tabulate print", {
  set.seed(123)
  x <- sample.int(3, 1e6, TRUE)
  out <- data_tabulate(x, name = "Large Number")
  expect_identical(
    attributes(out),
    list(
      names = c("Variable", "Value", "N", "Raw %", "Valid %", "Cumulative %"),
      class = c("datawizard_table", "data.frame"),
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
  data(efc, package = "datawizard")
  expect_snapshot(data_tabulate(efc$e42dep))
})


test_that("data_tabulate print multiple", {
  data(efc, package = "datawizard")
  expect_snapshot(data_tabulate(efc, c("c172code", "e16sex")))
})


test_that("data_tabulate big numbers", {
  set.seed(123)
  x <- sample.int(5, size = 1e7, TRUE)
  expect_snapshot(data_tabulate(x))
  expect_snapshot(print(data_tabulate(x), big_mark = "-"))
})


test_that("data_tabulate print multiple, collapse", {
  data(efc, package = "datawizard")
  expect_snapshot(data_tabulate(efc, c("c172code", "e16sex"), collapse = TRUE))
})


test_that("data_tabulate grouped data.frame", {
  skip_if_not_installed("poorman")
  data(efc, package = "datawizard")
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
      class = c("datawizard_table", "data.frame"),
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
  data(efc, package = "datawizard")
  expect_snapshot(data_tabulate(poorman::group_by(efc, e16sex), "c172code"))
})

test_that("data_tabulate print, collapse groups", {
  skip_if_not_installed("poorman")
  data(efc, package = "datawizard")
  expect_snapshot(
    data_tabulate(poorman::group_by(efc, e16sex), "c172code", collapse = TRUE)
  )
})

test_that("data_tabulate print, collapse groups, drop levels", {
  skip_if_not_installed("poorman")
  data(efc, package = "datawizard")
  expect_snapshot(
    data_tabulate(
      poorman::group_by(efc, e16sex),
      "e42dep",
      collapse = TRUE,
      drop_levels = TRUE
    )
  )
})

test_that("data_tabulate drop levels", {
  x <- factor(rep(letters[1:3], 3), levels = letters[1:5])
  out1 <- data_tabulate(x, drop_levels = FALSE)
  out2 <- data_tabulate(x, drop_levels = TRUE)
  expect_identical(out1$N, c(3L, 3L, 3L, 0L, 0L, 0L))
  expect_identical(as.character(out1$Value), c("a", "b", "c", "d", "e", NA))
  expect_identical(out2$N, c(3L, 3L, 3L, 0L))
  expect_identical(as.character(out2$Value), c("a", "b", "c", NA))
})


# select helpers ------------------------------

test_that("data_tabulate regex", {
  data(mtcars)
  expect_identical(
    data_tabulate(mtcars, select = "arb", regex = TRUE),
    data_tabulate(mtcars, select = "carb")
  )
})


# missing values ------------------------------

test_that("data_tabulate exclude/include missing values", {
  data(efc, package = "datawizard")
  set.seed(123)
  efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
  efc$e16sex[sample.int(nrow(efc), 5)] <- NA
  out <- data_tabulate(efc$c172code)
  expect_identical(out$N, c(8L, 66L, 16L, 10L))
  out <- data_tabulate(efc$c172code, remove_na = TRUE)
  expect_identical(out$N, c(8L, 66L, 16L))
  out <- data_tabulate(efc$c172code, weights = efc$weights)
  expect_identical(out$N, c(10, 67, 15, 13))
  out <- data_tabulate(efc$c172code, remove_na = TRUE, weights = efc$weights)
  expect_identical(out$N, c(10, 67, 15))
})


# cross tables ------------------------------

test_that("data_tabulate, cross tables", {
  data(efc, package = "datawizard")
  set.seed(123)
  efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
  efc$e16sex[sample.int(nrow(efc), 5)] <- NA

  expect_snapshot(print(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full")))
  expect_snapshot(print(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full", remove_na = TRUE)))
  expect_snapshot(print(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full", weights = efc$weights)))
  expect_snapshot(print(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full", remove_na = TRUE, weights = efc$weights))) # nolint
  expect_snapshot(print(data_tabulate(efc, "c172code", by = efc$e16sex, proportions = "row")))
  expect_snapshot(print(data_tabulate(efc, "c172code", by = efc$e16sex, proportions = "row", remove_na = TRUE)))
  expect_snapshot(print(data_tabulate(efc, "c172code", by = efc$e16sex, proportions = "row", weights = efc$weights)))
  expect_snapshot(print(data_tabulate(efc, "c172code", by = efc$e16sex, proportions = "row", remove_na = TRUE, weights = efc$weights))) # nolint
  expect_snapshot(print(data_tabulate(efc, "c172code", by = "e16sex", proportions = "column")))
  expect_snapshot(print(data_tabulate(efc, "c172code", by = "e16sex", proportions = "column", remove_na = TRUE)))
  expect_snapshot(print(data_tabulate(efc, "c172code", by = "e16sex", proportions = "column", weights = "weights")))
  expect_snapshot(print(data_tabulate(efc, "c172code", by = "e16sex", proportions = "column", remove_na = TRUE, weights = "weights"))) # nolint
  expect_snapshot(print(data_tabulate(efc, c("c172code", "e42dep"), by = "e16sex", proportions = "row"))) # nolint
})

test_that("data_tabulate, cross tables, HTML", {
  skip_if_not_installed("gt")
  data(efc, package = "datawizard")
  set.seed(123)
  efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
  efc$e16sex[sample.int(nrow(efc), 5)] <- NA

  expect_s3_class(print_html(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full")), "gt_tbl")
  expect_s3_class(print_html(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full", remove_na = TRUE)), "gt_tbl") # nolint
  expect_s3_class(print_html(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full", weights = efc$weights)), "gt_tbl") # nolint
  expect_s3_class(print_html(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full", remove_na = TRUE, weights = efc$weights)), "gt_tbl") # nolint
  expect_s3_class(print_html(data_tabulate(efc, "c172code", by = efc$e16sex, proportions = "row")), "gt_tbl")
  expect_s3_class(print_html(data_tabulate(efc, "c172code", by = efc$e16sex, proportions = "row", remove_na = TRUE, weights = efc$weights)), "gt_tbl") # nolint
})

test_that("data_tabulate, cross tables, grouped df", {
  data(efc, package = "datawizard")
  set.seed(123)
  efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
  efc$e16sex[sample.int(nrow(efc), 5)] <- NA
  grp <- data_group(efc, "e42dep")
  expect_snapshot(print(data_tabulate(grp, "c172code", by = "e16sex", proportions = "row")))
  skip_if_not_installed("gt")
  expect_s3_class(print_html(data_tabulate(grp, "c172code", by = "e16sex", proportions = "row")), "gt_tbl") # nolint
  expect_s3_class(print_html(data_tabulate(efc, c("e16sex", "e42dep"), by = "c172code", proportions = "row")), "gt_tbl") # nolint
})

test_that("data_tabulate, cross tables, errors by", {
  data(efc, package = "datawizard")
  set.seed(123)
  efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
  efc$e16sex[sample.int(nrow(efc), 5)] <- NA
  expect_error(data_tabulate(efc$c172code, by = "e16sex"), regex = "If `by` is a string")
  expect_error(data_tabulate(efc$c172code, by = efc$e16sex[-1]), regex = "Length of `by`")
  expect_error(data_tabulate(efc, "c172code", by = efc$e16sex[-1]), regex = "Length of `by`")
  expect_error(data_tabulate(efc, "c172code", by = "c16sex"), regex = "not found")
  expect_error(data_tabulate(efc, "c172code", by = c("e16sex", "e42dep")), regex = "You may use")
})

test_that("data_tabulate, cross tables, errors weights", {
  data(efc, package = "datawizard")
  set.seed(123)
  efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
  efc$e16sex[sample.int(nrow(efc), 5)] <- NA
  expect_error(data_tabulate(efc$c172code, weights = "weights"), regex = "If `weights`")
  expect_error(data_tabulate(efc$c172code, weights = efc$weights[-1]), regex = "Length of `weights`")
  expect_error(data_tabulate(efc, "c172code", weights = efc$weights[-1]), regex = "Length of `weights`")
  expect_error(data_tabulate(efc, "c172code", weights = "weigths"), regex = "not found")
  expect_error(data_tabulate(efc, "c172code", weights = c("e16sex", "e42dep")), regex = "length 1")
  expect_error(data_tabulate(efc$c172code, weights = efc$wweight), regex = "not found")
})


# markdown -------------------------

test_that("data_tabulate, cross tables, markdown", {
  data(efc, package = "datawizard")
  set.seed(123)
  efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
  efc$e16sex[sample.int(nrow(efc), 5)] <- NA

  expect_snapshot(print_md(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full")))
  expect_snapshot(print_md(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full", remove_na = TRUE)))
  expect_snapshot(print_md(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full", weights = efc$weights)))
  expect_snapshot(print_md(data_tabulate(efc$c172code, by = efc$e16sex, proportions = "full", remove_na = TRUE, weights = efc$weights))) # nolint
  expect_snapshot(print_md(data_tabulate(efc, "c172code", by = "e16sex", proportions = "column", remove_na = TRUE, weights = "weights"))) # nolint
  expect_snapshot(print_md(data_tabulate(efc, c("c172code", "e42dep"), by = "e16sex", proportions = "row"))) # nolint
})


# validate against table -------------------------

test_that("data_tabulate, validate against table", {
  data(mtcars)
  # frequency table
  out1 <- as.data.frame(table(mtcars$cyl))
  out2 <- data_tabulate(mtcars$cyl, remove_na = TRUE)
  expect_identical(out1$Freq, out2$N)
  # crosstable
  out1 <- data_arrange(as.data.frame(table(mtcars$cyl, mtcars$gear)), c("Var1", "Var2"))
  out2 <- data_rename(data_to_long(
    as.data.frame(data_tabulate(mtcars$cyl, by = mtcars$gear, remove_na = TRUE)), 2:4,
    names_to = "Var2", values_to = "Freq"
  ), "mtcars$cyl", "Var1")
  out1[[2]] <- as.character(out1[[2]])
  expect_equal(out1, out2, ignore_attr = TRUE)
})


test_that("data_tabulate, correct 0% for proportions", {
  data(efc, package = "datawizard")
  out <- data_tabulate(efc, "c172code", by = "e16sex", proportions = "column")
  expect_identical(format(out[[1]])[[4]], c("0 (0%)", "0 (0%)", "0 (0%)", "0 (0%)", "", "0"))
  expect_snapshot(print(out[[1]]))
})


# coercing to data frame -------------------------

test_that("data_tabulate, as.data.frame, frequency tables", {
  data(mtcars)
  # frequency table
  x <- data_tabulate(mtcars$cyl)
  out <- as.data.frame(x)
  expect_named(out, c("Variable", "Value", "N", "Raw %", "Valid %", "Cumulative %"))
  expect_identical(out$Variable, c("mtcars$cyl", "mtcars$cyl", "mtcars$cyl", "mtcars$cyl"))
  expect_false(any(vapply(out[2:ncol(out)], is.character, logical(1))))
  # frequency tables
  x <- data_tabulate(mtcars, select = c("cyl", "am"))
  out <- as.data.frame(x)
  expect_named(out, c("var", "table"))
  expect_equal(vapply(out, class, character(1)), c("character", "AsIs"), ignore_attr = TRUE)
  expect_length(out$table, 2L)
  expect_named(out$table[[1]], c("Variable", "Value", "N", "Raw %", "Valid %", "Cumulative %"))
  expect_identical(out$table[[1]]$Variable, c("cyl", "cyl", "cyl", "cyl"))
  expect_false(any(vapply(out$table[[1]][2:ncol(out$table[[1]])], is.character, logical(1))))
})


test_that("data_tabulate, as.data.frame, cross tables", {
  data(mtcars)
  # cross table
  x <- data_tabulate(mtcars, "cyl", by = "am")
  out <- as.data.frame(x)
  expect_named(out, c("var", "table"))
  expect_equal(vapply(out, class, character(1)), c("character", "AsIs"), ignore_attr = TRUE)
  expect_length(out$table, 1L)
  expect_named(out$table[[1]], c("cyl", "0", "1", "NA"))
  expect_identical(nrow(out$table[[1]]), 4L)
  # cross tables
  x <- data_tabulate(mtcars, c("cyl", "vs"), by = "am")
  out <- as.data.frame(x)
  expect_named(out, c("var", "table"))
  expect_equal(vapply(out, class, character(1)), c("character", "AsIs"), ignore_attr = TRUE)
  expect_length(out$table, 2L)
  expect_named(out$table[[1]], c("cyl", "0", "1", "NA"))
  expect_identical(nrow(out$table[[1]]), 4L)
})


test_that("data_tabulate, as.data.frame, cross tables with total N", {
  # cross table, with total
  x <- data_tabulate(mtcars, "cyl", by = "am")
  out <- as.data.frame(x, add_total = TRUE)
  expect_named(out, c("var", "table"))
  expect_equal(vapply(out, class, character(1)), c("character", "AsIs"), ignore_attr = TRUE)
  expect_length(out$table, 1L)
  expect_named(out$table[[1]], c("cyl", "0", "1", "<NA>", "Total"))
  expect_identical(nrow(out$table[[1]]), 5L)
  expect_identical(out$table[[1]]$cyl, c("4", "6", "8", NA, "Total"))
  # cross tables, with total
  x <- data_tabulate(mtcars, c("cyl", "vs"), by = "am")
  out <- as.data.frame(x, add_total = TRUE)
  expect_named(out, c("var", "table"))
  expect_equal(vapply(out, class, character(1)), c("character", "AsIs"), ignore_attr = TRUE)
  expect_length(out$table, 2L)
  expect_named(out$table[[1]], c("cyl", "0", "1", "<NA>", "Total"))
  expect_identical(nrow(out$table[[1]]), 5L)
  expect_identical(out$table[[1]]$cyl, c("4", "6", "8", NA, "Total"))
})
