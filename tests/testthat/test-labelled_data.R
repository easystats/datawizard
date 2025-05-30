data(efc, package = "datawizard")

# reverse -----------------------------------

test_that("reverse, labels preserved", {
  # factor, label
  expect_identical(
    attr(reverse(efc$e42dep), "label", exact = TRUE),
    "elder's dependency"
  )
  # factor, labels
  expect_named(
    attr(reverse(efc$e42dep), "labels", exact = TRUE),
    names(attr(efc$e42dep, "labels", exact = TRUE))
  )
  expect_equal(
    attr(reverse(efc$e42dep), "labels", exact = TRUE),
    rev(attr(efc$e42dep, "labels", exact = TRUE)),
    ignore_attr = TRUE
  )
  # numeric
  expect_named(
    attr(reverse(efc$c12hour), "labels", exact = TRUE),
    names(attr(efc$c12hour, "labels", exact = TRUE))
  )
  # data frame
  labels <- sapply(reverse(efc), attr, which = "label", exact = TRUE)
  expect_identical(
    labels,
    c(
      c12hour = "average number of hours of care per week",
      e16sex = "elder's gender",
      e42dep = "elder's dependency",
      c172code = "carer's level of education",
      neg_c_7 = "Negative impact with 7 items"
    )
  )
})


# data_merge -----------------------------------

test_that("data_merge, labels preserved", {
  labels <- sapply(data_merge(efc[1:2], efc[3:4], verbose = FALSE), attr, which = "label", exact = TRUE)
  expect_identical(
    labels,
    c(
      c12hour = "average number of hours of care per week",
      e16sex = "elder's gender",
      e42dep = "elder's dependency",
      c172code = "carer's level of education"
    )
  )
})


# data_extract -----------------------------------

test_that("data_extract, labels preserved", {
  # factor
  expect_equal(
    attr(data_extract(efc, select = "e42dep"), "labels", exact = TRUE),
    attr(efc$e42dep, "labels", exact = TRUE),
    ignore_attr = TRUE
  )
  # numeric
  expect_equal(
    attr(data_extract(efc, select = "c172code"), "labels", exact = TRUE),
    attr(efc$c172code, "labels", exact = TRUE),
    ignore_attr = TRUE
  )
  # data frame
  labels <- sapply(data_extract(efc, select = c("e42dep", "c172code")), attr, which = "label", exact = TRUE)
  expect_identical(
    labels,
    c(e42dep = "elder's dependency", c172code = "carer's level of education")
  )
})


# categorize -----------------------------------

test_that("categorize, labels preserved", {
  # factor
  expect_equal(
    attr(categorize(efc$e42dep), "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # numeric
  expect_equal(
    attr(categorize(efc$c12hour), "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE),
    ignore_attr = TRUE
  )
})


# data_reorder -----------------------------------

test_that("data_reorder, labels preserved", {
  expect_equal(
    attr(data_reorder(efc, "e42dep")[[1]], "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
})


# data_remove -----------------------------------

test_that("data_remove, labels preserved", {
  expect_equal(
    attr(data_remove(efc, "e42dep")[[1]], "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE),
    ignore_attr = TRUE
  )
})


# data_rename -----------------------------------

test_that("data_rename, labels preserved", {
  # factor
  x <- data_rename(efc, "e42dep", "dependency")
  expect_equal(
    attr(x$dependency, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # numeric
  x <- data_rename(efc, "c12hour", "careload")
  expect_equal(
    attr(x$careload, "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # data frame
  labels <- sapply(data_remove(efc, starts_with("c1")), attr, which = "label", exact = TRUE)
  expect_identical(
    labels,
    c(e16sex = "elder's gender", e42dep = "elder's dependency", neg_c_7 = "Negative impact with 7 items")
  )
})


# data_addprefix -----------------------------------

test_that("data_addprefix, labels preserved", {
  x <- data_addprefix(efc, "new_")
  # factor
  expect_equal(
    attr(x$new_e42dep, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # numeric
  expect_equal(
    attr(x$new_c12hour, "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE),
    ignore_attr = TRUE
  )
})


# data_suffix -----------------------------------

test_that("data_addsuffix, labels preserved", {
  x <- data_addsuffix(efc, "_new")
  # factor
  expect_equal(
    attr(x$e42dep_new, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # numeric
  expect_equal(
    attr(x$c12hour_new, "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE),
    ignore_attr = TRUE
  )
})


# to_numeric -----------------------------------

test_that("to_numeric, labels preserved", {
  x <- to_numeric(efc, dummy_factors = FALSE)
  # factor
  expect_equal(
    attr(x$e42dep, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # numeric
  expect_equal(
    attr(x$c12hour, "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE),
    ignore_attr = TRUE
  )

  x <- to_numeric(efc, dummy_factors = TRUE)
  # numeric
  expect_equal(
    attr(x$c12hour, "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE),
    ignore_attr = TRUE
  )
})


# data_match -----------------------------------

test_that("data_match, labels preserved", {
  x <- data_match(efc, data.frame(c172code = 1, e16sex = 2), match = "or")
  # factor
  expect_equal(
    attr(x$e42dep, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # numeric
  expect_equal(
    attr(x$c12hour, "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # filtered
  expect_equal(
    attr(x$c172code, "label", exact = TRUE),
    attr(efc$c172code, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  expect_equal(
    attr(x$c172code, "labels", exact = TRUE),
    attr(efc$c172code, "labels", exact = TRUE),
    ignore_attr = TRUE
  )
})


# data_filter -----------------------------------

test_that("data_filter, labels preserved", {
  x <- data_filter(efc, c172code == 1 & c12hour > 40)
  # factor
  expect_identical(
    attr(x$e42dep, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE)
  )
  # numeric
  expect_identical(
    attr(x$c12hour, "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE)
  )
})


# convert_to_na -----------------------------------

test_that("convert_to_na, labels preserved", {
  expect_message({
    x <- convert_to_na(efc, na = c(2, "2"), select = starts_with("e"))
  })
  # factor
  expect_equal(
    attr(x$e42dep, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # numeric
  expect_equal(
    attr(x$e16sex, "label", exact = TRUE),
    attr(efc$e16sex, "label", exact = TRUE),
    ignore_attr = TRUE
  )

  # factor
  x <- convert_to_na(efc$e42dep, na = "2")
  expect_equal(
    attr(x, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # numeric
  x <- convert_to_na(efc$e16sex, na = 2)
  expect_equal(
    attr(x, "label", exact = TRUE),
    attr(efc$e16sex, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  # drop unused value labels
  x <- convert_to_na(efc$c172code, na = 2)
  expect_identical(
    attr(x, "labels", exact = TRUE),
    c(`low level of education` = 1, `high level of education` = 3)
  )
})


# data_select -----------------------------------

test_that("data_select, labels preserved", {
  x <- data_select(efc, starts_with("c"))
  # numeric
  expect_equal(
    attr(x$c12hour, "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE),
    ignore_attr = TRUE
  )

  x <- data_select(efc, starts_with("e"))
  # factor
  expect_equal(
    attr(x$e42dep, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
})


# recode_values -----------------------------------

test_that("recode_values, labels preserved", {
  options(data_recode_pattern = NULL)
  data(efc)
  x <- recode_values(efc$c172code, recode = list(`0` = 1:2, `1` = 3))
  expect_equal(
    attr(x, "label", exact = TRUE),
    attr(efc$c172code, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  expect_null(attr(x, "labels", exact = TRUE))
})


# slide -----------------------------------

test_that("slide, labels preserved", {
  data(efc)
  suppressMessages({
    x <- slide(efc)
  })
  expect_equal(
    attr(x$c172code, "label", exact = TRUE),
    attr(efc$c172code, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  expect_null(attr(x$c172code, "labels", exact = TRUE))

  x <- slide(efc$c172code)
  expect_equal(
    attr(x, "label", exact = TRUE),
    attr(efc$c172code, "label", exact = TRUE),
    ignore_attr = TRUE
  )
})


# to_factor -----------------------------------

test_that("to_factor, labels preserved", {
  data(efc)
  x <- to_factor(efc)
  expect_equal(
    attr(x$c172code, "label", exact = TRUE),
    attr(efc$c172code, "label", exact = TRUE),
    ignore_attr = TRUE
  )
  expect_null(attr(x$c172code, "labels", exact = TRUE))

  x <- to_factor(efc$c172code)
  expect_equal(
    attr(x, "label", exact = TRUE),
    attr(efc$c172code, "label", exact = TRUE),
    ignore_attr = TRUE
  )
})
