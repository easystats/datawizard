data(efc, package = "datawizard")

test_that("data_reverse, labels preserved", {
  expect_equal(
    attr(data_reverse(efc$e42dep), "label", exact = TRUE),
    "elder's dependency"
  )

  expect_equal(
    names(attr(data_reverse(efc$e42dep), "labels", exact = TRUE)),
    names(attr(efc$e42dep, "labels", exact = TRUE))
  )

  expect_equal(
    attr(data_reverse(efc$e42dep), "labels", exact = TRUE),
    rev(attr(efc$e42dep, "labels", exact = TRUE)),
    ignore_attr = TRUE
  )

  expect_equal(
    names(attr(data_reverse(efc$c12hour), "labels", exact = TRUE)),
    names(attr(efc$c12hour, "labels", exact = TRUE))
  )

  labels <- sapply(data_reverse(efc), function(i) attr(i, "label", exact = TRUE))
  expect_equal(
    labels,
    c(c12hour = "average number of hours of care per week",
      e16sex = "elder's gender",
      e42dep = "elder's dependency",
      c172code = "carer's level of education",
      neg_c_7 = "Negative impact with 7 items")
  )
})



test_that("data_merge, labels preserved", {
  labels <- sapply(data_merge(efc[c(1:2)], efc[c(3:4)]), function(i) attr(i, "label", exact = TRUE))
  expect_equal(
    labels,
    c(c12hour = "average number of hours of care per week",
      e16sex = "elder's gender",
      e42dep = "elder's dependency",
      c172code = "carer's level of education"
    )
  )
})



test_that("data_extract, labels preserved", {
  expect_equal(
    attr(data_extract(efc, select = "e42dep"), "labels", exact = TRUE),
    attr(efc$e42dep, "labels", exact = TRUE),
    ignore_attr = TRUE
  )

  labels <- sapply(data_extract(efc, select = c("e42dep", "c172code")), function(i) attr(i, "label", exact = TRUE))
  expect_equal(labels, c("elder's dependency", "carer's level of education"))
})



test_that("data_cut, labels preserved", {
  expect_equal(
    attr(data_cut(efc$c12hour), "label", exact = TRUE),
    attr(efc$c12hour, "label", exact = TRUE),
    ignore_attr = TRUE
  )

  expect_equal(
    attr(data_cut(efc$e42dep), "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE),
    ignore_attr = TRUE
  )
})
