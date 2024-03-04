test_that("value_at", {
  data(efc, package = "datawizard")
  expect_equal(value_at(efc$e42dep, 5), 4, ignore_attr = TRUE)
  expect_equal(value_at(efc$e42dep, 4), NA, ignore_attr = TRUE)
  expect_equal(value_at(efc$e42dep, 4, remove_na = TRUE), 4, ignore_attr = TRUE)
  expect_equal(value_at(efc$c12hour, 5:7), efc$c12hour[5:7], ignore_attr = TRUE)
  expect_equal(value_at(efc$e42dep, 123456, default = 55), 55, ignore_attr = TRUE)
  expect_null(value_at(efc$e42dep, 123456))
})
