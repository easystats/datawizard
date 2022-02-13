test_that("convert dataframe to numeric", {
  expect_snapshot(convert_data_to_numeric(head(ToothGrowth)))
  expect_snapshot(convert_data_to_numeric(head(ToothGrowth), dummy_factors = FALSE))
})

test_that("convert character to numeric", {
  expect_equal(convert_data_to_numeric(c("xyz", "ab")), c(1, 1))
  # convert_data_to_numeric(c("123", "24x"))
})

test_that("convert factor to numeric", {
  f <- factor(substring("statistics", 1:10, 1:10))
  expect_snapshot(convert_data_to_numeric(f))
})
