test_that("convert dataframe to numeric", {
  expect_snapshot(convert_data_to_numeric(head(ToothGrowth)))
  expect_snapshot(convert_data_to_numeric(head(ToothGrowth), dummy_factors = FALSE))
})

test_that("convert character to numeric", {
  expect_equal(convert_data_to_numeric(c("xyz", "ab")), c(2, 1))
})

test_that("convert factor to numeric", {
  f <- factor(substring("statistics", 1:10, 1:10))
  expect_snapshot(convert_data_to_numeric(f))
})


test_that("convert factor to numeric", {
  expect_equal(convert_data_to_numeric(c("abc", "xyz")), c(1, 2))
  expect_equal(convert_data_to_numeric(c("123", "789")), c(123, 789))
  expect_equal(convert_data_to_numeric(c("1L", "2e-3")), c(1, 0.002))
  expect_equal(convert_data_to_numeric(c("1L", "2e-3", "ABC")), c(1, 2, 3))
})


test_that("convert factor to numeric, dummy factors", {
  expect_equal(
    convert_data_to_numeric(c("abc", "xyz"), dummy_factors = TRUE),
    data.frame(abc = c(1, 0), xyz = c(0, 1)),
    ignore_attr = TRUE
  )
  expect_equal(
    convert_data_to_numeric(c("1L", "2e-3", "ABC"), dummy_factors = TRUE),
    data.frame(`1L` = c(1, 0, 0), `2e-3` = c(0, 1, 0), ABC = c(0, 0, 1)),
    ignore_attr = TRUE
  )
})
