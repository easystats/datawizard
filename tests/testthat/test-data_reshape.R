set.seed(123)
wide_data <- data.frame(replicate(3, rnorm(5)))

test_that("data_reshape works as expected - wide to long", {
  expect_snapshot(data_to_long(wide_data))

  expect_snapshot(data_to_long(wide_data,
    cols = c(1, 2),
    colnames_to = "Column",
    values_to = "Numbers",
    rows_to = "Row"
  ))
})

test_that("data_reshape works as expected - long to wide", {
  long_data <- data_to_long(wide_data, rows_to = "Row_ID")

  expect_snapshot(data_to_wide(long_data,
    colnames_from = "Name",
    values_from = "Value",
    rows_from = "Row_ID"
  ))
})

test_that("data_reshape works as expected - complex dataset", {
  skip_if_not_installed("psych")

  data <- psych::bfi

  long <- data_to_long(data,
    cols = "\\d",
    colnames_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  expect_snapshot(str(long))

  long$Facet <- gsub("\\d", "", long$Item)
  long$Item <- gsub("[A-Z]", "", long$Item)
  long$Item <- paste0("I", long$Item)

  wide <- data_to_wide(long,
    colnames_from = "Item",
    values_from = "Score"
  )

  expect_snapshot(str(wide))
})
