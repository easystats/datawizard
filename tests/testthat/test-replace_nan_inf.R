test_that("extract from data frame", {
  x <- c(1, 2, NA, 3, NaN, 4, NA, 5, Inf, -Inf, 6, 7)

  expect_equal(
    replace_nan_inf(x),
    c(1, 2, NA, 3, NA, 4, NA, 5, NA, NA, 6, 7)
  )

  # a data frame
  df <- data.frame(
    x = c(1, NA, 5, Inf, 2, NA),
    y = c(3, NaN, 4, -Inf, 6, 7),
    stringsAsFactors = FALSE
  )

  expect_equal(
    replace_nan_inf(df),
    structure(
      list(
        x = c(1, NA, 5, NA, 2, NA),
        y = c(3, NA, 4, NA, 6, 7)
      ),
      row.names = c(NA, -6L),
      class = "data.frame"
    )
  )

  expect_equal(
    replace_nan_inf(df, select = starts_with("x")),
    structure(
      list(
        x = c(1, NA, 5, NA, 2, NA),
        y = c(3, NaN, 4, -Inf, 6, 7)
      ),
      row.names = c(NA, -6L),
      class = "data.frame"
    )
  )
})
