test_that("reshape_ci with single CI level", {
  df <- data.frame(
    Parameter = c("Term 1"),
    CI = c(0.8),
    CI_low = c(0.2),
    CI_high = c(0.5),
    stringsAsFactors = FALSE
  )

  df_reshape <- reshape_ci(df)

  expect_snapshot(df_reshape)
})


test_that("reshape_ci with multiple CI levels", {
  x <- data.frame(
    Parameter = c("Term 1", "Term 2", "Term 1", "Term 2"),
    CI = c(0.8, 0.8, 0.9, 0.9),
    CI_low = c(0.2, 0.3, 0.1, 0.15),
    CI_high = c(0.5, 0.6, 0.8, 0.85),
    stringsAsFactors = FALSE
  )

  expect_snapshot(reshape_ci(x))
  expect_snapshot(reshape_ci(reshape_ci(x)))
})
