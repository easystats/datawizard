test_that("reshape_ci with single CI level", {
  df <- data.frame(
    Parameter = c("Term 1"),
    CI = c(.8),
    CI_low = c(.2),
    CI_high = c(.5),
    stringsAsFactors = FALSE
  )

  df_reshape <- reshape_ci(df)

  expect_snapshot(df_reshape)
})


test_that("reshape_ci with multiple CI levels", {
  x <- data.frame(
    Parameter = c("Term 1", "Term 2", "Term 1", "Term 2"),
    CI = c(.8, .8, .9, .9),
    CI_low = c(.2, .3, .1, .15),
    CI_high = c(.5, .6, .8, .85),
    stringsAsFactors = FALSE
  )

  expect_snapshot(reshape_ci(x))
  expect_snapshot(reshape_ci(reshape_ci(x)))
})
