test_that("describe_missing", {
  expect_snapshot(describe_missing(airquality))

  # Use selected columns explicitly
  expect_snapshot(describe_missing(airquality,
    vars = list(
      c("Ozone", "Solar.R", "Wind"),
      c("Temp", "Month", "Day")
    )
  ))

  # If the questionnaire items start with the same name, e.g.,
  set.seed(15)
  fun <- function() {
    c(sample(c(NA, 1:10), replace = TRUE), NA, NA, NA)
  }
  expect_snapshot(df <- data.frame(
    ID = c("idz", NA),
    scale1_Q1 = fun(), scale1_Q2 = fun(), scale1_Q3 = fun(),
    scale2_Q1 = fun(), scale2_Q2 = fun(), scale2_Q3 = fun(),
    scale3_Q1 = fun(), scale3_Q2 = fun(), scale3_Q3 = fun()
  ))

  # One can list the scale names directly:
  expect_snapshot(describe_missing(df, scales = c("ID", "scale1", "scale2", "scale3")))
})
