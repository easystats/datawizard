test_that("describe_missing", {
  airquality2 <- cbind(airquality[2:6], airquality[1])

  expect_snapshot(describe_missing(airquality2))

  expect_snapshot(describe_missing(airquality2, sort = TRUE))

  expect_snapshot(describe_missing(
    airquality2,
    select = "Ozone:Temp"
  ))

  expect_snapshot(describe_missing(
    airquality2,
    exclude = "Ozone:Temp"
  ))

  # Testing the 'by' argument for survey scales
  set.seed(15)
  fun <- function() {
    c(sample(c(NA, 1:10), replace = TRUE), NA, NA, NA)
  }
  df <- data.frame(
    ID = c("idz", NA),
    openness_1 = fun(), openness_2 = fun(), openness_3 = fun(),
    extroversion_1 = fun(), extroversion_2 = fun(), extroversion_3 = fun(),
    agreeableness_1 = fun(), agreeableness_2 = fun(), agreeableness_3 = fun(),
    stringsAsFactors = FALSE
  )

  # Pivot and group using datawizard
  df_long <- reshape_longer(df,
    select = -1,
    names_sep = "_",
    names_to = c("dimension", "item")
  )

  # Run describe_missing with 'by' argument
  expect_snapshot(describe_missing(
    df_long,
    select = -c(1, 3), by = "dimension"
  ))
})
