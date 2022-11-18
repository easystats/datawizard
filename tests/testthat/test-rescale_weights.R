test_that("rescale_weights works as expected", {
  data(nhanes_sample)

  expect_snapshot(head(rescale_weights(nhanes_sample, "SDMVSTRA", "WTINT2YR")))

  expect_snapshot(head(rescale_weights(nhanes_sample, c("SDMVSTRA", "SDMVPSU"), "WTINT2YR")))
})


test_that("rescale_weights nested works as expected", {
  data(nhanes_sample)

  expect_snapshot(
    rescale_weights(
      data = head(nhanes_sample, n = 30),
      group = c("SDMVSTRA", "SDMVPSU"),
      probability_weights = "WTINT2YR",
      nest = TRUE
    )
  )

  expect_warning(
    x <- rescale_weights(
      data = head(nhanes_sample),
      group = "SDMVPSU",
      probability_weights = "WTINT2YR",
      nest = TRUE
    ),
    "Only one group variable selected"
  )

  expect_identical(
    x,
    rescale_weights(
      data = head(nhanes_sample),
      group = "SDMVPSU",
      probability_weights = "WTINT2YR"
    )
  )
})
