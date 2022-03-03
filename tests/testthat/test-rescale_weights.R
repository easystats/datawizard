test_that("rescale_weights works as expected", {
  data(nhanes_sample)

  expect_snapshot(head(rescale_weights(nhanes_sample, "SDMVSTRA", "WTINT2YR")))

  expect_snapshot(head(rescale_weights(nhanes_sample, c("SDMVSTRA", "SDMVPSU"), "WTINT2YR")))
})
