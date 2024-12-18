test_that("rescale_weights works as expected", {
  data(nhanes_sample)
  # convert tibble into data frame, so check-hard GHA works
  nhanes_sample <- as.data.frame(nhanes_sample)

  expect_snapshot(head(rescale_weights(nhanes_sample, "SDMVSTRA", "WTINT2YR")))

  expect_snapshot(head(rescale_weights(nhanes_sample, c("SDMVSTRA", "SDMVPSU"), "WTINT2YR")))

  expect_snapshot(head(rescale_weights(nhanes_sample, probability_weights = "WTINT2YR", method = "kish")))

  out <- rescale_weights(nhanes_sample, "SDMVSTRA", "WTINT2YR")
  expect_equal(sum(out$rescaled_weights_a), 2992, tolerance = 1e-3)
  expect_equal(sum(out$rescaled_weights_b), 2244.71451, tolerance = 1e-3)
  out <- rescale_weights(nhanes_sample, probability_weights = "WTINT2YR", method = "kish")
  expect_equal(sum(out$rescaled_weights), 2162.53961, tolerance = 1e-3)
})


test_that("rescale_weights nested works as expected", {
  data(nhanes_sample)
  # convert tibble into data frame, so check-hard GHA works
  nhanes_sample <- as.data.frame(nhanes_sample)

  expect_snapshot(
    rescale_weights(
      data = head(nhanes_sample, n = 30),
      by = c("SDMVSTRA", "SDMVPSU"),
      probability_weights = "WTINT2YR",
      nest = TRUE
    )
  )

  expect_warning(
    {
      x <- rescale_weights(
        data = head(nhanes_sample),
        by = "SDMVPSU",
        probability_weights = "WTINT2YR",
        nest = TRUE
      )
    },
    "Only one group variable selected"
  )

  expect_identical(
    x,
    rescale_weights(
      data = head(nhanes_sample),
      by = "SDMVPSU",
      probability_weights = "WTINT2YR"
    )
  )
})


test_that("rescale_weights errors and warnings", {
  data(nhanes_sample)
  expect_error(
    rescale_weights(
      data = head(nhanes_sample, n = 30),
      by = c("a", "SDMVSTRA", "c"),
      probability_weights = "WTINT2YR"
    ),
    regex = "The following"
  )
  expect_error(
    rescale_weights(
      data = head(nhanes_sample, n = 30),
      by = "SDMVSTRA",
      probability_weights = NULL
    ),
    regex = "is missing, but required"
  )
  expect_error(
    rescale_weights(
      data = head(nhanes_sample, n = 30),
      by = NULL,
      probability_weights = "WTINT2YR"
    ),
    regex = "must be specified"
  )
  expect_warning(
    rescale_weights(
      data = head(nhanes_sample, n = 30),
      by = "SDMVSTRA",
      probability_weights = "WTINT2YR",
      method = "kish"
    ),
    regex = "is not used"
  )
  expect_warning(
    rescale_weights(
      data = head(nhanes_sample, n = 30),
      probability_weights = "WTINT2YR",
      method = "dish"
    ),
    regex = "Invalid option for argument"
  )

  nhanes_sample$rescaled_weights_a <- 1
  expect_warning(
    {
      out <- rescale_weights(
        data = head(nhanes_sample, n = 30),
        by = "SDMVSTRA",
        probability_weights = "WTINT2YR"
      )
    },
    regex = "The variable name"
  )
  expect_named(
    out,
    c(
      "total", "age", "RIAGENDR", "RIDRETH1", "SDMVPSU", "SDMVSTRA",
      "WTINT2YR", "rescaled_weights_a", "rescaled_weights_a_1", "rescaled_weights_b"
    )
  )
})
