library(tidyr)

# EQUIVALENCE WITH TIDYR ----------------------------------------------------

# Examples coming from: https://tidyr.tidyverse.org/articles/pivot.html#longer

test_that("reshape_longer equivalent to pivot_longer: ex 1", {
  x <- relig_income %>%
    pivot_longer(!religion, names_to = "income", values_to = "count")

  y <- relig_income %>%
    reshape_longer(select = -religion, names_to = "income", values_to = "count")

  expect_equal(x, y, ignore_attr = TRUE)
})


test_that("reshape_longer equivalent to pivot_longer: ex 2", {
  x <- billboard %>%
    pivot_longer(
      cols = starts_with("wk"),
      names_to = "week",
      values_to = "rank"
    )

  y <- billboard %>%
    reshape_longer(
      select = starts_with("wk"),
      names_to = "week",
      values_to = "rank"
    )

  expect_equal(x, y, ignore_attr = TRUE)
})


test_that("reshape_longer equivalent to pivot_longer: ex 3", {
  x <- billboard %>%
    pivot_longer(
      cols = starts_with("wk"),
      names_to = "week",
      values_to = "rank",
      values_drop_na = TRUE
    )

  y <- billboard %>%
    reshape_longer(
      select = starts_with("wk"),
      names_to = "week",
      values_to = "rank",
      values_drop_na = TRUE
    )

  expect_equal(x, y, ignore_attr = TRUE)
})


test_that("reshape_longer equivalent to pivot_longer: ex 4", {
  x <- billboard %>%
    pivot_longer(
      cols = starts_with("wk"),
      names_to = "week",
      names_prefix = "wk",
      values_to = "rank",
      values_drop_na = TRUE
    )

  y <- billboard %>%
    reshape_longer(
      select = starts_with("wk"),
      names_to = "week",
      names_prefix = "wk",
      values_to = "rank",
      values_drop_na = TRUE
    )

  expect_equal(x, y, ignore_attr = TRUE)
})


test_that("reshape_longer equivalent to pivot_longer: ex 5", {
  suppressWarnings({
    x <- who %>%
      pivot_longer(
        cols = 5:60,
        names_to = c("diagnosis", "gender", "age"),
        names_sep = "_",
        values_to = "count"
      )
  })

  y <- who %>%
    reshape_longer(
      select = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_sep = "_",
      values_to = "count"
    )

  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("reshape_longer equivalent to pivot_longer: ex 6", {
  x <- who %>%
    pivot_longer(
      cols = new_sp_m014:newrel_f65,
      names_to = c("diagnosis", "gender", "age"),
      names_pattern = "new_?(.*)_(.)(.*)",
      values_to = "count"
    )

  y <- who %>%
    reshape_longer(
      select = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_pattern = "new_?(.*)_(.)(.*)",
      values_to = "count"
    )

  expect_equal(x, y, ignore_attr = TRUE)
})
