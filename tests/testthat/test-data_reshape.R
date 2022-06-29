set.seed(123)
wide_data <- data.frame(replicate(3, sample(1:5)))

test_that("data_reshape works as expected - wide to long", {

  expect_equal(
    head(data_to_long(wide_data)),
    data.frame(Name = c("X1", "X2", "X3", "X1", "X2", "X3"),
               Value = c(3L, 3L, 2L, 2L, 1L, 3L),
               stringsAsFactors = FALSE),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  expect_equal(
    head(data_to_long(wide_data,
      cols = c(1, 2),
      colnames_to = "Column",
      values_to = "Numbers",
      rows_to = "Row"
    )),
    data.frame(
      X3 = c(2L, 2L, 3L, 3L, 1L, 1L),
      Row = c(1, 1, 2, 2, 3, 3),
      Column = c("X1", "X2", "X1", "X2", "X1", "X2"),
      Numbers = c(3L, 3L, 2L, 1L, 5L, 2L),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_reshape works as expected - long to wide", {
  long_data <- data_to_long(wide_data, rows_to = "Row_ID")

  expect_equal(
    data_to_wide(
      long_data,
      names_from = "Name",
      values_from = "Value"
    ),
    data.frame(
      Row_ID = c(1, 2, 3, 4, 5),
      X1 = c(3L, 2L, 5L, 4L, 1L),
      X2 = c(3L, 1L, 2L, 5L, 4L),
      X3 = c(2L, 3L, 1L, 4L, 5L),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  # warning for column name collision
  long_data$X1 <- 5
  expect_error(
    data_to_wide(
      long_data,
      names_from = "Name",
      values_from = "Value",
      rows_from = "Row_ID"
    ),
    regexp = "Some values of the columns specified in 'names_from'"
  )

  # colnames
  long_data <- data_to_long(wide_data, select = c("X2", "X3"))
  wide <- data_to_wide(long_data, names_from = "Name", values_from = "Value")
  expect_equal(colnames(wide), colnames(wide_data))
})


test_that("data_reshape works as expected - complex dataset", {
  skip_if_not_installed("psych")

  data <- psych::bfi

  long <- data_to_long(data,
    select = regex("\\d"),
    colnames_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  expect_snapshot(str(long))

  long$Facet <- gsub("\\d", "", long$Item)
  long$Item <- gsub("[A-Z]", "", long$Item)
  long$Item <- paste0("I", long$Item)

  wide <- data_to_wide(long,
    names_from = "Item",
    values_from = "Score"
  )

  expect_snapshot(str(wide))


  long1 <- data_to_long(data,
    select = starts_with("A"),
    colnames_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  long2 <- data_to_long(data,
    cols = c("A1", "A2", "A3", "A4", "A5"),
    colnames_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  expect_equal(unique(long1$Item), c("A1", "A2", "A3", "A4", "A5"))
  expect_equal(unique(long1$Score), c(2L, 4L, 3L, 5L, 6L, 1L, NA))
  expect_equal(ncol(long1), 26)
  expect_equal(nrow(long1), 14000)

  expect_equal(unique(long1$Item), unique(long2$Item))
  expect_equal(unique(long1$Score), unique(long2$Score))
  expect_equal(ncol(long1), ncol(long2))
  expect_equal(nrow(long1), nrow(long2))


  long1 <- data_to_long(data,
    select = starts_with("a"),
    colnames_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  long2 <- data_to_long(data,
    cols = "age",
    colnames_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  expect_equal(ncol(long1), 30)
  expect_equal(nrow(long1), nrow(data))

  expect_equal(unique(long1$Item), unique(long2$Item))
  expect_equal(unique(long1$Score), unique(long2$Score))
  expect_equal(ncol(long1), ncol(long2))
  expect_equal(nrow(long1), nrow(long2))

  long1 <- data_to_long(data,
    select = starts_with("a"),
    colnames_to = "Item",
    values_to = "Score",
    rows_to = "Participant",
    ignore_case = TRUE
  )

  long2 <- data_to_long(data,
    cols = c("A1", "A2", "A3", "A4", "A5", "age"),
    colnames_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  expect_equal(unique(long1$Item), c("A1", "A2", "A3", "A4", "A5", "age"))
  expect_equal(ncol(long1), 25)
  expect_equal(nrow(long1), 16800)

  expect_equal(unique(long1$Item), unique(long2$Item))
  expect_equal(unique(long1$Score), unique(long2$Score))
  expect_equal(ncol(long1), ncol(long2))
  expect_equal(nrow(long1), nrow(long2))


  long1 <- data_to_long(data,
    cols = c(1:5, 28),
    colnames_to = "Item",
    values_to = "Score",
    rows_to = "Participant",
    ignore_case = TRUE
  )

  long2 <- data_to_long(data,
    cols = c("A1", "A2", "A3", "A4", "A5", "age"),
    colnames_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  expect_equal(unique(long1$Item), c("A1", "A2", "A3", "A4", "A5", "age"))
  expect_equal(ncol(long1), 25)
  expect_equal(nrow(long1), 16800)

  expect_equal(unique(long1$Item), unique(long2$Item))
  expect_equal(unique(long1$Score), unique(long2$Score))
  expect_equal(ncol(long1), ncol(long2))
  expect_equal(nrow(long1), nrow(long2))
})


d <- data.frame(
  age = c(20, 30, 40),
  sex = c("Female", "Male", "Male"),
  score_t1 = c(30, 35, 32),
  score_t2 = c(33, 34, 37),
  speed_t1 = c(2, 3, 1),
  speed_t2 = c(3, 4, 5),
  stringsAsFactors = FALSE
)

test_that("data_reshape works as expected - simple dataset", {
  out <- data_to_long(d, starts_with("score"))
  expect_equal(out$Name, c("score_t1", "score_t2", "score_t1", "score_t2", "score_t1", "score_t2"))
  expect_equal(out$Value, c(d$score_t1, d$score_t2)[c(1, 4, 2, 5, 3, 6)])

  out <- data_to_long(d, contains("t2"), colnames_to = "NewCol", values_to = "Time")
  expect_equal(out$NewCol, c("score_t2", "speed_t2", "score_t2", "speed_t2", "score_t2", "speed_t2"))
  expect_equal(out$Time, c(33, 3, 34, 4, 37, 5))
})


test_that("data_reshape works as expected - select-helper inside functions, using regex", {
  test_fun <- function(data, i) {
    data_to_long(data, select = i, regex = TRUE)
  }
  out <- test_fun(d, "^score")
  expect_equal(out$Name, c("score_t1", "score_t2", "score_t1", "score_t2", "score_t1", "score_t2"))
  expect_equal(out$Value, c(d$score_t1, d$score_t2)[c(1, 4, 2, 5, 3, 6)])
})


# EQUIVALENCE WITH TIDYR ----------------------------------------------------

# Examples coming from: https://tidyr.tidyverse.org/articles/pivot.html#wider

library(tidyr)
library(dplyr)

test_that("reshape_wider equivalent to pivot_wider: ex 1", {
  x <- fish_encounters |>
    pivot_wider(names_from = "station", values_from = "seen", values_fill = 0)

  y <- fish_encounters %>%
    reshape_wider(
      names_from = "station",
      values_from = "seen",
      values_fill = 0
    )

  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("reshape_wider equivalent to pivot_wider: ex 2", {
  production <- expand_grid(
    product = c("A", "B"),
    country = c("AI", "EI"),
    year = 2000:2014
  ) %>%
    filter((product == "A" & country == "AI") | product == "B") %>%
    mutate(production = rnorm(nrow(.)))

  x <- production %>%
    pivot_wider(
      names_from = c(product, country),
      values_from = production
    )

  y <- production %>%
    reshape_wider(
      names_from = c("product", "country"),
      values_from = "production"
    )

  expect_identical(x, y)
})

test_that("reshape_wider equivalent to pivot_wider: ex 3", {
  x <- us_rent_income %>%
    pivot_wider(
      names_from = variable,
      values_from = c(estimate, moe)
    )

  y <- us_rent_income %>%
    reshape_wider(
      names_from = "variable",
      values_from = c("estimate", "moe")
    )

  expect_identical(x, y)
})

test_that("reshape_wider equivalent to pivot_wider: ex 4", {
  x <- us_rent_income %>%
    pivot_wider(
      names_from = variable,
      names_sep = ".",
      values_from = c(estimate, moe)
    )

  y <- us_rent_income %>%
    reshape_wider(
      names_from = "variable",
      names_sep = ".",
      values_from = c("estimate", "moe")
    )

  expect_identical(x, y)
})

test_that("reshape_wider equivalent to pivot_wider: ex 5", {
  contacts <- tribble(
    ~field, ~value,
    "name", "Jiena McLellan",
    "company", "Toyota",
    "name", "John Smith",
    "company", "google",
    "email", "john@google.com",
    "name", "Huxley Ratcliffe"
  ) %>%
    mutate(
      person_id = cumsum(field == "name")
    )

  x <- contacts %>%
    pivot_wider(names_from = field, values_from = value)

  y <- contacts %>%
    reshape_wider(names_from = "field", values_from = "value")

  expect_identical(x, y)
})


