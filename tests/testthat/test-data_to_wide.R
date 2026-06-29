test_that("data_to_wide works", {
  long_data <- data.frame(
    Row_ID = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    name = c(
      "X1",
      "X1",
      "X1",
      "X1",
      "X1",
      "X2",
      "X2",
      "X2",
      "X2",
      "X2",
      "X3",
      "X3",
      "X3",
      "X3",
      "X3"
    ),
    value = c(3L, 2L, 5L, 4L, 1L, 3L, 1L, 2L, 5L, 4L, 2L, 3L, 1L, 4L, 5L),
    stringsAsFactors = FALSE
  )

  expect_equal(
    data_to_wide(
      long_data,
      names_from = "name",
      values_from = "value",
      id_cols = "Row_ID"
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

  long_data$X1 <- 5
  expect_error(
    data_to_wide(
      long_data,
      names_from = "name",
      values_from = "value",
      id_cols = "Row_ID"
    ),
    regexp = "Some values of the columns specified in `names_from`"
  )
})


test_that("data_to_wide, names_prefix works", {
  skip_if_not_installed("tidyr")

  out <- data_to_wide(
    tidyr::fish_encounters,
    names_from = "station",
    values_from = "seen",
    names_prefix = "foo_"
  )

  expect_named(
    out,
    c(
      "fish",
      "foo_Release",
      "foo_I80_1",
      "foo_Lisbon",
      "foo_Rstr",
      "foo_Base_TD",
      "foo_BCE",
      "foo_BCW",
      "foo_BCE2",
      "foo_BCW2",
      "foo_MAE",
      "foo_MAW"
    )
  )
})


test_that("data_to_wide, values_fill deprecated", {
  skip_if_not_installed("tidyr")

  expect_warning(
    data_to_wide(
      tidyr::fish_encounters,
      names_from = "station",
      values_from = "seen",
      values_fill = c(1, 2)
    ),
    regexp = "`values_fill` is defunct",
    fixed = TRUE
  )
})


# EQUIVALENCE WITH TIDYR - PIVOT_WIDER -----------------------------------------------

# Examples coming from: https://tidyr.tidyverse.org/articles/pivot.html#wider
# and from https://github.com/tidyverse/tidyr/blob/main/tests/testthat/test-pivot-wide.R

### From tidyr tests

test_that("can pivot all cols to wide", {
  skip_if_not_installed("tidyr")

  df <- tidyr::tibble(key = c("x", "y", "z"), val = 1:3)
  pv <- data_to_wide(df, names_from = "key", values_from = "val")

  expect_named(pv, c("x", "y", "z"))
  expect_identical(nrow(pv), 1L)
})

test_that("non-pivoted cols are preserved", {
  skip_if_not_installed("tidyr")

  df <- tidyr::tibble(a = 1, key = c("x", "y"), val = 1:2)
  pv <- data_to_wide(df, names_from = "key", values_from = "val")

  expect_named(pv, c("a", "x", "y"))
  expect_identical(nrow(pv), 1L)
})

test_that("implicit missings turn into explicit missings", {
  skip_if_not_installed("tidyr")

  df <- tidyr::tibble(a = 1:2, key = c("x", "y"), val = 1:2)
  pv <- data_to_wide(df, names_from = "key", values_from = "val")

  expect_identical(pv$a, c(1L, 2L))
  expect_identical(pv$x, c(1L, NA))
  expect_identical(pv$y, c(NA, 2L))
})

test_that("error when overwriting existing column", {
  skip_if_not_installed("tidyr")

  df <- tidyr::tibble(
    a = c(1, 1),
    key = c("a", "b"),
    val = c(1, 2)
  )

  expect_error(
    data_to_wide(df, names_from = "key", values_from = "val"),
    regexp = "Some values of the columns specified"
  )
})

test_that("data_to_wide: fill values, #293", {
  skip_if_not_installed("tidyr")

  weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  daily <- tidyr::tibble(
    day = factor(c("Tue", "Thu", "Fri", "Mon"), levels = weekdays),
    value = c(2, 3, 1, 5),
    type = factor(c("A", "B", "B", "A"))
  )

  expect_identical(
    tidyr::pivot_wider(
      daily,
      names_from = type,
      values_from = value
    ),
    data_to_wide(
      daily,
      names_from = "type",
      values_from = "value"
    )
  )
})

test_that("data_to_wide, id_cols works correctly, #293", {
  skip_if_not_installed("tidyr")

  updates <- tidyr::tibble(
    county = c("Wake", "Wake", "Wake", "Guilford", "Guilford"),
    date = c(as.Date("2020-01-01") + 0:2, as.Date("2020-01-03") + 0:1),
    system = c("A", "B", "C", "A", "C"),
    value = c(3.2, 4, 5.5, 2, 1.2)
  )

  expect_identical(
    tidyr::pivot_wider(
      updates,
      id_cols = county,
      names_from = system,
      values_from = value
    ),
    data_to_wide(
      updates,
      id_cols = "county",
      names_from = "system",
      values_from = "value"
    )
  )
})


### Examples from tidyr website

test_that("data_to_wide equivalent to pivot_wider: ex 1", {
  skip_if_not_installed("tidyr")

  x <- tidyr::pivot_wider(
    tidyr::fish_encounters,
    names_from = "station",
    values_from = "seen"
  )

  y <- data_to_wide(
    tidyr::fish_encounters,
    names_from = "station",
    values_from = "seen"
  )

  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("data_to_wide equivalent to pivot_wider: ex 2", {
  skip_if_not_installed("tidyr")

  production <- tidyr::expand_grid(
    product = c("A", "B"),
    country = c("AI", "EI"),
    year = 2000:2014
  ) %>%
    data_filter((product == "A" & country == "AI") | product == "B")

  production$production <- rnorm(nrow(production))

  x <- production %>%
    tidyr::pivot_wider(
      names_from = c(product, country),
      values_from = production
    )

  y <- production %>%
    data_to_wide(
      names_from = c("product", "country"),
      values_from = "production"
    )

  expect_identical(x, y)
})

test_that("data_to_wide equivalent to pivot_wider: ex 3", {
  skip_if_not_installed("tidyr")

  x <- tidyr::us_rent_income %>%
    tidyr::pivot_wider(
      names_from = variable,
      values_from = c(estimate, moe)
    )

  y <- tidyr::us_rent_income %>%
    data_to_wide(
      names_from = "variable",
      values_from = c("estimate", "moe")
    )

  expect_identical(x, y)
})

test_that("data_to_wide equivalent to pivot_wider: ex 4", {
  skip_if_not_installed("tidyr")

  x <- tidyr::us_rent_income %>%
    tidyr::pivot_wider(
      names_from = variable,
      names_sep = ".",
      values_from = c(estimate, moe)
    )

  y <- tidyr::us_rent_income %>%
    data_to_wide(
      names_from = "variable",
      names_sep = ".",
      values_from = c("estimate", "moe")
    )

  expect_identical(x, y)
})

test_that("data_to_wide equivalent to pivot_wider: ex 5", {
  skip_if_not_installed("tidyr")

  contacts <- tidyr::tribble(
    ~field, ~value,
    "name", "Jiena McLellan",
    "company", "Toyota",
    "name", "John Smith",
    "company", "google",
    "email", "john@google.com",
    "name", "Huxley Ratcliffe"
  )
  contacts$person_id <- cumsum(contacts$field == "name")

  x <- tidyr::pivot_wider(contacts, names_from = field, values_from = value)
  y <- data_to_wide(contacts, names_from = "field", values_from = "value")

  expect_identical(x, y)
})


test_that("data_to_wide equivalent to pivot_wider: ex 6", {
  skip_if_not_installed("tidyr")

  production <- tidyr::expand_grid(
    product = c("A", "B"),
    country = c("AI", "EI"),
    year = 2000:2014
  ) %>%
    data_filter((product == "A" & country == "AI") | product == "B")

  production$production <- rnorm(nrow(production))

  x <- production %>%
    tidyr::pivot_wider(
      names_from = c(product, country),
      values_from = production,
      names_glue = "prod_{product}_{country}"
    )

  y <- production %>%
    data_to_wide(
      names_from = c("product", "country"),
      values_from = "production",
      names_glue = "prod_{product}_{country}"
    )

  expect_identical(x, y)
})


test_that("data_to_wide, names_glue works", {
  skip_if_not_installed("tidyr")

  df <- data.frame(
    food = c(
      "banana",
      "banana",
      "banana",
      "banana",
      "cheese",
      "cheese",
      "cheese",
      "cheese"
    ),
    binary = rep(c("yes", "no"), 4),
    car = c(
      "toyota",
      "subaru",
      "mazda",
      "skoda",
      "toyota",
      "subaru",
      "mazda",
      "skoda"
    ),
    fun = c(2, 4, 3, 6, 2, 4, 2, 3),
    stringsAsFactors = FALSE
  )

  x <- df %>%
    tidyr::pivot_wider(
      id_cols = food,
      names_from = c(car, binary),
      names_glue = "{binary}_{car}",
      values_from = fun
    )

  y <- df %>%
    data_to_wide(
      id_cols = "food",
      names_from = c("car", "binary"),
      names_glue = "{binary}_{car}",
      values_from = "fun"
    )

  expect_identical(x, y, ignore_attr = TRUE)
})


test_that("preserve date format", {
  skip_if_not_installed("tidyr")

  family <- tidyr::tibble(
    family = c(1L, 1L, 2L, 2L, 3L, 3L),
    child = c(
      "dob_child1",
      "dob_child2",
      "dob_child1",
      "dob_child2",
      "dob_child1",
      "dob_child2"
    ),
    value = as.Date(c(
      "1998-11-26",
      "2000-01-29",
      "2004-10-10",
      NA,
      "2000-12-05",
      "2004-04-05"
    ))
  )

  tidyr <- tidyr::pivot_wider(
    family,
    names_from = "child",
    values_from = "value"
  )
  datawiz <- data_to_wide(family, names_from = "child", values_from = "value")

  expect_identical(tidyr, datawiz)
})


test_that("#293", {
  skip_if_not_installed("tidyr")

  weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  daily <- tidyr::tibble(
    day = factor(c("Tue", "Thu", "Fri", "Mon"), levels = weekdays),
    value = c(2, 3, 1, 5)
  )

  expect_identical(
    tidyr::pivot_wider(daily, names_from = day, values_from = value),
    data_to_wide(daily, names_from = "day", values_from = "value")
  )
})


test_that("new names starting with digits are not corrected automatically", {
  skip_if_not_installed("tidyr")

  percentages <- tidyr::tibble(
    year = c(2018, 2019, 2020, 2020),
    type = factor(c("A", "B", "A", "B"), levels = c("A", "B")),
    percentage = c(100, 100, 40, 60)
  )

  tidyr <- tidyr::pivot_wider(
    percentages,
    names_from = c(year, type),
    values_from = percentage
  )
  datawiz <- data_to_wide(
    percentages,
    names_from = c("year", "type"),
    values_from = "percentage"
  )
  expect_identical(tidyr, datawiz)
})


test_that("Preserve column name when names_from column only has one unique value", {
  d <- data.frame(
    Value = rnorm(10),
    Level = paste0("Participant_", 1:10),
    Parameter = "Intercept",
    stringsAsFactors = FALSE
  )
  out <- data_to_wide(
    d,
    values_from = "Value",
    names_from = "Parameter",
    names_sep = "_"
  )
  expect_named(out, c("Level", "Intercept"))
  expect_identical(nrow(out), 10L)

  d <- data.frame(
    Value = rnorm(10),
    Level = paste0("Participant_", 1:10),
    Parameter = c("Intercept", "abc"),
    stringsAsFactors = FALSE
  )
  out <- data_to_wide(
    d,
    values_from = "Value",
    names_from = "Parameter",
    names_sep = "_"
  )
  expect_named(out, c("Level", "Intercept", "abc"))
  expect_identical(nrow(out), 10L)
})


test_that("data_to_wide with multiple values_from and unbalanced panel", {
  skip_if_not_installed("tidyr")

  long_df <- tidyr::tibble(
    subject_id = c(1, 1, 2, 2, 3, 5, 4, 4),
    time = rep(c(1, 2), 4),
    score = c(10, NA, 15, 12, 18, 11, NA, 14),
    anxiety = c(5, 7, 6, NA, 8, 4, 5, NA)
  )

  tidyr <- tidyr::pivot_wider(
    long_df,
    id_cols = "subject_id",
    names_from = time,
    values_from = c(score, anxiety)
  )
  datawiz <- data_to_wide(
    long_df,
    id_cols = "subject_id",
    names_from = "time",
    values_from = c("score", "anxiety")
  )
  expect_identical(tidyr, datawiz)
})


test_that("data_to_wide preserves empty columns", {
  long_df <- data.frame(
    subject_id = c(1, 1, 2, 2, 3, 5, 4, 4),
    time = rep(c(1, 2), 4),
    score = c(10, NA, 15, 12, 18, 11, NA, 14),
    anxiety = c(5, 7, 6, NA, 8, 4, 5, NA),
    test = rep(NA_real_, 8)
  )

  out <- data_to_wide(
    long_df,
    id_cols = "subject_id",
    names_from = "time",
    values_from = c("score", "anxiety", "test")
  )

  expect_equal(
    out,
    data.frame(
      subject_id = c(1, 2, 3, 5, 4),
      score_1 = c(10, 15, 18, NA, NA),
      score_2 = c(NA, 12, NA, 11, 14),
      anxiety_1 = c(5, 6, 8, NA, 5),
      anxiety_2 = c(7, NA, NA, 4, NA),
      test_1 = as.double(c(NA, NA, NA, NA, NA)),
      test_2 = as.double(c(NA, NA, NA, NA, NA))
    ),
    ignore_attr = TRUE
  )
})


test_that("data_to_wide, check for valid columns", {
  long_df <- data.frame(
    subject_id = c(1, 1, 2, 2, 3, 5, 4, 4),
    time = rep(c(1, 2), 4),
    score = c(10, NA, 15, 12, 18, 11, NA, 14),
    anxiety = c(5, 7, 6, NA, 8, 4, 5, NA),
    test = rep(NA_real_, 8)
  )

  expect_error(
    data_to_wide(
      long_df,
      id_cols = "id",
      names_from = "time",
      values_from = c("score", "anxiety", "test")
    ),
    regexp = "`id_cols` must be the names of",
    fixed = TRUE
  )

  expect_error(
    data_to_wide(
      long_df,
      id_cols = "subject_id",
      names_from = "times",
      values_from = c("score", "anxiety", "test")
    ),
    regexp = "`names_from` must be the name of",
    fixed = TRUE
  )

  expect_warning(
    data_to_wide(
      long_df,
      id_cols = "subject_id",
      names_from = "time",
      values_from = c("scores", "anxiety", "test")
    ),
    regexp = "Following variable(s) were not found",
    fixed = TRUE
  )

  expect_error(
    expect_warning(expect_warning(expect_warning(
      data_to_wide(
        long_df,
        id_cols = "subject_id",
        names_from = "time",
        values_from = c("a", "b", "c")
      )
    ))),
    regexp = "No variable defined",
    fixed = TRUE
  )
})


test_that("data_to_wide, select helper for values_from", {
  long_df <- data.frame(
    subject_id = c(1, 1, 2, 2, 3, 5, 4, 4),
    time = rep(c(1, 2), 4),
    score_a = c(10, NA, 15, 12, 18, 11, NA, 14),
    score_b = c(5, 7, 6, NA, 8, 4, 5, NA),
    score_c = rep(NA_real_, 8)
  )

  out <- data_to_wide(
    long_df,
    id_cols = "subject_id",
    names_from = "time",
    values_from = starts_with("score_")
  )

  expect_equal(
    out,
    data.frame(
      subject_id = c(1, 2, 3, 5, 4),
      score_a_1 = c(10, 15, 18, NA, NA),
      score_a_2 = c(NA, 12, NA, 11, 14),
      score_a_1 = c(5, 6, 8, NA, 5),
      score_a_2 = c(7, NA, NA, 4, NA),
      score_a_1 = as.double(c(NA, NA, NA, NA, NA)),
      score_a_2 = as.double(c(NA, NA, NA, NA, NA))
    ),
    ignore_attr = TRUE
  )
})
