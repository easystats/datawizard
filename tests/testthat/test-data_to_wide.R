test_that("data_to_wide works", {
  long_data <- data.frame(
    Row_ID = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5),
    name = c(
      "X1", "X1", "X1", "X1", "X1", "X2", "X2", "X2", "X2", "X2",
      "X3", "X3", "X3", "X3", "X3"
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
    regexp = "Some values of the columns specified in 'names_from'"
  )
})



test_that("data_to_wide, names_prefix works", {
  skip_if_not_or_load_if_installed("tidyr")

  out <- fish_encounters %>%
    data_to_wide(
      names_from = "station",
      values_from = "seen",
      names_prefix = "foo_"
    )

  expect_named(
    out,
    c(
      "fish", "foo_Release", "foo_I80_1", "foo_Lisbon", "foo_Rstr", "foo_Base_TD",
      "foo_BCE", "foo_BCW", "foo_BCE2", "foo_BCW2", "foo_MAE", "foo_MAW"
    )
  )
})

test_that("data_to_wide, values_fill works", {
  skip_if_not_or_load_if_installed("tidyr")

  data <- fish_encounters[c(1:3, 20:25), ]

  ### Should be numeric
  expect_identical(
    data %>%
      data_to_wide(
        names_from = "station",
        values_from = "seen",
        values_fill = 1
      ),
    tibble(
      fish = factor(
        c("4842", "4843", "4844"),
        levels = c(
          "4842", "4843", "4844", "4845", "4847", "4848", "4849", "4850",
          "4851", "4854", "4855", "4857", "4858", "4859", "4861", "4862",
          "4863", "4864", "4865"
        )
      ),
      Release = c(1, 1, 1),
      I80_1 = c(1, 1, 1),
      Lisbon = c(1, 1, 1),
      BCW2 = c(1, 1, 1),
      MAE = c(1, 1, 1),
      MAW = c(1, 1, 1)
    )
  )
  expect_error(
    data %>%
      data_to_wide(
        names_from = "station",
        values_from = "seen",
        values_fill = "a"
      ),
    regexp = "must be of type numeric"
  )
  expect_error(
    data %>%
      data_to_wide(
        names_from = "station",
        values_from = "seen",
        values_fill = factor("a")
      ),
    regexp = "must be of type numeric"
  )

  ### Should be character
  contacts <- tribble(
    ~field, ~value,
    "name", "Jiena McLellan",
    "company", "Toyota",
    "name", "John Smith",
    "name", "Huxley Ratcliffe"
  )
  contacts$person_id <- cumsum(contacts$field == "name")

  expect_identical(
    contacts %>%
      data_to_wide(
        names_from = "field",
        values_from = "value",
        values_fill = "foo"
      ),
    tibble(
      person_id = 1:3,
      name = c("Jiena McLellan", "John Smith", "Huxley Ratcliffe"),
      company = c("Toyota", "foo", "foo")
    )
  )
  expect_error(
    contacts %>%
      data_to_wide(
        names_from = "field",
        values_from = "value",
        values_fill = 1
      ),
    regexp = "must be of type character"
  )
  expect_error(
    contacts %>%
      data_to_wide(
        names_from = "field",
        values_from = "value",
        values_fill = factor("a")
      ),
    regexp = "must be of type character"
  )

  ### Should be factor
  contacts$value <- as.factor(contacts$value)
  expect_error(
    contacts %>%
      data_to_wide(
        names_from = "field",
        values_from = "value",
        values_fill = "a"
      ),
    regexp = "must be of type factor"
  )
  expect_error(
    contacts %>%
      data_to_wide(
        names_from = "field",
        values_from = "value",
        values_fill = 1
      ),
    regexp = "must be of type factor"
  )
})

test_that("data_to_wide, values_fill errors when length > 1", {
  skip_if_not_or_load_if_installed("tidyr")

  expect_error(
    fish_encounters %>%
      data_to_wide(
        names_from = "station",
        values_from = "seen",
        values_fill = c(1, 2)
      ),
    regexp = "must be of length 1"
  )
})



# EQUIVALENCE WITH TIDYR - PIVOT_WIDER -----------------------------------------------


# Examples coming from: https://tidyr.tidyverse.org/articles/pivot.html#wider
# and from https://github.com/tidyverse/tidyr/blob/main/tests/testthat/test-pivot-wide.R

### From tidyr tests

test_that("can pivot all cols to wide", {
  skip_if_not_or_load_if_installed("tidyr")

  df <- tibble(key = c("x", "y", "z"), val = 1:3)
  pv <- data_to_wide(df, names_from = "key", values_from = "val")

  expect_named(pv, c("x", "y", "z"))
  expect_identical(nrow(pv), 1L)
})

test_that("non-pivoted cols are preserved", {
  skip_if_not_or_load_if_installed("tidyr")

  df <- tibble(a = 1, key = c("x", "y"), val = 1:2)
  pv <- data_to_wide(df, names_from = "key", values_from = "val")

  expect_named(pv, c("a", "x", "y"))
  expect_identical(nrow(pv), 1L)
})

test_that("implicit missings turn into explicit missings", {
  skip_if_not_or_load_if_installed("tidyr")

  df <- tibble(a = 1:2, key = c("x", "y"), val = 1:2)
  pv <- data_to_wide(df, names_from = "key", values_from = "val")

  expect_identical(pv$a, c(1L, 2L))
  expect_identical(pv$x, c(1L, NA))
  expect_identical(pv$y, c(NA, 2L))
})

test_that("error when overwriting existing column", {
  skip_if_not_or_load_if_installed("tidyr")

  df <- tibble(
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
  skip_if_not_or_load_if_installed("tidyr")

  weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  daily <- tibble(
    day = factor(c("Tue", "Thu", "Fri", "Mon"), levels = weekdays),
    value = c(2, 3, 1, 5),
    type = factor(c("A", "B", "B", "A"))
  )

  expect_identical(
    tidyr::pivot_wider(
      daily,
      names_from = type,
      values_from = value,
      values_fill = 0
    ),
    data_to_wide(
      daily,
      names_from = "type",
      values_from = "value",
      values_fill = 0
    )
  )
})

test_that("data_to_wide, id_cols works correctly, #293", {
  skip_if_not_or_load_if_installed("tidyr")

  updates <- tibble(
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
  skip_if_not_or_load_if_installed("tidyr")

  x <- fish_encounters %>%
    tidyr::pivot_wider(names_from = "station", values_from = "seen", values_fill = 0)

  y <- fish_encounters %>%
    data_to_wide(
      names_from = "station",
      values_from = "seen",
      values_fill = 0
    )

  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("data_to_wide equivalent to pivot_wider: ex 2", {
  skip_if_not_or_load_if_installed("tidyr")

  production <- expand_grid(
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
  skip_if_not_or_load_if_installed("tidyr")

  x <- us_rent_income %>%
    tidyr::pivot_wider(
      names_from = variable,
      values_from = c(estimate, moe)
    )

  y <- us_rent_income %>%
    data_to_wide(
      names_from = "variable",
      values_from = c("estimate", "moe")
    )

  expect_identical(x, y)
})

test_that("data_to_wide equivalent to pivot_wider: ex 4", {
  skip_if_not_or_load_if_installed("tidyr")

  x <- us_rent_income %>%
    tidyr::pivot_wider(
      names_from = variable,
      names_sep = ".",
      values_from = c(estimate, moe)
    )

  y <- us_rent_income %>%
    data_to_wide(
      names_from = "variable",
      names_sep = ".",
      values_from = c("estimate", "moe")
    )

  expect_identical(x, y)
})

test_that("data_to_wide equivalent to pivot_wider: ex 5", {
  skip_if_not_or_load_if_installed("tidyr")

  contacts <- tribble(
    ~field, ~value,
    "name", "Jiena McLellan",
    "company", "Toyota",
    "name", "John Smith",
    "company", "google",
    "email", "john@google.com",
    "name", "Huxley Ratcliffe"
  )
  contacts$person_id <- cumsum(contacts$field == "name")

  x <- contacts %>%
    tidyr::pivot_wider(names_from = field, values_from = value)

  y <- contacts %>%
    data_to_wide(names_from = "field", values_from = "value")

  expect_identical(x, y)
})


test_that("data_to_wide equivalent to pivot_wider: ex 6", {
  skip_if_not_or_load_if_installed("tidyr")

  production <- expand_grid(
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
  skip_if_not_or_load_if_installed("tidyr")

  df <- data.frame(
    food = c("banana", "banana", "banana", "banana", "cheese", "cheese", "cheese", "cheese"),
    binary = rep(c("yes", "no"), 4),
    car = c("toyota", "subaru", "mazda", "skoda", "toyota", "subaru", "mazda", "skoda"),
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
  skip_if_not_or_load_if_installed("tidyr")

  family <- tibble(
    family = c(1L, 1L, 2L, 2L, 3L, 3L),
    child = c(
      "dob_child1", "dob_child2", "dob_child1", "dob_child2", "dob_child1",
      "dob_child2"
    ),
    value = as.Date(c(
      "1998-11-26", "2000-01-29", "2004-10-10", NA, "2000-12-05",
      "2004-04-05"
    ))
  )


  tidyr <- tidyr::pivot_wider(family, names_from = "child", values_from = "value")
  datawiz <- data_to_wide(family, names_from = "child", values_from = "value")

  expect_identical(tidyr, datawiz)
})


test_that("#293", {
  skip_if_not_or_load_if_installed("tidyr")

  weekdays <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  daily <- tibble(
    day = factor(c("Tue", "Thu", "Fri", "Mon"), levels = weekdays),
    value = c(2, 3, 1, 5)
  )

  expect_identical(
    tidyr::pivot_wider(daily, names_from = day, values_from = value),
    data_to_wide(daily, names_from = "day", values_from = "value")
  )
})


test_that("new names starting with digits are not corrected automatically", {
  skip_if_not_or_load_if_installed("tidyr")

  percentages <- tibble(
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
