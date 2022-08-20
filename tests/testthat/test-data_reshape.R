library(tidyr)

set.seed(123)
wide_data <- data.frame(replicate(3, sample(1:5)))

test_that("data_reshape works as expected - wide to long", {
  expect_equal(
    head(data_to_long(wide_data)),
    data.frame(
      Name = c("X1", "X2", "X3", "X1", "X2", "X3"),
      Value = c(3L, 3L, 2L, 2L, 1L, 3L),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  expect_equal(
    head(data_to_long(wide_data,
      select = c(1, 2),
      names_to = "Column",
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

  # warning for column name collision
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

  # colnames
  long_data <- data_to_long(wide_data, select = c("X2", "X3"))
  wide <- data_to_wide(long_data, names_from = "name", values_from = "value")
  expect_equal(colnames(wide), colnames(wide_data))
})


test_that("data_reshape works as expected - complex dataset", {
  skip_if_not_installed("psych")

  data <- psych::bfi

  long <- data_to_long(data,
    select = regex("\\d"),
    names_to = "Item",
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
    names_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  long2 <- data_to_long(data,
    select = c("A1", "A2", "A3", "A4", "A5"),
    names_to = "Item",
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
    names_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  long2 <- data_to_long(data,
    select = "age",
    names_to = "Item",
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
    names_to = "Item",
    values_to = "Score",
    rows_to = "Participant",
    ignore_case = TRUE
  )

  long2 <- data_to_long(data,
    select = c("A1", "A2", "A3", "A4", "A5", "age"),
    names_to = "Item",
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
    select = c(1:5, 28),
    names_to = "Item",
    values_to = "Score",
    rows_to = "Participant",
    ignore_case = TRUE
  )

  long2 <- data_to_long(data,
    select = c("A1", "A2", "A3", "A4", "A5", "age"),
    names_to = "Item",
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


test_that("data_to_long: arg 'cols' overrides 'select'", {
  skip_if_not_installed("psych")

  data <- psych::bfi

  expect_identical(
    data_to_long(
      wide_data,
      select = c(1, 2),
      names_to = "Column",
      values_to = "Numbers",
      rows_to = "Row"
    ),
    data_to_long(
      wide_data,
      cols = c(1, 2),
      names_to = "Column",
      values_to = "Numbers",
      rows_to = "Row"
    )
  )

  expect_identical(
    data_to_long(
      data,
      cols = regex("\\d"),
      names_to = "Item",
      values_to = "Score",
      rows_to = "Participant"
    ),
    data_to_long(
      data,
      select = regex("\\d"),
      names_to = "Item",
      values_to = "Score",
      rows_to = "Participant"
    )
  )

  expect_identical(
    data_to_long(
      data,
      cols = starts_with("A"),
      names_to = "Item",
      values_to = "Score",
      rows_to = "Participant"
    ),
    data_to_long(
      data,
      select = starts_with("A"),
      names_to = "Item",
      values_to = "Score",
      rows_to = "Participant"
    )
  )
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
  expect_equal(out$name, c("score_t1", "score_t2", "score_t1", "score_t2", "score_t1", "score_t2"))
  expect_equal(out$value, c(d$score_t1, d$score_t2)[c(1, 4, 2, 5, 3, 6)])

  out <- data_to_long(d, contains("t2"), names_to = "NewCol", values_to = "Time")
  expect_equal(out$NewCol, c("score_t2", "speed_t2", "score_t2", "speed_t2", "score_t2", "speed_t2"))
  expect_equal(out$Time, c(33, 3, 34, 4, 37, 5))
})


test_that("data_reshape works as expected - select-helper inside functions, using regex", {
  test_fun <- function(data, i) {
    data_to_long(data, select = i, regex = TRUE)
  }
  out <- test_fun(d, "^score")
  expect_equal(out$name, c("score_t1", "score_t2", "score_t1", "score_t2", "score_t1", "score_t2"))
  expect_equal(out$value, c(d$score_t1, d$score_t2)[c(1, 4, 2, 5, 3, 6)])
})


test_that("reshape_wider, names_prefix works", {
  out <- fish_encounters %>%
    reshape_wider(
      names_from = "station",
      values_from = "seen",
      names_prefix = "foo_"
    )

  expect_equal(
    names(out),
    c(
      "fish", "foo_Release", "foo_I80_1", "foo_Lisbon", "foo_Rstr", "foo_Base_TD",
      "foo_BCE", "foo_BCW", "foo_BCE2", "foo_BCW2", "foo_MAE", "foo_MAW"
    )
  )
})

test_that("reshape_wider, values_fill errors when wrong type", {
  ### Should be numeric
  expect_error(
    fish_encounters %>%
      reshape_wider(
        names_from = "station",
        values_from = "seen",
        values_fill = "a"
      ),
    regexp = "must be of type numeric"
  )
  expect_error(
    fish_encounters %>%
      reshape_wider(
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
    "company", "google",
    "email", "john@google.com",
    "name", "Huxley Ratcliffe"
  )
  contacts$person_id <- cumsum(contacts$field == "name")

  expect_error(
    contacts %>%
      reshape_wider(
        names_from = "field",
        values_from = "value",
        values_fill = 1
      ),
    regexp = "must be of type character"
  )
  expect_error(
    contacts %>%
      reshape_wider(
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
      reshape_wider(
        names_from = "field",
        values_from = "value",
        values_fill = "a"
      ),
    regexp = "must be of type factor"
  )
  expect_error(
    contacts %>%
      reshape_wider(
        names_from = "field",
        values_from = "value",
        values_fill = 1
      ),
    regexp = "must be of type factor"
  )
})

test_that("reshape_wider, values_fill errors when length > 1", {
  expect_error(
    fish_encounters %>%
      reshape_wider(
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
  df <- tibble(key = c("x", "y", "z"), val = 1:3)
  pv <- reshape_wider(df, names_from = "key", values_from = "val")

  expect_named(pv, c("x", "y", "z"))
  expect_equal(nrow(pv), 1)
})

test_that("non-pivoted cols are preserved", {
  df <- tibble(a = 1, key = c("x", "y"), val = 1:2)
  pv <- reshape_wider(df, names_from = "key", values_from = "val")

  expect_named(pv, c("a", "x", "y"))
  expect_equal(nrow(pv), 1)
})

test_that("implicit missings turn into explicit missings", {
  df <- tibble(a = 1:2, key = c("x", "y"), val = 1:2)
  pv <- reshape_wider(df, names_from = "key", values_from = "val")

  expect_equal(pv$a, c(1, 2))
  expect_equal(pv$x, c(1, NA))
  expect_equal(pv$y, c(NA, 2))
})

test_that("error when overwriting existing column", {
  df <- tibble(
    a = c(1, 1),
    key = c("a", "b"),
    val = c(1, 2)
  )

  expect_error(
    reshape_wider(df, names_from = "key", values_from = "val"),
    regexp = "Some values of the columns specified"
  )
})


### Examples from tidyr website

test_that("reshape_wider equivalent to pivot_wider: ex 1", {
  x <- fish_encounters %>%
    tidyr::pivot_wider(names_from = "station", values_from = "seen", values_fill = 0)

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
    data_filter((product == "A" & country == "AI") | product == "B")

  production$production <- rnorm(nrow(production))

  x <- production %>%
    tidyr::pivot_wider(
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
    tidyr::pivot_wider(
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
    tidyr::pivot_wider(
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
  )
  contacts$person_id <- cumsum(contacts$field == "name")

  x <- contacts %>%
    tidyr::pivot_wider(names_from = field, values_from = value)

  y <- contacts %>%
    reshape_wider(names_from = "field", values_from = "value")

  expect_identical(x, y)
})


test_that("reshape_wider equivalent to pivot_wider: ex 6", {
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

test_that("reshape_wider, names_glue works", {
  df <- data.frame(
    food = c("banana", "banana", "banana", "banana", "cheese", "cheese", "cheese", "cheese"),
    binary = c(rep(c("yes", "no"), 4)),
    car = c("toyota", "subaru", "mazda", "skoda", "toyota", "subaru", "mazda", "skoda"),
    fun = c(2, 4, 3, 6, 2, 4, 2, 3)
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


# EQUIVALENCE WITH TIDYR - PIVOT_LONGER -------------------------------------------

# Examples coming from: https://tidyr.tidyverse.org/articles/pivot.html#longer

test_that("reshape_longer equivalent to pivot_longer: ex 1", {
  x <- relig_income %>%
    tidyr::pivot_longer(!religion, names_to = "income", values_to = "count")

  y <- relig_income %>%
    reshape_longer(select = -religion, names_to = "income", values_to = "count")

  expect_equal(x, y, ignore_attr = TRUE)
})


test_that("reshape_longer equivalent to pivot_longer: ex 2", {
  x <- billboard %>%
    tidyr::pivot_longer(
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
    tidyr::pivot_longer(
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
    tidyr::pivot_longer(
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
      tidyr::pivot_longer(
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
    tidyr::pivot_longer(
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



# tests coming from tidyr's repo
# https://github.com/tidyverse/tidyr/blob/main/tests/testthat/test-pivot-long.R

test_that("can reshape all cols to long", {
  df <- tibble(x = 1:2, y = 3:4)
  pv <- reshape_longer(df, x:y)

  expect_named(pv, c("name", "value"))
  expect_equal(pv$name, rep(names(df), 2))
  expect_equal(pv$value, c(1, 3, 2, 4))
})

test_that("values interleaved correctly", {
  df <- tibble(
    x = c(1, 2),
    y = c(10, 20),
    z = c(100, 200),
  )
  pv <- reshape_longer(df, 1:3)

  expect_equal(pv$value, c(1, 10, 100, 2, 20, 200))
})

test_that("preserves original keys", {
  df <- tibble(x = 1:2, y = 2, z = 1:2)
  pv <- reshape_longer(df, y:z)

  expect_named(pv, c("x", "name", "value"))
  expect_equal(pv$x, rep(df$x, each = 2))
})

test_that("can drop missing values", {
  df <- data.frame(x = c(1, NA), y = c(NA, 2))
  pv <- reshape_longer(df, x:y, values_drop_na = TRUE)

  expect_equal(pv$name, c("x", "y"))
  expect_equal(pv$value, c(1, 2))
})

test_that("mixed columns are automatically coerced", {
  df <- data.frame(x = factor("a"), y = factor("b"))
  pv <- reshape_longer(df, x:y)

  expect_equal(pv$value, factor(c("a", "b")))
})

test_that("error when overwriting existing column", {
  df <- tibble(x = 1, y = 2)

  expect_error(
    reshape_longer(df, y, names_to = "x"),
    regexp = "Some values of the columns specified in 'names_to' are already present"
  )
})
