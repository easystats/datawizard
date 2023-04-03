set.seed(123)
wide_data <- data.frame(replicate(3, sample(1:5)))

test_that("data_to_long works", {
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
    head(data_to_long(
      wide_data,
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


test_that("data_to_long works - using row names as idvar", {
  data(mtcars)
  out <- data_to_long(mtcars, select = 2:4)
  expect_equal(
    dim(out),
    c(96, 10),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  expect_equal(
    colnames(out),
    c("mpg", "drat", "wt", "qsec", "vs", "am", "gear", "carb", "name", "value"),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  expect_equal(
    head(out$value),
    c(8, 304, 150, 8, 472, 205),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_to_long works - complex dataset", {
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

  long1 <- data_to_long(data,
    select = starts_with("A"),
    names_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  expect_identical(unique(long1$Item), c("A1", "A2", "A3", "A4", "A5"))
  expect_identical(unique(long1$Score), c(2L, 4L, 3L, 5L, 6L, 1L, NA))
  expect_identical(ncol(long1), 26L)
  expect_identical(nrow(long1), 14000L)


  long1 <- data_to_long(data,
    select = starts_with("a"),
    names_to = "Item",
    values_to = "Score",
    rows_to = "Participant"
  )

  expect_identical(ncol(long1), 30L)
  expect_identical(nrow(long1), nrow(data))

  long1 <- data_to_long(data,
    select = starts_with("a"),
    names_to = "Item",
    values_to = "Score",
    rows_to = "Participant",
    ignore_case = TRUE
  )

  expect_identical(unique(long1$Item), c("A1", "A2", "A3", "A4", "A5", "age"))
  expect_identical(ncol(long1), 25L)
  expect_identical(nrow(long1), 16800L)

  long1 <- data_to_long(data,
    select = c(1:5, 28),
    names_to = "Item",
    values_to = "Score",
    rows_to = "Participant",
    ignore_case = TRUE
  )

  expect_identical(unique(long1$Item), c("A1", "A2", "A3", "A4", "A5", "age"))
  expect_identical(ncol(long1), 25L)
  expect_identical(nrow(long1), 16800L)
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

test_that("data_to_long works as expected - simple dataset", {
  out <- data_to_long(d, starts_with("score"))
  expect_identical(
    out$name,
    c("score_t1", "score_t2", "score_t1", "score_t2", "score_t1", "score_t2")
  )
  expect_identical(
    out$value,
    c(d$score_t1, d$score_t2)[c(1, 4, 2, 5, 3, 6)]
  )

  out <- data_to_long(d, contains("t2"), names_to = "NewCol", values_to = "Time")
  expect_identical(
    out$NewCol,
    c("score_t2", "speed_t2", "score_t2", "speed_t2", "score_t2", "speed_t2")
  )
  expect_identical(out$Time, c(33, 3, 34, 4, 37, 5))
})


test_that("data_to_long works as expected - select-helper inside functions, using regex", {
  test_fun <- function(data, i) {
    data_to_long(data, select = i, regex = TRUE)
  }
  out <- test_fun(d, "^score")
  expect_identical(
    out$name,
    c("score_t1", "score_t2", "score_t1", "score_t2", "score_t1", "score_t2")
  )
  expect_identical(
    out$value,
    c(d$score_t1, d$score_t2)[c(1, 4, 2, 5, 3, 6)]
  )
})


test_that("data_to_long: need to provide sep or pattern if several names_to", {
  expect_error(
    data_to_long(wide_data, names_to = c("foo", "foo2")),
    "you supply multiple names"
  )
})

test_that("data_to_long: can't use sep or pattern if only one names_to", {
  expect_error(
    data_to_long(wide_data, names_to = "foo", names_sep = "_"),
    "can't use `names_sep`"
  )

  expect_error(
    data_to_long(wide_data, names_to = "foo", names_pattern = "_"),
    "can't use `names_pattern`"
  )
})

test_that("data_to_long: error if no columns to reshape", {
  expect_error(
    data_to_long(wide_data, cols = "foo"),
    "No columns found"
  )
})






# EQUIVALENCE WITH TIDYR - PIVOT_LONGER -------------------------------------------

# Examples coming from: https://tidyr.tidyverse.org/articles/pivot.html#longer

test_that("data_to_long equivalent to pivot_longer: ex 1", {
  skip_if_not_installed("tidyr")

  x <- tidyr::relig_income %>%
    tidyr::pivot_longer(!religion, names_to = "income", values_to = "count")

  y <- tidyr::relig_income %>%
    data_to_long(cols = -religion, names_to = "income", values_to = "count")

  expect_equal(x, y, ignore_attr = TRUE)
})


test_that("data_to_long equivalent to pivot_longer: ex 2", {
  skip_if_not_installed("tidyr")

  x <- tidyr::billboard %>%
    tidyr::pivot_longer(
      cols = starts_with("wk"),
      names_to = "week",
      values_to = "rank"
    )

  y <- tidyr::billboard %>%
    data_to_long(
      cols = starts_with("wk"),
      names_to = "week",
      values_to = "rank"
    )

  expect_equal(x, y, ignore_attr = TRUE)
})


test_that("data_to_long equivalent to pivot_longer: ex 3", {
  skip_if_not_installed("tidyr")

  x <- tidyr::billboard %>%
    tidyr::pivot_longer(
      cols = starts_with("wk"),
      names_to = "week",
      values_to = "rank",
      values_drop_na = TRUE
    )

  y <- tidyr::billboard %>%
    data_to_long(
      cols = starts_with("wk"),
      names_to = "week",
      values_to = "rank",
      values_drop_na = TRUE
    )

  expect_equal(x, y, ignore_attr = TRUE)
})


test_that("data_to_long equivalent to pivot_longer: ex 4", {
  skip_if_not_installed("tidyr")

  x <- tidyr::billboard %>%
    tidyr::pivot_longer(
      cols = starts_with("wk"),
      names_to = "week",
      names_prefix = "wk",
      values_to = "rank",
      values_drop_na = TRUE
    )

  y <- tidyr::billboard %>%
    data_to_long(
      select = starts_with("wk"),
      names_to = "week",
      names_prefix = "wk",
      values_to = "rank",
      values_drop_na = TRUE
    )

  expect_equal(x, y, ignore_attr = TRUE)
})


test_that("data_to_long equivalent to pivot_longer: ex 5", {
  skip_if_not_installed("tidyr")

  suppressWarnings({
    x <- tidyr::who %>%
      tidyr::pivot_longer(
        cols = 5:60,
        names_to = c("diagnosis", "gender", "age"),
        names_sep = "_",
        values_to = "count"
      )
  })

  y <- tidyr::who %>%
    data_to_long(
      cols = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_sep = "_",
      values_to = "count"
    )

  expect_equal(x, y, ignore_attr = TRUE)
})

test_that("data_to_long equivalent to pivot_longer: ex 6", {
  skip_if_not_installed("tidyr")

  x <- tidyr::who %>%
    tidyr::pivot_longer(
      cols = new_sp_m014:newrel_f65,
      names_to = c("diagnosis", "gender", "age"),
      names_pattern = "new_?(.*)_(.)(.*)",
      values_to = "count"
    )

  y <- tidyr::who %>%
    data_to_long(
      cols = 5:60,
      names_to = c("diagnosis", "gender", "age"),
      names_pattern = "new_?(.*)_(.)(.*)",
      values_to = "count"
    )

  expect_equal(x, y, ignore_attr = TRUE)
})



# tests coming from tidyr's repo
# https://github.com/tidyverse/tidyr/blob/main/tests/testthat/test-pivot-long.R

test_that("can reshape all cols to long", {
  skip_if_not_installed("tidyr")

  df <- tidyr::tibble(x = 1:2, y = 3:4)
  pv <- data_to_long(df, x:y)

  expect_named(pv, c("name", "value"))
  expect_identical(pv$name, rep(names(df), 2))
  expect_identical(pv$value, c(1L, 3L, 2L, 4L))
})

test_that("values interleaved correctly", {
  skip_if_not_installed("tidyr")

  df <- tidyr::tibble(
    x = c(1, 2),
    y = c(10, 20),
    z = c(100, 200)
  )
  pv <- data_to_long(df, 1:3)

  expect_identical(pv$value, c(1, 10, 100, 2, 20, 200))
})

test_that("preserves original keys", {
  skip_if_not_installed("tidyr")

  df <- tidyr::tibble(x = 1:2, y = 2, z = 1:2)
  pv <- data_to_long(df, y:z)

  expect_named(pv, c("x", "name", "value"))
  expect_identical(pv$x, rep(df$x, each = 2))
})

test_that("can drop missing values", {
  skip_if_not_installed("tidyr")

  df <- data.frame(x = c(1, NA), y = c(NA, 2))
  pv <- data_to_long(df, x:y, values_drop_na = TRUE)

  expect_identical(pv$name, c("x", "y"))
  expect_identical(pv$value, c(1, 2))
})

test_that("mixed columns are automatically coerced", {
  skip_if_not_installed("tidyr")

  df <- data.frame(x = factor("a"), y = factor("b"))
  pv <- data_to_long(df, x:y)

  expect_identical(pv$value, factor(c("a", "b")))
})

test_that("error when overwriting existing column", {
  skip_if_not_installed("tidyr")

  df <- tidyr::tibble(x = 1, y = 2)

  expect_error(
    data_to_long(df, y, names_to = "x"),
    regexp = "are already present"
  )
})

test_that("preserve date format", {
  skip_if_not_installed("tidyr")

  family <- tidyr::tibble(
    family = 1:3,
    dob_child1 = as.Date(c("1998-11-26", "2004-10-10", "2000-12-05")),
    dob_child2 = as.Date(c("2000-01-29", NA, "2004-04-05"))
  )

  tidyr <- tidyr::pivot_longer(family, !family, names_to = "child")
  datawiz <- data_to_long(family, -family, names_to = "child")

  expect_identical(tidyr, datawiz)
})
