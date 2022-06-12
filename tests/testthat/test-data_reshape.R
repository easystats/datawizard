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
      colnames_from = "Name",
      values_from = "Value",
      rows_from = "Row_ID"
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
  expect_warning(
    data_to_wide(
      long_data,
      colnames_from = "Name",
      values_from = "Value",
      rows_from = "Row_ID"
    )
  )

  out <- suppressWarnings(data_to_wide(
    long_data,
    colnames_from = "Name",
    values_from = "Value",
    rows_from = "Row_ID"
  ))

  expect_equal(
    colnames(out),
    c("Row_ID", "X1", "Value_X1", "Value_X2", "Value_X3")
  )

  # colnames
  long_data <- data_to_long(wide_data, select = c("X2", "X3"))
  wide <- data_to_wide(long_data, colnames_from = "Name", values_from = "Value")
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
    colnames_from = "Item",
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
