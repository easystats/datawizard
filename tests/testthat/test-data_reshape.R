set.seed(123)
wide_data <- data.frame(replicate(3, rnorm(5)))

test_that("data_reshape works as expected - wide to long", {
  expect_snapshot(data_to_long(wide_data))

  expect_snapshot(data_to_long(wide_data,
    cols = c(1, 2),
    colnames_to = "Column",
    values_to = "Numbers",
    rows_to = "Row"
  ))
})

test_that("data_reshape works as expected - long to wide", {
  long_data <- data_to_long(wide_data, rows_to = "Row_ID")

  expect_snapshot(data_to_wide(long_data,
    colnames_from = "Name",
    values_from = "Value",
    rows_from = "Row_ID"
  ))
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
