data(efc)
data(iris)

test_that("data_codebook iris", {
  x <- data_codebook(iris)
  expect_equal(colnames(x), c("ID", "Name", "Type", "Missings", "Values", "N", ".row_id"))
  expect_equal(dim(x), c(12, 7))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "iris (150 rows and 5 variables, 5 shown)",
      "",
      "ID | Name         | Type        | Missings |     Values |   N",
      "---+--------------+-------------+----------+------------+----",
      "1  | Sepal.Length | numeric     | 0 (0.0%) | [4.3, 7.9] | 150",
      "---+--------------+-------------+----------+------------+----",
      "2  | Sepal.Width  | numeric     | 0 (0.0%) |   [2, 4.4] | 150",
      "---+--------------+-------------+----------+------------+----",
      "3  | Petal.Length | numeric     | 0 (0.0%) |   [1, 6.9] | 150",
      "---+--------------+-------------+----------+------------+----",
      "4  | Petal.Width  | numeric     | 0 (0.0%) | [0.1, 2.5] | 150",
      "---+--------------+-------------+----------+------------+----",
      "5  | Species      | categorical | 0 (0.0%) |     setosa |  50",
      "   |              |             |          | versicolor |  50",
      "   |              |             |          |  virginica |  50",
      "-------------------------------------------------------------"
    )
  )
})


test_that("data_codebook iris, reordered", {
  x <- data_codebook(iris[c(1, 2, 5, 3, 4)])
  out <- capture.output(x)
  expect_equal(out[1], "iris[c(1, 2, 5, 3, 4)] (150 rows and 5 variables, 5 shown)")
})


test_that("data_codebook NaN and Inf", {
  d <- data.frame(
    x = c(1, 4, NA, Inf, 4, NaN, 2, 1, 1)
  )
  x <- data_codebook(d)
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "d (9 rows and 1 variables, 1 shown)",
      "",
      "ID | Name | Type    |  Missings | Values | N",
      "---+------+---------+-----------+--------+--",
      "1  | x    | numeric | 2 (22.2%) |      1 | 3",
      "   |      |         |           |      2 | 1",
      "   |      |         |           |      4 | 2",
      "   |      |         |           |    Inf | 1",
      "--------------------------------------------"
    )
  )

  set.seed(123)
  d <- data.frame(
    x = c(sample(1:15, 100, TRUE), Inf, Inf)
  )
  x <- data_codebook(d)
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "d (102 rows and 1 variables, 1 shown)",
      "",
      "ID | Name | Type    | Missings |  Values |   N",
      "---+------+---------+----------+---------+----",
      "1  | x    | numeric | 0 (0.0%) | [1, 15] | 102",
      "   |      |         |          |     Inf |   2",
      "----------------------------------------------"
    )
  )

  x <- data_codebook(d, range_at = 100)
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "d (102 rows and 1 variables, 1 shown)",
      "",
      "ID | Name | Type    | Missings | Values |  N",
      "---+------+---------+----------+--------+---",
      "1  | x    | numeric | 0 (0.0%) |      1 |  4",
      "   |      |         |          |      2 |  5",
      "   |      |         |          |      3 |  6",
      "   |      |         |          |      4 |  5",
      "   |      |         |          |      5 |  8",
      "   |      |         |          |      6 | 10",
      "   |      |         |          |      7 |  6",
      "   |      |         |          |      8 |  3",
      "   |      |         |          |      9 | 13",
      "   |      |         |          |     10 |  7",
      "   |      |         |          |  (...) |   ",
      "--------------------------------------------"
    )
  )

  x <- data_codebook(d, range_at = 100, max_values = 4)
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "d (102 rows and 1 variables, 1 shown)",
      "",
      "ID | Name | Type    | Missings | Values | N",
      "---+------+---------+----------+--------+--",
      "1  | x    | numeric | 0 (0.0%) |      1 | 4",
      "   |      |         |          |      2 | 5",
      "   |      |         |          |      3 | 6",
      "   |      |         |          |      4 | 5",
      "   |      |         |          |  (...) |  ",
      "-------------------------------------------"
    )
  )
})


test_that("data_codebook iris, select", {
  x <- data_codebook(iris, select = starts_with("Sepal"))
  expect_equal(colnames(x), c("ID", "Name", "Type", "Missings", "Values", "N", ".row_id"))
  expect_equal(dim(x), c(4, 7))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "iris (150 rows and 5 variables, 2 shown)",
      "",
      "ID | Name         | Type    | Missings |     Values |   N",
      "---+--------------+---------+----------+------------+----",
      "1  | Sepal.Length | numeric | 0 (0.0%) | [4.3, 7.9] | 150",
      "---+--------------+---------+----------+------------+----",
      "2  | Sepal.Width  | numeric | 0 (0.0%) |   [2, 4.4] | 150",
      "---------------------------------------------------------"
    )
  )
})


test_that("data_codebook iris, select, ID", {
  x <- data_codebook(iris, select = starts_with("Petal"))
  expect_equal(colnames(x), c("ID", "Name", "Type", "Missings", "Values", "N", ".row_id"))
  expect_equal(dim(x), c(4, 7))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "iris (150 rows and 5 variables, 2 shown)",
      "",
      "ID | Name         | Type    | Missings |     Values |   N",
      "---+--------------+---------+----------+------------+----",
      "3  | Petal.Length | numeric | 0 (0.0%) |   [1, 6.9] | 150",
      "---+--------------+---------+----------+------------+----",
      "4  | Petal.Width  | numeric | 0 (0.0%) | [0.1, 2.5] | 150",
      "---------------------------------------------------------"
    )
  )
})


test_that("data_codebook efc", {
  x <- data_codebook(efc)
  expect_equal(colnames(x), c(
    "ID", "Name", "Label", "Type", "Missings", "Values", "Value Labels", "N", ".row_id"
  ))
  expect_equal(dim(x), c(16, 9))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "efc (100 rows and 5 variables, 5 shown)",
      "",
      "ID | Name     | Label                                    | Type        |   Missings |   Values | Value Labels                    |  N",
      "---+----------+------------------------------------------+-------------+------------+----------+---------------------------------+---",
      "1  | c12hour  | average number of hours of care per week | numeric     |   2 (2.0%) | [5, 168] |                                 | 98",
      "---+----------+------------------------------------------+-------------+------------+----------+---------------------------------+---",
      "2  | e16sex   | elder's gender                           | numeric     |   0 (0.0%) |        1 | male                            | 46",
      "   |          |                                          |             |            |        2 | female                          | 54",
      "---+----------+------------------------------------------+-------------+------------+----------+---------------------------------+---",
      "3  | e42dep   | elder's dependency                       | categorical |   3 (3.0%) |        1 | independent                     |  2",
      "   |          |                                          |             |            |        2 | slightly dependent              |  4",
      "   |          |                                          |             |            |        3 | moderately dependent            | 28",
      "   |          |                                          |             |            |        4 | severely dependent              | 63",
      "---+----------+------------------------------------------+-------------+------------+----------+---------------------------------+---",
      "4  | c172code | carer's level of education               | numeric     | 10 (10.0%) |        1 | low level of education          |  8",
      "   |          |                                          |             |            |        2 | intermediate level of education | 66",
      "   |          |                                          |             |            |        3 | high level of education         | 16",
      "---+----------+------------------------------------------+-------------+------------+----------+---------------------------------+---",
      "5  | neg_c_7  | Negative impact with 7 items             | numeric     |   3 (3.0%) |  [7, 28] |                                 | 97",
      "-------------------------------------------------------------------------------------------------------------------------------------"
    )
  )
})


test_that("data_codebook efc, variable_label_width", {
  x <- data_codebook(efc, variable_label_width = 30)
  expect_equal(dim(x), c(17, 9))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "efc (100 rows and 5 variables, 5 shown)",
      "",
      "ID | Name     | Label                        | Type        |   Missings |   Values | Value Labels                    |  N",
      "---+----------+------------------------------+-------------+------------+----------+---------------------------------+---",
      "1  | c12hour  | average number of hours of   | numeric     |   2 (2.0%) | [5, 168] |                                 | 98",
      "   |          | care per week                |             |            |          |                                 |   ",
      "---+----------+------------------------------+-------------+------------+----------+---------------------------------+---",
      "2  | e16sex   | elder's gender               | numeric     |   0 (0.0%) |        1 | male                            | 46",
      "   |          |                              |             |            |        2 | female                          | 54",
      "---+----------+------------------------------+-------------+------------+----------+---------------------------------+---",
      "3  | e42dep   | elder's dependency           | categorical |   3 (3.0%) |        1 | independent                     |  2",
      "   |          |                              |             |            |        2 | slightly dependent              |  4",
      "   |          |                              |             |            |        3 | moderately dependent            | 28",
      "   |          |                              |             |            |        4 | severely dependent              | 63",
      "---+----------+------------------------------+-------------+------------+----------+---------------------------------+---",
      "4  | c172code | carer's level of education   | numeric     | 10 (10.0%) |        1 | low level of education          |  8",
      "   |          |                              |             |            |        2 | intermediate level of education | 66",
      "   |          |                              |             |            |        3 | high level of education         | 16",
      "---+----------+------------------------------+-------------+------------+----------+---------------------------------+---",
      "5  | neg_c_7  | Negative impact with 7 items | numeric     |   3 (3.0%) |  [7, 28] |                                 | 97",
      "-------------------------------------------------------------------------------------------------------------------------"
    )
  )
})


test_that("data_codebook efc, value_label_width", {
  x <- data_codebook(efc, variable_label_width = 30, value_label_width = 15)
  expect_equal(dim(x), c(17, 9))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "efc (100 rows and 5 variables, 5 shown)",
      "",
      "ID | Name     | Label                        | Type        |   Missings |   Values | Value Labels     |  N",
      "---+----------+------------------------------+-------------+------------+----------+------------------+---",
      "1  | c12hour  | average number of hours of   | numeric     |   2 (2.0%) | [5, 168] |                  | 98",
      "   |          | care per week                |             |            |          |                  |   ",
      "---+----------+------------------------------+-------------+------------+----------+------------------+---",
      "2  | e16sex   | elder's gender               | numeric     |   0 (0.0%) |        1 | male             | 46",
      "   |          |                              |             |            |        2 | female           | 54",
      "---+----------+------------------------------+-------------+------------+----------+------------------+---",
      "3  | e42dep   | elder's dependency           | categorical |   3 (3.0%) |        1 | independent      |  2",
      "   |          |                              |             |            |        2 | slightly...      |  4",
      "   |          |                              |             |            |        3 | moderately...    | 28",
      "   |          |                              |             |            |        4 | severely...      | 63",
      "---+----------+------------------------------+-------------+------------+----------+------------------+---",
      "4  | c172code | carer's level of education   | numeric     | 10 (10.0%) |        1 | low level of...  |  8",
      "   |          |                              |             |            |        2 | intermediate...  | 66",
      "   |          |                              |             |            |        3 | high level of... | 16",
      "---+----------+------------------------------+-------------+------------+----------+------------------+---",
      "5  | neg_c_7  | Negative impact with 7 items | numeric     |   3 (3.0%) |  [7, 28] |                  | 97",
      "----------------------------------------------------------------------------------------------------------"
    )
  )
})


test_that("data_codebook truncated data", {
  set.seed(123)
  d <- data.frame(
    a = sample(1:15, 100, TRUE),
    b = sample(letters[1:18], 100, TRUE),
    stringsAsFactors = FALSE
  )
  x <- data_codebook(d, max_values = 5)
  expect_equal(colnames(x), c("ID", "Name", "Type", "Missings", "Values", "N", ".row_id"))
  expect_equal(dim(x), c(9, 7))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "d (100 rows and 2 variables, 2 shown)",
      "",
      "ID | Name | Type      | Missings |  Values |   N",
      "---+------+-----------+----------+---------+----",
      "1  | a    | integer   | 0 (0.0%) | [1, 15] | 100",
      "---+------+-----------+----------+---------+----",
      "2  | b    | character | 0 (0.0%) |       a |   4",
      "   |      |           |          |       b |   3",
      "   |      |           |          |       c |   5",
      "   |      |           |          |       d |   4",
      "   |      |           |          |       e |   3",
      "   |      |           |          |   (...) |    ",
      "------------------------------------------------"
    )
  )
})


test_that("data_codebook mixed numeric lengths", {
  set.seed(123)
  d <- data.frame(
    a = sample(1:4, 100, TRUE),
    b = sample(5:15, 100, TRUE),
    stringsAsFactors = FALSE
  )
  x <- data_codebook(d)
  expect_equal(dim(x), c(7, 7))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "d (100 rows and 2 variables, 2 shown)",
      "",
      "ID | Name | Type    | Missings |  Values |   N",
      "---+------+---------+----------+---------+----",
      "1  | a    | integer | 0 (0.0%) |       1 |  28",
      "   |      |         |          |       2 |  26",
      "   |      |         |          |       3 |  29",
      "   |      |         |          |       4 |  17",
      "---+------+---------+----------+---------+----",
      "2  | b    | integer | 0 (0.0%) | [5, 15] | 100",
      "----------------------------------------------"
    )
  )
})

test_that("data_codebook mixed range_at", {
  set.seed(123)
  d <- data.frame(
    a = sample(1:4, 100, TRUE),
    b = sample(5:15, 100, TRUE),
    stringsAsFactors = FALSE
  )
  x <- data_codebook(d, range_at = 3)
  expect_equal(dim(x), c(4, 7))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "d (100 rows and 2 variables, 2 shown)",
      "",
      "ID | Name | Type    | Missings |  Values |   N",
      "---+------+---------+----------+---------+----",
      "1  | a    | integer | 0 (0.0%) |  [1, 4] | 100",
      "---+------+---------+----------+---------+----",
      "2  | b    | integer | 0 (0.0%) | [5, 15] | 100",
      "----------------------------------------------"
    )
  )
})


test_that("data_codebook logicals", {
  set.seed(123)
  d <- data.frame(
    a = sample(1:15, 100, TRUE),
    b = sample(letters[1:3], 100, TRUE),
    c = sample(c(TRUE, FALSE), 100, TRUE),
    stringsAsFactors = FALSE
  )
  x <- data_codebook(d)
  expect_equal(x$Values, c("[1, 15]", "", "a", "b", "c", "", "FALSE", "TRUE", ""))
})


test_that("data_codebook labelled data exceptions", {
  set.seed(123)

  f1 <- sample(1:5, 100, TRUE)
  f1[f1 == 4] <- NA
  attr(f1, "labels") <- setNames(1:5, c("One", "Two", "Three", "Four", "Five"))

  f2 <- sample(1:5, 100, TRUE)
  attr(f2, "labels") <- setNames(c(1:3, 5), c("One", "Two", "Three", "Five"))

  f3 <- sample(1:5, 100, TRUE)
  attr(f3, "labels") <- setNames(1:5, c("One", "Two", "Three", "Four", "Five"))

  d <- data.frame(f1, f2, f3)
  x <- data_codebook(d)

  expect_equal(
    x$Values,
    c(names(table(f1)), "", names(table(f2)), "", names(table(f3)), "")
  )
  expect_equal(
    x$N,
    as.character(c(table(f1), "", table(f2), "", table(f3), ""))
  )
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "d (100 rows and 3 variables, 3 shown)",
      "",
      "ID | Name | Type    |   Missings | Values | Value Labels |  N",
      "---+------+---------+------------+--------+--------------+---",
      "1  | f1   | integer | 17 (17.0%) |      1 | One          | 21",
      "   |      |         |            |      2 | Two          | 20",
      "   |      |         |            |      3 | Three        | 23",
      "   |      |         |            |      5 | Five         | 19",
      "---+------+---------+------------+--------+--------------+---",
      "2  | f2   | integer |   0 (0.0%) |      1 | One          | 25",
      "   |      |         |            |      2 | Two          | 20",
      "   |      |         |            |      3 | Three        | 14",
      "   |      |         |            |      4 | 4            | 17",
      "   |      |         |            |      5 | Five         | 24",
      "---+------+---------+------------+--------+--------------+---",
      "3  | f3   | integer |   0 (0.0%) |      1 | One          | 21",
      "   |      |         |            |      2 | Two          | 24",
      "   |      |         |            |      3 | Three        | 16",
      "   |      |         |            |      4 | Four         | 14",
      "   |      |         |            |      5 | Five         | 25",
      "-------------------------------------------------------------"
    )
  )
})


test_that("data_codebook labelled data factors", {
  set.seed(123)

  f1 <- factor(sample(c("c", "b", "a"), 100, TRUE))
  attr(f1, "labels") <- setNames(c("c", "b", "a"), c("Cee", "Bee", "A"))

  f2 <- factor(sample(c("a", "b", "c"), 100, TRUE))
  attr(f2, "labels") <- setNames(c("c", "b", "a"), c("Cee", "Bee", "A"))

  f3 <- factor(sample(c("c", "b", "a"), 100, TRUE))
  attr(f3, "labels") <- setNames(c("a", "c", "b"), c("A", "Cee", "Bee"))

  d <- data.frame(f1, f2, f3)
  x <- data_codebook(d)

  expect_equal(
    x$Values,
    c(names(table(f1)), "", names(table(f2)), "", names(table(f3)), "")
  )
  expect_equal(
    x$N,
    as.character(c(table(f1), "", table(f2), "", table(f3), ""))
  )
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "d (100 rows and 3 variables, 3 shown)",
      "",
      "ID | Name | Type        | Missings | Values | Value Labels |  N",
      "---+------+-------------+----------+--------+--------------+---",
      "1  | f1   | categorical | 0 (0.0%) |      a | A            | 35",
      "   |      |             |          |      b | Bee          | 32",
      "   |      |             |          |      c | Cee          | 33",
      "---+------+-------------+----------+--------+--------------+---",
      "2  | f2   | categorical | 0 (0.0%) |      a | A            | 30",
      "   |      |             |          |      b | Bee          | 38",
      "   |      |             |          |      c | Cee          | 32",
      "---+------+-------------+----------+--------+--------------+---",
      "3  | f3   | categorical | 0 (0.0%) |      a | A            | 23",
      "   |      |             |          |      b | Bee          | 28",
      "   |      |             |          |      c | Cee          | 49",
      "---------------------------------------------------------------"
    )
  )
})


test_that("data_codebook works with numbers < 1", {
  d <- data.frame(
    a = c(1, 1, 2, 2, 3, 3),
    b = c(0, 0, 0, 1, 1, 2)
  )
  out <- data_codebook(d)
  expect_equal(
    out$N,
    c("2", "2", "2", "", "3", "2", "1", "")
  )

  d <- data.frame(
    a = c(1, 1, 2, 2, 3, 3),
    b = c(-1.3, 0.2, 0.2, 1, 1, 2)
  )
  out <- data_codebook(d)
  expect_equal(
    out$N,
    c("2", "2", "2", "", "1", "2", "2", "1", "")
  )
})


test_that("data_codebook, big marks", {
  set.seed(123)
  f1 <- factor(sample(c("c", "b", "a"), 1e6, TRUE))
  f2 <- factor(sample(1:3, 1e6, TRUE))
  d <- data.frame(f1, f2)
  x <- data_codebook(d)
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "d (1,000,000 rows and 2 variables, 2 shown)",
      "",
      "ID | Name | Type        | Missings | Values |       N",
      "---+------+-------------+----------+--------+--------",
      "1  | f1   | categorical | 0 (0.0%) |      a | 333,238",
      "   |      |             |          |      b | 332,910",
      "   |      |             |          |      c | 333,852",
      "---+------+-------------+----------+--------+--------",
      "2  | f2   | categorical | 0 (0.0%) |      1 | 333,285",
      "   |      |             |          |      2 | 333,358",
      "   |      |             |          |      3 | 333,357",
      "-----------------------------------------------------"
    )
  )
})
