data(efc)
data(iris)

test_that("data_codebook iris", {
  x <- data_codebook(iris)
  expect_equal(colnames(x), c("ID", "Name", "missings", "Values", "N"))
  expect_equal(dim(x), c(12, 5))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "iris (total N=150)",
      "",
      "ID |         Name | missings |     Values |   N",
      "---+--------------+----------+------------+----",
      "1  | Sepal.Length | 0 (0.0%) | [4.3, 7.9] | 150",
      "---+--------------+----------+------------+----",
      "2  |  Sepal.Width | 0 (0.0%) |   [2, 4.4] | 150",
      "---+--------------+----------+------------+----",
      "3  | Petal.Length | 0 (0.0%) |   [1, 6.9] | 150",
      "---+--------------+----------+------------+----",
      "4  |  Petal.Width | 0 (0.0%) | [0.1, 2.5] | 150",
      "---+--------------+----------+------------+----",
      "5  |      Species | 0 (0.0%) |     setosa |  50",
      "   |              |          | versicolor |  50",
      "   |              |          |  virginica |  50",
      "-----------------------------------------------"
    )
  )
})


test_that("data_codebook iris, select", {
  x <- data_codebook(iris, select = starts_with("Sepal"))
  expect_equal(colnames(x), c("ID", "Name", "missings", "Values", "N"))
  expect_equal(dim(x), c(4, 5))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "iris (total N=150)",
      "",
      "ID |         Name | missings |     Values |   N",
      "---+--------------+----------+------------+----",
      "1  | Sepal.Length | 0 (0.0%) | [4.3, 7.9] | 150",
      "---+--------------+----------+------------+----",
      "2  |  Sepal.Width | 0 (0.0%) |   [2, 4.4] | 150",
      "-----------------------------------------------"
    )
  )
})


test_that("data_codebook efc", {
  x <- data_codebook(efc)
  expect_equal(colnames(x), c(
    "ID", "Name", "missings", "Label", "Values", "Value Labels", "N"
  ))
  expect_equal(dim(x), c(16, 7))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "efc (total N=100)",
      "",
      "ID |     Name |   missings |                                    Label |   Values |                    Value Labels |  N",
      "---+----------+------------+------------------------------------------+----------+---------------------------------+---",
      "1  |  c12hour |   2 (2.0%) | average number of hours of care per week | [5, 168] |                                 | 98",
      "---+----------+------------+------------------------------------------+----------+---------------------------------+---",
      "2  |   e16sex |   0 (0.0%) |                           elder's gender |        1 |                            male | 46",
      "   |          |            |                                          |        2 |                          female | 54",
      "---+----------+------------+------------------------------------------+----------+---------------------------------+---",
      "3  |   e42dep |   3 (3.0%) |                       elder's dependency |        1 |                     independent |  2",
      "   |          |            |                                          |        2 |              slightly dependent |  4",
      "   |          |            |                                          |        3 |            moderately dependent | 28",
      "   |          |            |                                          |        4 |              severely dependent | 63",
      "---+----------+------------+------------------------------------------+----------+---------------------------------+---",
      "4  | c172code | 10 (10.0%) |               carer's level of education |        1 |          low level of education |  8",
      "   |          |            |                                          |        2 | intermediate level of education | 66",
      "   |          |            |                                          |        3 |         high level of education | 16",
      "---+----------+------------+------------------------------------------+----------+---------------------------------+---",
      "5  |  neg_c_7 |   3 (3.0%) |             Negative impact with 7 items |  [7, 28] |                                 | 97",
      "-----------------------------------------------------------------------------------------------------------------------"
    )
  )
})


test_that("data_codebook efc, label_width", {
  x <- data_codebook(efc, label_width = 30)
  expect_equal(dim(x), c(17, 7))
  out <- capture.output(x)
  expect_equal(
    out,
    c(
      "efc (total N=100)",
      "",
      "ID |     Name |   missings |                        Label |   Values |                    Value Labels |  N",
      "---+----------+------------+------------------------------+----------+---------------------------------+---",
      "1  |  c12hour |   2 (2.0%) |   average number of hours of | [5, 168] |                                 | 98",
      "   |          |            |                care per week |          |                                 |   ",
      "---+----------+------------+------------------------------+----------+---------------------------------+---",
      "2  |   e16sex |   0 (0.0%) |               elder's gender |        1 |                            male | 46",
      "   |          |            |                              |        2 |                          female | 54",
      "---+----------+------------+------------------------------+----------+---------------------------------+---",
      "3  |   e42dep |   3 (3.0%) |           elder's dependency |        1 |                     independent |  2",
      "   |          |            |                              |        2 |              slightly dependent |  4",
      "   |          |            |                              |        3 |            moderately dependent | 28",
      "   |          |            |                              |        4 |              severely dependent | 63",
      "---+----------+------------+------------------------------+----------+---------------------------------+---",
      "4  | c172code | 10 (10.0%) |   carer's level of education |        1 |          low level of education |  8",
      "   |          |            |                              |        2 | intermediate level of education | 66",
      "   |          |            |                              |        3 |         high level of education | 16",
      "---+----------+------------+------------------------------+----------+---------------------------------+---",
      "5  |  neg_c_7 |   3 (3.0%) | Negative impact with 7 items |  [7, 28] |                                 | 97",
      "-----------------------------------------------------------------------------------------------------------"
    )
  )
})
