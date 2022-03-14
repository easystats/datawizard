test_that("data_remove works as expected", {
  expect_equal(
    data_remove(BOD, "Time"),
    structure(list(demand = c(8.3, 10.3, 19, 16, 15.6, 19.8)),
      class = "data.frame",
      row.names = c(NA, 6L),
      reference = "A1.4, p. 270"
    )
  )
})


test_that("data_remove works with NSE", {
  # can't test this, wrong environment for tests
  # expect_equal(
  #   colnames(data_remove(iris, starts_with("Sepal"))),
  #   c("Petal.Length", "Petal.Width", "Species")
  # )

  expect_equal(
    colnames(data_remove(iris, "Sepal")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    colnames(data_remove(iris, c("Sepal.Length", "Sepal.Width"))),
    c("Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    colnames(data_remove(iris, regex("\\."))),
    "Species"
  )

  expect_equal(
    colnames(data_remove(iris, Sepal.Width:Petal.Width)),
    c("Sepal.Length", "Species")
  )

  expect_equal(
    colnames(data_remove(iris, contains("Sep"))),
    c("Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    colnames(data_remove(iris, contains("sep"))),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    colnames(data_remove(iris, contains("sep"), ignore_case = TRUE)),
    c("Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    colnames(data_remove(iris, 1:3)),
    c("Petal.Width", "Species")
  )

  expect_equal(
    colnames(data_remove(iris, "Species")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_equal(
    colnames(data_remove(iris, "species")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    colnames(data_remove(iris, "species", ignore_case = TRUE)),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
})


test_that("data_remove from other functions", {
  test_fun <- function(data, i) {
    data_remove(data, pattern = i)
  }
  expect_equal(
    colnames(test_fun(iris, c("Sepal.Length", "Sepal.Width"))),
    c("Petal.Length", "Petal.Width", "Species")
  )
})
