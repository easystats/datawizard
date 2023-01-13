test_that("data_remove works as expected", {
  expect_identical(
    data_remove(BOD, "Time"),
    structure(list(demand = c(8.3, 10.3, 19, 16, 15.6, 19.8)),
      class = "data.frame",
      row.names = c(NA, 6L),
      reference = "A1.4, p. 270"
    )
  )
})


test_that("data_remove works with NSE", {
  expect_named(
    data_remove(iris, starts_with("Sepal")),
    c("Petal.Length", "Petal.Width", "Species")
  )

  expect_named(
    data_remove(iris, "Sepal"),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_named(
    data_remove(iris, c("Sepal.Length", "Sepal.Width")),
    c("Petal.Length", "Petal.Width", "Species")
  )

  expect_named(
    data_remove(iris, regex("\\.")),
    "Species"
  )

  expect_named(
    data_remove(iris, Sepal.Width:Petal.Width),
    c("Sepal.Length", "Species")
  )

  expect_named(
    data_remove(iris, contains("Sep")),
    c("Petal.Length", "Petal.Width", "Species")
  )

  expect_named(
    data_remove(iris, contains("sep")),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_named(
    data_remove(iris, contains("sep"), ignore_case = TRUE),
    c("Petal.Length", "Petal.Width", "Species")
  )

  expect_named(
    data_remove(iris, 1:3),
    c("Petal.Width", "Species")
  )

  expect_identical(
    colnames(data_remove(iris, c(1, 5))),
    colnames(iris)[2:4]
  )

  expect_identical(
    colnames(data_remove(iris, -1:-2)),
    colnames(iris)[1:2]
  )

  expect_identical(
    colnames(data_remove(iris, c(1, 4:5))),
    colnames(iris)[2:3]
  )

  expect_identical(
    colnames(data_remove(iris, "abc")),
    colnames(iris)
  )

  expect_named(
    data_remove(iris, "Species"),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_named(
    data_remove(iris, "species"),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_named(
    data_remove(iris, "species", ignore_case = TRUE),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
})


test_that("data_remove from other functions", {
  test_fun <- function(data, i) {
    data_remove(data, select = i)
  }
  expect_named(
    test_fun(iris, c("Sepal.Length", "Sepal.Width")),
    c("Petal.Length", "Petal.Width", "Species")
  )
})


# preserve attributes --------------------------

test_that("data_remove preserves attributes", {
  skip_if_not_installed("parameters")

  m <- lm(Sepal.Length ~ Species, data = iris)
  out <- parameters::parameters(m)
  a1 <- attributes(out)

  out2 <- data_remove(out, "SE")
  a2 <- attributes(out2)

  # attributes may not be in the same order
  expect_true(all(names(a1) %in% names(a2)))
  expect_identical(length(a1), length(a2))
})

# select helpers ------------------------------
test_that("data_remove regex", {
  expect_identical(
    names(data_remove(mtcars, select = "pg", regex = TRUE)),
    names(mtcars[-(1)])
  )
})
