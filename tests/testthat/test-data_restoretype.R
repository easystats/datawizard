test_that("data_restoretype works with reference", {
  data <- data.frame(
    Sepal.Length = c("1", "3", "2"),
    Species = c("setosa", "versicolor", "setosa"),
    New = c("1", "3", "4"),
    stringsAsFactors = FALSE
  )

  fixed <- data_restoretype(data, reference = iris)

  expect_equal(typeof(fixed$Species), typeof(iris$Species))
  expect_equal(typeof(fixed$Sepal.Length), typeof(iris$Sepal.Length))
  expect_equal(typeof(fixed$New), "character")
})


test_that("data_restoretype works without reference", {
  data <- data.frame(
    Sepal.Length = c("1", "3", "2"),
    Species = c("setosa", "versicolor", "setosa"),
    New = c("1", "3", "4"),
    stringsAsFactors = FALSE
  )

  expect_equal(
    data_restoretype(data, reference = NULL),
    data.frame(
      Sepal.Length = c(1, 3, 2),
      Species = c("setosa", "versicolor", "setosa"),
      New = c(1, 3, 4),
      stringsAsFactors = FALSE
    )
  )
})
