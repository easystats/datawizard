test_that("data_restoretype works as expected", {
  data <- data.frame(
    Sepal.Length = c("1", "3", "2"),
    Species = c("setosa", "versicolor", "setosa"),
    New = c("1", "3", "4")
  )

  fixed <- data_restoretype(data, reference = iris)

  expect_equal(typeof(fixed$Species), typeof(iris$Species))
  expect_equal(typeof(fixed$Sepal.Length), typeof(iris$Sepal.Length))
  expect_equal(typeof(fixed$New), "character")
})
