# numeric
test_that("data_to_factor", {
  x <- c(10, 11, 12)
  expect_equal(
    data_to_factor(x),
    structure(1:3, .Label = c("10", "11", "12"), class = "factor")
  )

  data(efc)
  x <- data_to_factor(efc$c172code)
  expect_equal(
    levels(x),
    c(
      "low level of education", "intermediate level of education",
      "high level of education"
    )
  )
})

# factor
test_that("data_to_factor", {
  data(efc)
  expect_equal(data_to_factor(efc$e42dep), efc$e42dep)
})

# data frame
test_that("data_to_factor", {
  data(iris)
  out <- data_to_factor(iris)
  expect_equal(out$Species, iris$Species)
  expect_true(all(sapply(out, is.factor)))
  expect_equal(
    levels(out$Sepal.Length),
    c(
      "4.3", "4.4", "4.5", "4.6", "4.7", "4.8", "4.9", "5", "5.1",
      "5.2", "5.3", "5.4", "5.5", "5.6", "5.7", "5.8", "5.9", "6",
      "6.1", "6.2", "6.3", "6.4", "6.5", "6.6", "6.7", "6.8", "6.9",
      "7", "7.1", "7.2", "7.3", "7.4", "7.6", "7.7", "7.9"
    )
  )

  out <- data_to_factor(iris, select = starts_with("Sep"), append = TRUE)
  expect_equal(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "Sepal.Length_f", "Sepal.Width_f"
    )
  )
  expect_equal(sum(sapply(out, is.factor)), 3)
})
