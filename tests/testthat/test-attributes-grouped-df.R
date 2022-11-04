data(efc, package = "datawizard")


# data_arrange -----------------------------------

test_that("data_arrange, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_arrange(data_group(x, "cyl"), "hp")
  expect_equal(attr(x2, "myattri", exact = TRUE), "I'm here")
})



# rescale -----------------------------------

test_that("rescale, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- datawizard::rescale(data_group(x, "Species"), 1:3)
  expect_equal(attr(x2, "myattri", exact = TRUE), "I'm here")
})



# center -----------------------------------

test_that("center, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- datawizard::center(data_group(x, "Species"), "Sepal.Width")
  expect_equal(attr(x2, "myattri", exact = TRUE), "I'm here")
})



# categorize -----------------------------------

test_that("categorize, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- datawizard::categorize(data_group(x, "Species"), "Sepal.Width")
  expect_equal(attr(x2, "myattri", exact = TRUE), "I'm here")
})



# standardize -----------------------------------

test_that("standardize, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- datawizard::standardize(data_group(x, "Species"), "Sepal.Width")
  expect_equal(attr(x2, "myattri", exact = TRUE), "I'm here")
})
