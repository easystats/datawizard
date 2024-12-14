# data_arrange -----------------------------------

test_that("data_arrange, attributes preserved", {
  # if dplyr:::`[.grouped_df` in the environment it destroys the attributes
  # (only occurs when we run tests in random order)
  skip_if("[.grouped_df" %in% methods(`[`))
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_arrange(data_group(x, "cyl"), "hp")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# rescale -----------------------------------

test_that("rescale, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- rescale(data_group(x, "Species"), 1:3)
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# center -----------------------------------

test_that("center, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- center(data_group(x, "Species"), "Sepal.Width")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# categorize -----------------------------------

test_that("categorize, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- categorize(data_group(x, "Species"), "Sepal.Width")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# standardize -----------------------------------

test_that("standardize, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- standardize(data_group(x, "Species"), "Sepal.Width")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})

# filter -----------------------------------

test_that("filter, attributes preserved", {
  # if dplyr:::`[.grouped_df` in the environment it destroys the attributes
  # (only occurs when we run tests in random order)
  skip_if("[.grouped_df" %in% methods(`[`))
  test <- data.frame(
    id = c(1, 1, 2, 2),
    x = c(0, 1, 3, 4)
  )
  attr(test, "myattri") <- "I'm here"
  test2 <- data_filter(data_group(test, "id"), x == min(x))
  expect_identical(attr(test2, "myattri", exact = TRUE), "I'm here")
})
