data(efc, package = "datawizard")


# data_filter -----------------------------------

test_that("data_filter, attributes preserved", {
  attr(efc, "myattri") <- "I'm here"
  x <- data_filter(efc, c172code == 1 & c12hour > 40)
  expect_equal(
    attr(x$e42dep, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE)
  )
  expect_equal(
    attr(x$myattri, "label", exact = TRUE),
    "I'm here"
  )
})



# data_arrange -----------------------------------

test_that("data_arrange, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_arrange(x, "hp")
  expect_equal(attr(x2$myattri, "label", exact = TRUE), "I'm here")
})



# data_select -----------------------------------

test_that("data_select, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_select(x, "hp")
  expect_equal(attr(x2$myattri, "label", exact = TRUE), "I'm here")
})



# to_numeric -----------------------------------

test_that("to_numeric, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- datawizard::to_numeric(x, "Species")
  expect_equal(attr(x2$myattri, "label", exact = TRUE), "I'm here")
})
