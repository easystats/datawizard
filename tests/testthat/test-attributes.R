data(efc, package = "datawizard")

# data_filter -----------------------------------

test_that("data_filter, attributes preserved", {
  attr(efc, "myattri") <- "I'm here"
  x <- data_filter(efc, c172code == 1 & c12hour > 40)
  expect_identical(
    attr(x$e42dep, "label", exact = TRUE),
    attr(efc$e42dep, "label", exact = TRUE)
  )
  expect_identical(
    attr(x, "myattri", exact = TRUE),
    "I'm here"
  )
})


# data_arrange -----------------------------------

test_that("data_arrange, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_arrange(x, "hp")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# data_match -----------------------------------

test_that("data_match, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_match(x, data.frame(vs = 0, am = 1))
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# data_select -----------------------------------

test_that("data_select, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_select(x, "hp")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# data_group -----------------------------------

test_that("data_group, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_group(x, "cyl")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# data_relocate -----------------------------------

test_that("data_relocate, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_relocate(x, "am", "mpg")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# data_remove -----------------------------------

test_that("data_remove, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_remove(x, "am")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# data_reorder -----------------------------------

test_that("data_reorder, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_reorder(x, c("hp", "vs", "wt"))
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# data_to_long -----------------------------------

test_that("data_to_long, attributes preserved", {
  wide_data <- data.frame(replicate(5, rnorm(10)))
  attr(wide_data, "myattri") <- "I'm here"
  x2 <- data_to_long(wide_data)
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# to_numeric -----------------------------------

test_that("to_numeric, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- to_numeric(x, "Species")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# convert_to_na -----------------------------------

test_that("convert_to_na, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- convert_to_na(x, na = 2, verbose = FALSE)
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
  # label attribute is preserved
  attr(x$Species, "label") <- "Species Variable"
  x2 <- convert_to_na(x, na = "setosa", drop_levels = TRUE, verbose = FALSE)
  expect_identical(attributes(x$Species)$label, "Species Variable")
})


# data_rename -----------------------------------

test_that("data_rename, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- data_rename(x, select = "hp", replacement = "horsepower")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# rescale -----------------------------------

test_that("rescale, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- rescale(x, 1:3)
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# center -----------------------------------

test_that("center, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- center(x, "Sepal.Width")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# categorize -----------------------------------

test_that("categorize, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- categorize(x, "Sepal.Width")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# change_code -----------------------------------

test_that("recode_values, attributes preserved", {
  x <- mtcars
  attr(x, "myattri") <- "I'm here"
  x2 <- recode_values(x, select = "am", recode = list(`5` = 0, `10` = 1))
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})


# standardize -----------------------------------

test_that("standardize, attributes preserved", {
  x <- iris
  attr(x, "myattri") <- "I'm here"
  x2 <- standardize(x, "Sepal.Width")
  expect_identical(attr(x2, "myattri", exact = TRUE), "I'm here")
})
