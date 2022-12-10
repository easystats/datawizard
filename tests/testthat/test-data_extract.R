
data(efc)

test_that("data_extract works with select-length > 1", {
  # works with multiple selects
  expect_s3_class(data_extract(efc, select = c("e42dep", "c172code")), "data.frame")

  # colnames properly set
  expect_equal(
    colnames(data_extract(efc, select = c("e42dep", "c172code"))),
    c("e42dep", "c172code")
  )

  # properly extract vector, w/o naming
  expect_equal(data_extract(efc, select = "e42dep"), efc$e42dep)

  # properly extract vector, with naming
  x <- data_extract(efc, select = "e42dep", name = "c172code")
  expect_equal(names(x), as.character(efc$c172code))
})


test_that("data_extract works with select-helpers", {
  expect_equal(
    data_extract(iris, starts_with("Sepal")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  expect_equal(
    data_extract(iris, 1:3),
    iris[1:3]
  )

  expect_equal(
    data_extract(iris, "Species"),
    iris$Species
  )

  expect_equal(
    data_extract(iris, contains("Wid")),
    iris[c("Sepal.Width", "Petal.Width")]
  )

  expect_equal(
    data_extract(iris, Sepal.Width),
    iris$Sepal.Width
  )
})


test_that("data_extract works with formulas", {
  expect_equal(
    data_extract(iris, ~ Sepal.Width + Species),
    iris[c("Sepal.Width", "Species")]
  )
})


test_that("data_extract from other functions", {
  test_fun <- function(data, i) {
    data_extract(data, select = i)
  }
  expect_equal(
    test_fun(iris, c("Sepal.Length", "Sepal.Width")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )
})


test_that("data_extract extract, pull", {
  expect_equal(
    data_extract(iris, starts_with("Sepal")),
    iris[c("Sepal.Length", "Sepal.Width")]
  )

  expect_equal(
    data_extract(iris, starts_with("Sepal"), extract = "first"),
    iris$Sepal.Length
  )

  expect_equal(
    data_extract(iris, starts_with("Sepal"), extract = "last"),
    iris$Sepal.Width
  )

  expect_equal(
    data_extract(iris, starts_with("Sepal"), extract = "last", as_data_frame = TRUE),
    iris["Sepal.Width"]
  )

  expect_equal(
    colnames(data_extract(mtcars, contains("a"))),
    c("drat", "am", "gear", "carb")
  )

  expect_equal(
    colnames(data_extract(mtcars, contains("a"), extract = "odd")),
    c("drat", "gear")
  )

  expect_equal(
    colnames(data_extract(mtcars, contains("a"), extract = "even")),
    c("am", "carb")
  )

  expect_equal(
    colnames(data_extract(mtcars, cyl:drat)),
    c("cyl", "disp", "hp", "drat")
  )

  expect_error(colnames(data_extract(mtcars, Cyl:Drat)))
  expect_equal(
    colnames(data_extract(mtcars, Cyl:Drat, ignore_case = TRUE)),
    c("cyl", "disp", "hp", "drat")
  )

  expect_equal(
    colnames(data_extract(iris, contains("Sep"))),
    c("Sepal.Length", "Sepal.Width")
  )

  expect_null(colnames(data_extract(iris, contains("sep"))))

  expect_equal(
    colnames(data_extract(iris, contains("sep"), ignore_case = TRUE)),
    c("Sepal.Length", "Sepal.Width")
  )

  expect_equal(
    colnames(data_extract(iris, c(1:2, 5))),
    c("Sepal.Length", "Sepal.Width", "Species")
  )
})

# select helpers ------------------------------
test_that("data_extract regex", {
  expect_equal(
    data_extract(mtcars, select = "pg", regex = TRUE),
    data_extract(mtcars, select = "mpg")
  )
  expect_equal(
    data_extract(mtcars, select = "pg$", regex = TRUE),
    data_extract(mtcars, select = "mpg")
  )
})


test_that("data_extract: 'name' is numeric", {
  expect_identical(
    data_extract(mtcars, "gear", 1),
    data_extract(mtcars, "gear", "mpg")
  )
  expect_identical(
    data_extract(mtcars, "gear", -2),
    data_extract(mtcars, "gear", "gear")
  )
  expect_identical(
    data_extract(mtcars, "gear", 0),
    data_extract(mtcars, "gear", "row.names")
  )
})
