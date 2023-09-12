test_that("unnormalize work as expected", {
  x <- normalize(c(0, 1, 5, -5, -2))
  expect_equal(
    unnormalize(x),
    c(0, 1, 5, -5, -2),
    ignore_attr = TRUE
  )
  expect_error(
    unnormalize(c(0, 1, 5, -5, -2)),
    "Can't unnormalize variable"
  )
})

test_that("unnormalize error if not supported", {
  expect_error(
    unnormalize(c("a", "b")),
    "can't be unnormalized"
  )
})

test_that("unnormalize and unstandardized x 4", {
  set.seed(123)
  x <- rnorm(6, 4, 10)

  z <- standardise(x)
  expect_named(attributes(z), c("center", "scale", "robust", "class"))
  expect_equal(attributes(z)$center, 8.47, tolerance = 0.01)
  expect_equal(unstandardise(z), x, ignore_attr = TRUE)

  z <- center(x)
  expect_named(attributes(z), c("center", "scale", "robust", "class"))
  expect_equal(unstandardise(z), x, ignore_attr = TRUE)


  z <- normalize(x)
  expect_named(attributes(z), c(
    "include_bounds", "flag_bounds", "min_value",
    "vector_length", "range_difference", "class"
  ))
  expect_equal(unnormalize(z), x, ignore_attr = TRUE)

  z <- change_scale(x, to = c(-3, 14.5))
  expect_named(attributes(z), c(
    "min_value", "max_value", "new_min", "new_max",
    "range_difference", "to_range", "class"
  ))
  expect_equal(unnormalize(z), x, ignore_attr = TRUE)

  z <- change_scale(x, range = c(-100, 100))
  expect_named(attributes(z), c(
    "min_value", "max_value", "new_min", "new_max",
    "range_difference", "to_range", "class"
  ))
  expect_equal(unnormalize(z), x, ignore_attr = TRUE)
})

# select helpers ------------------------------
test_that("unnormalize regex", {
  x <- normalize(mtcars, select = "mpg")
  expect_identical(
    unnormalize(x, select = "pg", regex = TRUE),
    unnormalize(x, select = "mpg")
  )
})


test_that("unnormalize: grouped data", {
  skip_if_not_installed("poorman")

  # 1 group, 1 normalized var
  norm <- poorman::group_by(mtcars, cyl)
  norm <- normalize(norm, "mpg")
  expect_identical(
    poorman::ungroup(unnormalize(norm, "mpg")),
    mtcars,
    ignore_attr = TRUE # unnormalize removed rownames
  )

  # 2 groups, 1 normalized var
  set.seed(123)
  test <- iris
  test$grp <- sample(c("A", "B"), nrow(test), replace = TRUE)
  norm <- poorman::group_by(test, Species, grp)
  norm <- normalize(norm, "Sepal.Length")
  expect_identical(
    poorman::ungroup(unnormalize(norm, "Sepal.Length")),
    test
  )

  # 2 groups, 2 normalized vars
  set.seed(123)
  test <- iris
  test$grp <- sample(c("A", "B"), nrow(test), replace = TRUE)
  norm <- poorman::group_by(test, Species, grp)
  norm <- normalize(norm, c("Sepal.Length", "Petal.Length"))
  unnorm <- unnormalize(norm, c("Sepal.Length", "Petal.Length"))
  expect_identical(
    poorman::ungroup(unnorm),
    test
  )

  expect_s3_class(unnorm, "grouped_df")

  # can't recover attributes
  norm <- poorman::group_by(iris, Species)
  norm <- normalize(norm, "Sepal.Length")
  attr(norm, "groups") <- NULL

  expect_error(
    unnormalize(norm, "Sepal.Length"),
    regexp = "Couldn't retrieve the necessary information"
  )

  # normalize applied on grouped data but unnormalize applied on ungrouped data
  norm <- poorman::group_by(mtcars, cyl)
  norm <- normalize(norm, "mpg")
  norm <- poorman::ungroup(norm)

  expect_error(
    unnormalize(norm, "mpg"),
    regexp = "Can't unnormalize variable"
  )

  # normalize applied on grouped data but unnormalize applied different grouped
  # data
  norm <- poorman::group_by(norm, am)
  expect_error(
    unnormalize(norm, "mpg"),
    regexp = "Couldn't retrieve the necessary"
  )
})
