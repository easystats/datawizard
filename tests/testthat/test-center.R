test_that("center", {
  z <- center(iris$Sepal.Width)
  expect_equal(
    as.vector(z),
    iris$Sepal.Width - mean(iris$Sepal.Width),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("center, robust", {
  z <- center(mtcars$hp, robust = TRUE)
  expect_equal(
    as.vector(z),
    mtcars$hp - median(mtcars$hp),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("center, select", {
  z <- center(iris, select = "Sepal.Width")
  expect_equal(
    as.vector(z$Sepal.Width),
    iris$Sepal.Width - mean(iris$Sepal.Width),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  # check class attributes
  expect_identical(
    vapply(z, class, character(1)),
    c(
      Sepal.Length = "numeric", Sepal.Width = "numeric", Petal.Length = "numeric",
      Petal.Width = "numeric", Species = "factor"
    )
  )
})

test_that("center, factors", {
  z <- center(iris, select = "Species")
  expect_identical(z$Species, iris$Species)
})

test_that("center, force factors", {
  z <- center(iris, select = "Species", force = TRUE)
  v <- as.numeric(iris$Species)
  expect_equal(as.vector(z$Species),
    v - median(v),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("center, all na", {
  z <- center(c(NA, NA, NA))
  expect_identical(z, c(NA, NA, NA))
})

test_that("center, with Inf", {
  z <- center(c(2, 4, Inf))
  expect_equal(z, c(-1, 1, NA), ignore_attr = TRUE)
})

test_that("center, all NA or Inf", {
  z <- center(c(NA, -Inf, Inf))
  expect_equal(z, c(NA, -Inf, Inf), ignore_attr = TRUE)
})

test_that("center works correctly with only one value", {
  expect_message(
    x <- center(100), # nolint
    "will be set to 0"
  )
  expect_equal(x, 0, ignore_attr = TRUE)

  expect_equal(center(100, center = 1), 99, ignore_attr = TRUE)
  expect_equal(
    center(100, reference = mtcars$mpg),
    100 - mean(mtcars$mpg),
    ignore_attr = TRUE
  )
})


# with grouped data -------------------------------------------

test_that("center (grouped data)", {
  skip_if_not_installed("poorman")

  datawizard <- iris %>%
    poorman::group_by(Species) %>%
    center(Sepal.Width) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  manual <- iris %>%
    poorman::group_by(Species) %>%
    poorman::mutate(Sepal.Width = Sepal.Width - mean(Sepal.Width)) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  expect_identical(datawizard, manual)
})

test_that("center (grouped data), with force = TRUE", {
  skip_if_not_installed("poorman")

  datawizard_c <- iris %>%
    poorman::group_by(Species) %>%
    center(force = TRUE) %>%
    poorman::ungroup()

  manual_c <- iris %>%
    poorman::group_by(Species) %>%
    poorman::mutate(
      Sepal.Length = Sepal.Length - mean(Sepal.Length),
      Sepal.Width = Sepal.Width - mean(Sepal.Width),
      Petal.Length = Petal.Length - mean(Petal.Length),
      Petal.Width = Petal.Width - mean(Petal.Width)
    ) %>%
    poorman::ungroup()

  expect_equal(datawizard_c, manual_c, ignore_attr = TRUE)
})

test_that("center, robust (grouped data)", {
  skip_if_not_installed("poorman")

  datawizard <- iris %>%
    poorman::group_by(Species) %>%
    center(Sepal.Width, robust = TRUE) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  manual <- iris %>%
    poorman::group_by(Species) %>%
    poorman::mutate(Sepal.Width = Sepal.Width - median(Sepal.Width)) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  expect_identical(datawizard, manual)
})

test_that("center, select (grouped data)", {
  skip_if_not_installed("poorman")

  datawizard <- iris %>%
    poorman::group_by(Species) %>%
    center(select = starts_with("Sepal\\.W")) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  manual <- iris %>%
    poorman::group_by(Species) %>%
    poorman::mutate(Sepal.Width = Sepal.Width - mean(Sepal.Width)) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  expect_identical(datawizard, manual)
})

test_that("center, factors (grouped data)", {
  skip_if_not_installed("poorman")

  datawizard <- iris %>%
    poorman::group_by(Species) %>%
    center(select = "Species") %>%
    poorman::ungroup() %>%
    poorman::pull(Species)

  manual <- poorman::pull(iris, Species)

  expect_identical(datawizard, manual)
})

# select helpers ------------------------------
test_that("center regex", {
  expect_equal(
    center(mtcars, select = "pg", regex = TRUE)$mpg,
    center(mtcars$mpg),
    ignore_attr = TRUE
  )
  expect_equal(
    center(mtcars, select = "pg$", regex = TRUE)$mpg,
    center(mtcars$mpg),
    ignore_attr = TRUE
  )
})

# no matches ------------------------------
test_that("center no match", {
  data(iris)
  expect_warning(center(iris, "Sepla.Length"))
})
