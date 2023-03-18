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
  skip_if_not_or_load_if_installed("poorman")

  datawizard <- iris %>%
    group_by(Species) %>%
    center(Sepal.Width) %>%
    ungroup() %>%
    pull(Sepal.Width)

  manual <- iris %>%
    group_by(Species) %>%
    mutate(Sepal.Width = Sepal.Width - mean(Sepal.Width)) %>%
    ungroup() %>%
    pull(Sepal.Width)

  expect_identical(datawizard, manual)
})

test_that("center, robust (grouped data)", {
  skip_if_not_or_load_if_installed("poorman")

  datawizard <- iris %>%
    group_by(Species) %>%
    center(Sepal.Width, robust = TRUE) %>%
    ungroup() %>%
    pull(Sepal.Width)

  manual <- iris %>%
    group_by(Species) %>%
    mutate(Sepal.Width = Sepal.Width - median(Sepal.Width)) %>%
    ungroup() %>%
    pull(Sepal.Width)

  expect_identical(datawizard, manual)
})

test_that("center, select (grouped data)", {
  skip_if_not_or_load_if_installed("poorman")

  datawizard <- iris %>%
    group_by(Species) %>%
    center(select = starts_with("Sepal\\.W")) %>%
    ungroup() %>%
    pull(Sepal.Width)

  manual <- iris %>%
    group_by(Species) %>%
    mutate(Sepal.Width = Sepal.Width - mean(Sepal.Width)) %>%
    ungroup() %>%
    pull(Sepal.Width)

  expect_identical(datawizard, manual)
})

test_that("center, factors (grouped data)", {
  skip_if_not_or_load_if_installed("poorman")

  datawizard <- iris %>%
    group_by(Species) %>%
    center(select = "Species") %>%
    ungroup() %>%
    pull(Species)

  manual <- iris %>%
    pull(Species)

  expect_identical(datawizard, manual)
})

# select helpers ------------------------------
test_that("center regex", {
  expect_identical(
    center(mtcars, select = "pg", regex = TRUE)$mpg,
    center(mtcars$mpg)
  )
  expect_identical(
    center(mtcars, select = "pg$", regex = TRUE)$mpg,
    center(mtcars$mpg)
  )
})

# no matches ------------------------------
test_that("center no match", {
  data(iris)
  expect_warning(center(iris, "Sepla.Length"))
})
