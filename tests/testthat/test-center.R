suppressPackageStartupMessages(library(poorman, warn.conflicts = FALSE))

data(iris)
data(mtcars)
z <- center(iris$Sepal.Width)

test_that("center", {
  expect_equal(
    as.vector(z),
    iris$Sepal.Width - mean(iris$Sepal.Width),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})
z <- center(mtcars$hp, robust = TRUE)
test_that("center, robust", {
  expect_equal(
    as.vector(z),
    mtcars$hp - median(mtcars$hp),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

z <- center(iris, select = "Sepal.Width")
test_that("center, select", {
  expect_equal(
    as.vector(z$Sepal.Width),
    iris$Sepal.Width - mean(iris$Sepal.Width),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

z <- center(iris, select = "Species")
test_that("center, factors", {
  expect_equal(z$Species, iris$Species)
})

z <- center(iris, select = "Species", force = TRUE)
v <- as.numeric(iris$Species)
test_that("center, force factors", {
  expect_equal(as.vector(z$Species),
    v - median(v),
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
})

test_that("center, all na", {
  z <- center(c(NA, NA, NA))
  expect_equal(z, c(NA, NA, NA))
})

test_that("center, with Inf", {
  z <- center(c(2, 4, Inf))
  expect_equal(z, c(-1, 1, NA), ignore_attr = TRUE)
})

test_that("center, all NA or Inf", {
  z <- center(c(NA, -Inf, Inf))
  expect_equal(z, c(NA, -Inf, Inf), ignore_attr = TRUE)
})



# with grouped data -------------------------------------------

test_that("center (grouped data)", {
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

  expect_equal(datawizard, manual)
})

test_that("center, robust (grouped data)", {
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

  expect_equal(datawizard, manual)
})

test_that("center, select (grouped data)", {
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

  expect_equal(datawizard, manual)
})

test_that("center, factors (grouped data)", {
  datawizard <- iris %>%
    group_by(Species) %>%
    center(select = "Species") %>%
    ungroup() %>%
    pull(Species)

  manual <- iris %>%
    pull(Species)

  expect_equal(datawizard, manual)
})
