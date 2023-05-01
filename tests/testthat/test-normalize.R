test_that("normalize work as expected", {
  expect_equal(
    normalize(c(0, 1, 5, -5, -2)),
    c(0.5, 0.6, 1, 0, 0.3),
    ignore_attr = TRUE
  )

  expect_equal(
    normalize(c(0, 1, 5, -5, -2), include_bounds = FALSE),
    c(0.5, 0.58, 0.9, 0.1, 0.34),
    ignore_attr = TRUE
  )

  expect_equal(
    normalize(c(0, 1, 5, -5, -2), include_bounds = 0.01),
    c(0.5, 0.598, 0.99, 0.01, 0.304),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  expect_equal(
    normalize(c(0, 1, 5, -5, -2), include_bounds = "a", verbose = FALSE),
    c(0.5, 0.6, 1, 0, 0.3),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  expect_warning(normalize(c(0, 1, 5, -5, -2), include_bounds = "a", verbose = TRUE))

  expect_snapshot(head(normalize(trees)))
})


test_that("normalize: only NAs", {
  expect_equal(
    normalize(c(NA_real_, NA_real_)),
    c(NA_real_, NA_real_),
    ignore_attr = TRUE
  )
})


test_that("normalize: with Inf", {
  expect_equal(
    normalize(c(1, 2, 3, NA, Inf)),
    c(0, 0.5, 1, NA, Inf),
    ignore_attr = TRUE
  )
})


test_that("normalize: with Inf", {
  expect_equal(
    normalize(c(1, 2, 3, -Inf, Inf)),
    c(0, 0.5, 1, -Inf, Inf),
    ignore_attr = TRUE
  )
})


test_that("normalize: all Inf", {
  expect_equal(
    normalize(c(-Inf, Inf)),
    c(-Inf, Inf),
    ignore_attr = TRUE
  )
})


test_that("normalize: all Na or Inf", {
  expect_equal(
    normalize(c(NA, -Inf, NA, Inf)),
    c(NA, -Inf, NA, Inf),
    ignore_attr = TRUE
  )
})


test_that("normalize: only one value", {
  foo <- 1
  expect_warning(
    normalize(x = foo),
    regexp = "Variable `foo` contains only one unique value and will"
  )
  expect_warning(
    {
      y <- normalize(x = 12)
    },
    regexp = "Variable `12` contains only one unique value and will"
  )
  expect_equal(y, 12, ignore_attr = TRUE)

  expect_silent(normalize(x = foo, verbose = FALSE))
  expect_equal(normalize(x = foo, verbose = FALSE), 1, ignore_attr = TRUE)
})

test_that("normalize: only two values", {
  expect_warning({
    y <- normalize(x = c(1, 2))
  })
  expect_equal(y, c(0, 1), ignore_attr = TRUE)

  expect_silent(normalize(x = c(1, 2), verbose = FALSE))
  expect_equal(normalize(x = c(1, 2), verbose = FALSE), c(0, 1), ignore_attr = TRUE)
})

test_that("normalize: factor", {
  expect_identical(
    normalize(factor(1:3)),
    factor(1:3)
  )
})

test_that("normalize: matrix", {
  expect_equal(
    normalize(matrix(1:4, ncol = 2)),
    matrix(seq(0, 1, by = 0.3333), ncol = 2),
    tolerance = 1e-3
  )
})

test_that("normalize: select", {
  skip_if_not_installed("poorman")

  expect_equal(
    normalize(
      iris,
      select = starts_with("Petal\\.L")
    ) %>%
      poorman::pull(Petal.Length),
    normalize(iris$Petal.Length),
    ignore_attr = TRUE
  )
})

test_that("normalize: exclude", {
  skip_if_not_installed("poorman")

  expect_identical(
    normalize(
      iris,
      exclude = ends_with("ecies")
    ),
    iris %>%
      normalize(select = 1:4)
  )
})

test_that("normalize, with append", {
  out_n <- normalize(iris, "Sepal.Width", append = TRUE)
  manual <- (iris$Sepal.Width - min(iris$Sepal.Width)) / diff(range(iris$Sepal.Width))
  expect_equal(out_n$Sepal.Width_n, manual, ignore_attr = TRUE)
})


# with grouped data -------------------------------------------

test_that("normalize (grouped data)", {
  skip_if_not_installed("poorman")

  datawizard <- iris %>%
    poorman::group_by(Species) %>%
    normalize(Sepal.Width) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  manual <- iris %>%
    poorman::group_by(Species) %>%
    poorman::mutate(Sepal.Width = (Sepal.Width - min(Sepal.Width)) / diff(range(Sepal.Width))) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  expect_identical(datawizard, manual)
})

test_that("normalize (grouped data), with append", {
  skip_if_not_installed("poorman")

  datawizard_n <- iris %>%
    poorman::group_by(Species) %>%
    normalize(Sepal.Width, append = TRUE) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width_n)

  manual_n <- iris %>%
    poorman::group_by(Species) %>%
    poorman::mutate(Sepal.Width = (Sepal.Width - min(Sepal.Width)) / diff(range(Sepal.Width))) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  expect_identical(datawizard_n, manual_n)
})

test_that("normalize, include bounds (grouped data)", {
  skip_if_not_installed("poorman")

  datawizard <- iris %>%
    poorman::group_by(Species) %>%
    normalize(Sepal.Width, include_bounds = TRUE) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  manual <- iris %>%
    poorman::group_by(Species) %>%
    poorman::mutate(Sepal.Width = (Sepal.Width - min(Sepal.Width)) / diff(range(Sepal.Width))) %>%
    poorman::ungroup() %>%
    poorman::pull(Sepal.Width)

  expect_identical(datawizard, manual)
})


test_that("normalize, factor (grouped data)", {
  skip_if_not_installed("poorman")

  datawizard <- iris %>%
    poorman::group_by(Species) %>%
    normalize(Species) %>%
    poorman::ungroup() %>%
    poorman::pull(Species)

  manual <- iris$Species

  expect_identical(datawizard, manual)
})

# select helpers ------------------------------
test_that("normalize regex", {
  expect_identical(
    normalize(mtcars, select = "pg", regex = TRUE),
    normalize(mtcars, select = "mpg")
  )
})
