
d <- iris[1:4, ]

# standardize -----------------------------------------------------
test_that("standardize.data.frame", {
  x <- standardise(d, select = c("Sepal.Length", "Sepal.Width"))
  expect_equal(as.vector(x$Sepal.Length), as.vector(scale(d$Sepal.Length)), tolerance = 0.001)
  expect_equal(as.vector(x$Petal.Length), as.vector(d$Petal.Length), tolerance = 0.001)
  expect_equal(colnames(x), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"))

  x <- standardise(d, select = c("Sepal.Length", "Sepal.Width"), append = TRUE)
  expect_equal(as.vector(x$Sepal.Length_z), as.vector(scale(d$Sepal.Length)), tolerance = 0.001)
  expect_equal(as.vector(x$Sepal.Length), as.vector(d$Sepal.Length), tolerance = 0.001)
  expect_equal(as.vector(x$Petal.Length), as.vector(d$Petal.Length), tolerance = 0.001)
  expect_equal(
    colnames(x),
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "Sepal.Length_z",
      "Sepal.Width_z"
    )
  )
})

test_that("standardize other classes", {
  d <- data.frame(
    a = 1:5,
    b = factor(letters[1:5]),
    c = as.Date(c("2022-03-22", "2022-01-02", "2022-02-02", "2021-04-02", "2020-01-19")),
    d = c(TRUE, TRUE, FALSE, FALSE, TRUE),
    e = as.complex(1:5)
  )

  x <- standardize(d$a)
  expect_equal(
    as.numeric(x),
    c(-1.26491, -0.63246, 0, 0.63246, 1.26491),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  x <- standardize(d$b)
  expect_equal(as.numeric(x), 1:5, tolerance = 1e-3, ignore_attr = TRUE)
  x <- standardize(d$b, force = TRUE)
  expect_equal(
    as.numeric(x),
    c(-1.26491, -0.63246, 0, 0.63246, 1.26491),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
  x <- standardize(d$c)
  expect_equal(x, as.Date(
    c(
      "2022-03-22",
      "2022-01-02",
      "2022-02-02",
      "2021-04-02",
      "2020-01-19"
    )
  ), tolerance = 1e-3, ignore_attr = TRUE)
  x <- standardize(d$c, force = TRUE)
  expect_equal(as.numeric(x), c(0.76992, 0.53121, 0.62488, -0.29975, -1.62626), tolerance = 1e-3, ignore_attr = TRUE)
  x <- standardize(d$d)
  expect_equal(x, c(TRUE, TRUE, FALSE, FALSE, TRUE), tolerance = 1e-3, ignore_attr = TRUE)
  expect_message(x <- standardize(d$d, force = TRUE))
  expect_equal(
    x,
    c(0.7303, 0.7303, -1.09545, -1.09545, 0.7303),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  ## TODO conflict with standardize.default() in effectsize

  # expect_message(x <- standardize(d$e))
  # expect_equal(x, d$e, tolerance = 1e-3, ignore_attr = TRUE)
})


# center -----------------------------------------------------
test_that("center.data.frame", {
  x <- center(d, select = c("Sepal.Length", "Sepal.Width"))
  expect_equal(as.vector(x$Sepal.Length), as.vector(d$Sepal.Length - mean(d$Sepal.Length)), tolerance = 0.001)
  expect_equal(as.vector(x$Petal.Length), as.vector(d$Petal.Length), tolerance = 0.001)
  expect_equal(colnames(x), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"))

  x <- center(d, select = c("Sepal.Length", "Sepal.Width"), append = TRUE)
  expect_equal(as.vector(x$Sepal.Length_c),
    as.vector(d$Sepal.Length - mean(d$Sepal.Length)),
    tolerance = 0.001
  )
  expect_equal(as.vector(x$Sepal.Length), as.vector(d$Sepal.Length), tolerance = 0.001)
  expect_equal(as.vector(x$Petal.Length), as.vector(d$Petal.Length), tolerance = 0.001)
  expect_equal(
    colnames(x),
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "Sepal.Length_c",
      "Sepal.Width_c"
    )
  )
})

test_that("center other classes", {
  d <- data.frame(
    a = 1:5,
    b = factor(letters[1:5]),
    c = as.Date(c("2022-03-22", "2022-01-02", "2022-02-02", "2021-04-02", "2020-01-19")),
    d = c(TRUE, TRUE, FALSE, FALSE, TRUE),
    e = as.complex(1:5)
  )

  x <- center(d$a)
  expect_equal(as.numeric(x), c(-2, -1, 0, 1, 2), tolerance = 1e-3, ignore_attr = TRUE)
  x <- center(d$b)
  expect_equal(as.numeric(x), 1:5, tolerance = 1e-3, ignore_attr = TRUE)
  x <- center(d$b, force = TRUE)
  expect_equal(as.numeric(x), c(-2, -1, 0, 1, 2), tolerance = 1e-3, ignore_attr = TRUE)
  x <- center(d$c)
  expect_equal(x, as.Date(
    c(
      "2022-03-22",
      "2022-01-02",
      "2022-02-02",
      "2021-04-02",
      "2020-01-19"
    )
  ), tolerance = 1e-3, ignore_attr = TRUE)
  x <- center(d$c, force = TRUE)
  expect_equal(as.numeric(x), c(254.8, 175.8, 206.8, -99.2, -538.2), tolerance = 1e-3, ignore_attr = TRUE)
  x <- center(d$d)
  expect_equal(x, c(TRUE, TRUE, FALSE, FALSE, TRUE), tolerance = 1e-3, ignore_attr = TRUE)
  expect_message(x <- center(d$d, force = TRUE))
  expect_equal(x, c(0.4, 0.4, -0.6, -0.6, 0.4), tolerance = 1e-3, ignore_attr = TRUE)
  expect_message(x <- center(d$e))
  expect_equal(x, d$e, tolerance = 1e-3, ignore_attr = TRUE)
})
