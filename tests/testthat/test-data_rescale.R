test_that("rescale works as expected", {
  expect_equal(
    rescale(c(0, 1, 5, -5, -2), to = NULL),
    c(0, 1, 5, -5, -2),
    ignore_attr = TRUE
  )

  expect_equal(
    rescale(rep(NA_real_, 3)),
    rep(NA_real_, 3),
    ignore_attr = TRUE
  )

  expect_message(rescale(iris$Species))

  expect_equal(
    rescale(c(0, 1, 5, -5, -2)),
    c(50, 60, 100, 0, 30),
    ignore_attr = TRUE
  )

  expect_equal(
    rescale(c(0, 1, 5, -5, -2), to = c(-5, 5)),
    c(0, 1, 5, -5, -2),
    ignore_attr = TRUE
  )

  expect_equal(
    rescale(c(1, 3, 4), to = c(0, 40), range = c(0, 4)),
    c(10, 30, 40),
    ignore_attr = TRUE
  )

  expect_snapshot(head(rescale(iris, to = c(0, 1))))

  expect_snapshot(head(rescale(iris, to = c(0, 1), select = "Sepal.Length")))

  expect_snapshot(
    head(rescale(iris, to = list(
      Sepal.Length = c(0, 1),
      Petal.Length = c(-1, 0)
    )))
  )
})


test_that("rescale works with select helpers", {
  out <- rescale(iris, to = c(0, 1), select = c("Sepal.Width", "Sepal.Length"))
  expect_equal(head(out$Sepal.Width), c(0.625, 0.41667, 0.5, 0.45833, 0.66667, 0.79167), tolerance = 1e-3)
  expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)

  # check class attributes
  expect_identical(
    vapply(out, class, character(1)),
    c(
      Sepal.Length = "numeric", Sepal.Width = "numeric", Petal.Length = "numeric",
      Petal.Width = "numeric", Species = "factor"
    )
  )

  out <- rescale(iris, to = c(0, 1), select = starts_with("Sepal"))
  expect_equal(head(out$Sepal.Width), c(0.625, 0.41667, 0.5, 0.45833, 0.66667, 0.79167), tolerance = 1e-3)
  expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)

  skip_if_not_installed("poorman")

  x <- poorman::group_by(iris, Species)
  out <- rescale(x, to = c(0, 1), select = starts_with("Sepal"))
  expect_equal(head(out$Sepal.Width), c(0.57143, 0.33333, 0.42857, 0.38095, 0.61905, 0.7619), tolerance = 1e-3)
  expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)
})


# grouped df ------------------------------
test_that("rescale works grouped df and append", {
  out <- rescale(iris, to = c(0, 1), select = c("Sepal.Width", "Sepal.Length"), append = TRUE)
  expect_equal(head(out$Sepal.Width_r), c(0.625, 0.41667, 0.5, 0.45833, 0.66667, 0.79167), tolerance = 1e-3)
  expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "Sepal.Width_r", "Sepal.Length_r"
    )
  )

  skip_if_not_installed("poorman")

  x <- poorman::group_by(iris, Species)
  out <- rescale(x, to = c(0, 1), select = starts_with("Sepal"), append = TRUE)
  expect_equal(head(out$Sepal.Width_r), c(0.57143, 0.33333, 0.42857, 0.38095, 0.61905, 0.7619), tolerance = 1e-3)
  expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "Sepal.Length_r", "Sepal.Width_r"
    )
  )
})


# select helpers ------------------------------
test_that("data_rescale regex", {
  expect_equal(
    rescale(mtcars, select = "pg", regex = TRUE)$mpg,
    rescale(mtcars, select = "mpg")$mpg,
    ignore_attr = TRUE
  )
})


# expanding range ------------------------------
test_that("data_rescale can expand range", {
  # for vectors
  x <- 5:15
  expect_equal(
    rescale(x, multiply = 1.1),
    c(4.5, 5.6, 6.7, 7.8, 8.9, 10, 11.1, 12.2, 13.3, 14.4, 15.5),
    ignore_attr = TRUE
  )
  expect_equal(rescale(x, multiply = 1.1), rescale(x, add = 0.5), ignore_attr = TRUE)
  expect_error(rescale(x, multiply = 0.9, add = 1), regex = "Only one of")
  expect_error(rescale(x, multiply = c(1.2, 1.4)), regex = "The length of")

  # different values for add
  expect_equal(
    rescale(x, add = c(1, 3)),
    c(4, 5.4, 6.8, 8.2, 9.6, 11, 12.4, 13.8, 15.2, 16.6, 18),
    ignore_attr = TRUE
  )
  expect_error(rescale(x, add = 1:3), regex = "The length of")

  # works with NA
  expect_equal(
    rescale(rep(NA_real_, 3), multiply = 1.1),
    rep(NA_real_, 3),
    ignore_attr = TRUE
  )
  expect_equal(
    rescale(rep(NA_real_, 3), add = 2),
    rep(NA_real_, 3),
    ignore_attr = TRUE
  )

  # for data frames
  d <- data.frame(x = 5:15, y = 5:15)
  expect_equal(
    rescale(d, multiply = 1.1),
    rescale(d, add = 0.5),
    ignore_attr = TRUE
  )
  expect_equal(
    rescale(d, multiply = list(x = 1.1, y = 0.5)),
    rescale(d, add = list(x = 0.5, y = -2.5)),
    ignore_attr = TRUE
  )
  # data frames accept multiple add-values per column
  out <- rescale(d, add = list(x = c(1, 3), y = c(2, 4)))
  expect_equal(
    out$x,
    rescale(d$x, add = c(1, 3)),
    ignore_attr = TRUE
  )
  expect_equal(
    out$y,
    rescale(d$y, add = c(2, 4)),
    ignore_attr = TRUE
  )

  expect_error(rescale(d, multiply = 0.9, add = 1), regex = "Only one of")
  expect_error(rescale(d, multiply = list(x = 0.9, y = 2), add = list(y = 1)), regex = "Only one of")
  expect_error(rescale(d, multiply = c(0.9, 1.5)), regex = "The length of")
})
