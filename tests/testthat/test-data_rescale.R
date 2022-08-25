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
      "Sepal.Length" = c(0, 1),
      "Petal.Length" = c(-1, 0)
    )))
  )
})



test_that("rescale works with select helpers", {
  out <- rescale(iris, to = c(0, 1), select = c("Sepal.Width", "Sepal.Length"))
  expect_equal(head(out$Sepal.Width), c(0.625, 0.41667, 0.5, 0.45833, 0.66667, 0.79167), tolerance = 1e-3)
  expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)

  out <- rescale(iris, to = c(0, 1), select = starts_with("Sepal"))
  expect_equal(head(out$Sepal.Width), c(0.625, 0.41667, 0.5, 0.45833, 0.66667, 0.79167), tolerance = 1e-3)
  expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)

  if (require("poorman")) {
    x <- poorman::group_by(iris, Species)
    out <- rescale(x, to = c(0, 1), select = starts_with("Sepal"))
    expect_equal(head(out$Sepal.Width), c(0.57143, 0.33333, 0.42857, 0.38095, 0.61905, 0.7619), tolerance = 1e-3)
    expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)
  }
})

# select helpers ------------------------------
test_that("data_rescale regex", {
  expect_equal(
    rescale(mtcars, select = "pg", regex = TRUE)$mpg,
    rescale(mtcars, select = "mpg")$mpg
  )
})
