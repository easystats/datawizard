test_that("data_rescale works as expected", {
  expect_equal(
    data_rescale(c(0, 1, 5, -5, -2), to = NULL),
    c(0, 1, 5, -5, -2)
  )

  expect_equal(
    data_rescale(rep(NA_real_, 3)),
    rep(NA_real_, 3)
  )

  expect_message(data_rescale(iris$Species))

  expect_equal(
    data_rescale(c(0, 1, 5, -5, -2)),
    c(50, 60, 100, 0, 30)
  )

  expect_equal(
    data_rescale(c(0, 1, 5, -5, -2), to = c(-5, 5)),
    c(0, 1, 5, -5, -2)
  )

  expect_equal(
    data_rescale(c(1, 3, 4), to = c(0, 40), range = c(0, 4)),
    c(10, 30, 40)
  )

  expect_snapshot(head(data_rescale(iris, to = c(0, 1))))

  expect_snapshot(head(data_rescale(iris, to = c(0, 1), select = "Sepal.Length")))

  expect_snapshot(
    head(data_rescale(iris, to = list(
      "Sepal.Length" = c(0, 1),
      "Petal.Length" = c(-1, 0)
    )))
  )
})



test_that("data_rescale works with select helpers", {
  out <- data_rescale(iris, to = c(0, 1), select = c("Sepal.Width", "Sepal.Length"))
  expect_equal(head(out$Sepal.Width), c(0.625, 0.41667, 0.5, 0.45833, 0.66667, 0.79167), tolerance = 1e-3)
  expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)

  out <- data_rescale(iris, to = c(0, 1), select = starts_with("Sepal"))
  expect_equal(head(out$Sepal.Width), c(0.625, 0.41667, 0.5, 0.45833, 0.66667, 0.79167), tolerance = 1e-3)
  expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)

  if (require("poorman")) {
    x <- poorman::group_by(iris, Species)
    out <- data_rescale(x, to = c(0, 1), select = starts_with("Sepal"))
    expect_equal(head(out$Sepal.Width), c(0.57143, 0.33333, 0.42857, 0.38095, 0.61905, 0.7619), tolerance = 1e-3)
    expect_equal(head(out$Petal.Length), head(iris$Petal.Length), tolerance = 1e-3)
  }
})
