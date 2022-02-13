test_that("data_rescale works as expected", {
  expect_equal(
    data_rescale(c(0, 1, 5, -5, -2), to = NULL),
    c(0, 1, 5, -5, -2)
  )
  expect_equal(
    data_rescale(rep(NA_real_, 3)),
    rep(NA_real_, 3)
  )

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
