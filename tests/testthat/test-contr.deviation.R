test_that("contr.deviation", {
  c.treatment <- solve(cbind(Intercept = 1, contr.treatment(3)))
  c.sum <- solve(cbind(Intercept = 1, contr.sum(3)))
  c.deviation <- solve(cbind(Intercept = 1, contr.deviation(3)))

  expect_equal(c.deviation[1, ], c.sum[1, ])
  expect_equal(c.deviation[-1, ], c.treatment[-1, ])
})

test_that("contr.deviation | snapshot", {
  skip_if_not_installed("base", "4.3")
  # IF THIS TESTS FAILS, UPDATE THE EXAMPLE

  data("mtcars")
  mtcars <- data_modify(mtcars, cyl = factor(cyl))
  mtcars <- data_modify(mtcars, am = factor(am))
  mtcars <- data_arrange(mtcars, select = c("cyl", "am"))

  contrasts(mtcars$cyl) <- contr.deviation
  c.deviation <- cbind(Intercept = 1, contrasts(mtcars$cyl))
  expect_snapshot(solve(c.deviation))

  mm <- unique(model.matrix(~ cyl * am, data = mtcars))
  rownames(mm) <- c(
    "cyl4.am0", "cyl4.am1", "cyl6.am0",
    "cyl6.am1", "cyl8.am0", "cyl8.am1"
  )

  expect_snapshot(solve(mm))
})
