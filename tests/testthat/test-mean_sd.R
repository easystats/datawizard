test_that("mean_sd", {
  x <- c(-1, 0, 1)
  msd <- mean_sd(x)
  expect_identical(unname(msd), x)
  expect_named(msd, c("-SD", "Mean", "+SD"))

  msd <- mean_sd(mtcars[["mpg"]])
  mmad <- median_mad(mtcars[["mpg"]])
  expect_identical(unname(msd), mean(mtcars[["mpg"]]) + c(-1, 0, 1) * sd(mtcars[["mpg"]]))
  expect_identical(unname(mmad), median(mtcars[["mpg"]]) + c(-1, 0, 1) * mad(mtcars[["mpg"]]))

  msd2 <- mean_sd(mtcars[["mpg"]], times = 3L)
  expect_length(msd2, n = 3 * 2 + 1)
  expect_identical(unname(msd2[3:5]), unname(msd))
  expect_equal(unname(diff(msd2)), rep(sd(mtcars[["mpg"]]), 6), tolerance = 0.00001)
  expect_named(msd2, c("-3 SD", "-2 SD", "-1 SD", "Mean", "+1 SD", "+2 SD", "+3 SD"))
})
