test_that("data_expand: simple use case", {
  data(mtcars)
  d <- as.data.frame(head(mtcars))
  out <- data_expand(d, "carb")
  expect_identical(dim(out), c(13L, 10L))
  expect_identical(out$disp, c(160, 160, 160, 160, 160, 160, 160, 160, 108, 258, 360, 360, 225))
  expect_named(out, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear"))

  d$mpg[5] <- NA
  out <- data_expand(d, "carb")
  expect_identical(dim(out), c(13L, 10L))
  expect_identical(out$mpg, c(21, 21, 21, 21, 21, 21, 21, 21, 22.8, 21.4, NA, NA, 18.1))
  expect_named(out, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear"))

  d$carb[3] <- NA
  out <- data_expand(d, "carb", remove_na = TRUE)
  expect_identical(dim(out), c(12L, 10L))
  expect_identical(out$mpg, c(21, 21, 21, 21, 21, 21, 21, 21, 21.4, NA, NA, 18.1))
  expect_named(out, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear"))

  out <- data_expand(d, "carb", select = c("disp", "hp"), remove_na = TRUE)
  expect_identical(dim(out), c(12L, 2L))
  expect_identical(out$disp, c(160, 160, 160, 160, 160, 160, 160, 160, 258, 360, 360, 225))
  expect_named(out, c("disp", "hp"))
})

test_that("data_expand: errors", {
  data(mtcars)
  d <- as.data.frame(head(mtcars))
  expect_error(data_expand(d), regex = "No column")
  expect_error(data_expand(d, expand = c("mpg", "gear")), regex = "a single string")
  expect_error(data_expand(d, expand = "geas"), regex = "The column provided")
  d$carb[3] <- NA
  expect_error(data_expand(d, "carb"), regex = "missing values")
})
