test_that("data_replicate: simple use case", {
  data(mtcars)
  d <- head(mtcars)
  out <- data_replicate(d, "carb")
  expect_identical(dim(out), c(13L, 10L))
  expect_identical(out$disp, c(160, 160, 160, 160, 160, 160, 160, 160, 108, 258, 360, 360, 225))
  expect_named(out, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear"))

  out <- data_replicate(d, 11)
  expect_identical(dim(out), c(13L, 10L))
  expect_identical(out$disp, c(160, 160, 160, 160, 160, 160, 160, 160, 108, 258, 360, 360, 225))
  expect_named(out, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear"))

  d$mpg[5] <- NA
  out <- data_replicate(d, "carb")
  expect_identical(dim(out), c(13L, 10L))
  expect_identical(out$mpg, c(21, 21, 21, 21, 21, 21, 21, 21, 22.8, 21.4, NA, NA, 18.1))
  expect_named(out, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear"))

  d$carb[3] <- NA
  out <- data_replicate(d, "carb", remove_na = TRUE)
  expect_identical(dim(out), c(12L, 10L))
  expect_identical(out$mpg, c(21, 21, 21, 21, 21, 21, 21, 21, 21.4, NA, NA, 18.1))
  expect_named(out, c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear"))

  out <- data_replicate(d, "carb", select = c("disp", "hp"), remove_na = TRUE)
  expect_identical(dim(out), c(12L, 2L))
  expect_identical(out$disp, c(160, 160, 160, 160, 160, 160, 160, 160, 258, 360, 360, 225))
  expect_named(out, c("disp", "hp"))

  d <- data.frame(
    a = c("a", "b", "c"),
    b = 1:3,
    rep = c(3, 2, 4),
    stringsAsFactors = FALSE
  )
  out <- data_replicate(d, "rep")
  expect_identical(out$a, c("a", "a", "a", "b", "b", "c", "c", "c", "c"))
})


test_that("data_replicate: errors", {
  data(mtcars)
  d <- head(mtcars)
  expect_error(data_replicate(d), regex = "No column")
  expect_error(data_replicate(d, expand = c("mpg", "gear")), regex = "a single string")
  expect_error(data_replicate(d, expand = "geas"), regex = "The column provided")
  expect_error(data_replicate(d, expand = "qsec"), regex = "The column provided")
  d$carb[3] <- NA
  expect_error(data_replicate(d, "carb"), regex = "missing values")
  d <- head(mtcars)
  d$carb[3] <- Inf
  expect_error(data_replicate(d, "carb"), regex = "infinite values")
})
