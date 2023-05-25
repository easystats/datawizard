test_that("data_modify works", {
  data(iris)
  out <- data_modify(
    iris,
    Sepal_W_z = standardize(Sepal.Width),
    Sepal_Wz_double = 2 * Sepal_W_z
  )
  expect_equal(
    out$Sepal_W_z,
    as.vector(scale(iris$Sepal.Width)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  expect_equal(
    out$Sepal_Wz_double,
    2 * as.vector(scale(iris$Sepal.Width)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_modify works with strings", {
  data(iris)
  out <- data_modify(
    iris,
    Sepal_W_z = "standardize(Sepal.Width)",
    Sepal_Wz_double = "2 * Sepal_W_z"
  )
  expect_equal(
    out$Sepal_W_z,
    as.vector(scale(iris$Sepal.Width)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  expect_equal(
    out$Sepal_Wz_double,
    2 * as.vector(scale(iris$Sepal.Width)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_modify preserves labels", {
  data(efc)
  out <- data_modify(
    efc,
    c12hour_c = center(c12hour),
    c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)
  )
  expect_identical(
    attributes(out$c12hour_c)$label,
    attributes(efc$c12hour)$label
  )
  expect_identical(
    attributes(out$c12hour_z)$label,
    attributes(efc$c12hour)$label
  )
  out <- data_modify(
    efc,
    c12hour_c = "center(c12hour)",
    c12hour_z = "c12hour_c / sd(c12hour, na.rm = TRUE)"
  )
  expect_identical(
    attributes(out$c12hour_c)$label,
    attributes(efc$c12hour)$label
  )
  expect_identical(
    attributes(out$c12hour_z)$label,
    attributes(efc$c12hour)$label
  )
})


test_that("data_modify recycling works", {
  data(iris)
  out <- data_modify(iris, x = 1, verbose = FALSE)
  expect_equal(out$x, rep(1, nrow(iris)), ignore_attr = TRUE)
  out <- data_modify(iris, x = c(1, 2), verbose = FALSE)
  expect_equal(out$x, rep(c(1, 2), nrow(iris) / 2), ignore_attr = TRUE)
  expect_error(data_modify(iris, x = 1:4), regex = "same length")
  expect_message(data_modify(iris, x = 1), regex = "1 value")
  expect_message(data_modify(iris, x = 1:2), regex = "2 values")
})


test_that("data_modify recycling works", {
  data(iris)
  d <- data_group(iris, "Species")
  expect_message(expect_message(data_modify(d, x = 1, test = 1:2)))
})


test_that("data_modify works on grouped data", {
  data(efc)
  grouped_efc <- data_group(efc, "c172code")
  out <- data_modify(
    grouped_efc,
    c12hour_c = center(c12hour),
    c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE),
    c12hour_z2 = standardize(c12hour)
  )
  out2 <- lapply(by(efc["c12hour"], efc$c172code, scale), as.vector)
  expect_equal(
    na.omit(out$c12hour_z2[out$c172code == 1]),
    out2[[1]],
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_modify works on grouped data, with strings", {
  data(efc)
  grouped_efc <- data_group(efc, "c172code")
  out <- data_modify(
    grouped_efc,
    c12hour_c = center(c12hour),
    c12hour_z = "c12hour_c / sd(c12hour, na.rm = TRUE)",
    c12hour_z2 = "standardize(c12hour)"
  )
  out2 <- lapply(by(efc["c12hour"], efc$c172code, scale), as.vector)
  expect_equal(
    na.omit(out$c12hour_z2[out$c172code == 1]),
    out2[[1]],
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  expect_equal(
    na.omit(out$c12hour_z[out$c172code == 1]),
    out2[[1]],
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_modify errors for non df", {
  expect_error(data_modify(iris$Sepal.Length, Sepal_W_z = standardize(Sepal.Width)))
})
