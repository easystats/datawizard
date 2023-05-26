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
    "Sepal_W_z = standardize(Sepal.Width)"
  )
  expect_equal(
    out$Sepal_W_z,
    as.vector(scale(iris$Sepal.Width)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  out <- data_modify(
    iris,
    c(
      "Sepal_W_z = standardize(Sepal.Width)",
      "Sepal_Wz_double = 2 * Sepal_W_z"
    )
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
    c(
      "c12hour_c = center(c12hour)",
      "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)"
    )
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
  out <- data_modify(iris, x = "a", verbose = FALSE)
  expect_equal(out$x, rep("a", nrow(iris)), ignore_attr = TRUE)
})


test_that("data_modify recycling works", {
  data(iris)
  d <- data_group(iris, "Species")
  expect_message(expect_message(data_modify(d, x = 1, test = 1:2)))
})


test_that("data_modify expression in character vector", {
  data(iris)
  x <- "var_a = Sepal.Width"
  out <- data_modify(iris, x)
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "var_a"
    )
  )
})


test_that("data_modify expression in character vector", {
  data(iris)
  foo <- function(data) {
    y <- "var_a = Sepal.Width"
    head(data_modify(data, y))
  }
  out <- foo(iris)
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "var_a"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)

  foo2 <- function(data, z) {
    head(data_modify(data, z))
  }
  out <- foo2(iris, "var_a = Sepal.Width")
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "var_a"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)
})


test_that("data_modify expression in character vector", {
  data(iris)
  aa <- "2 * Sepal.Width"
  out <- data_modify(iris, new_var = aa)
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "new_var"
    )
  )
  expect_identical(out$new_var, 2 * out$Sepal.Width)

  foo_nv <- function(data, z) {
    head(data_modify(data, new_var = z))
  }
  out <- foo_nv(iris, "2 * Sepal.Width")
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "new_var"
    )
  )
  expect_identical(out$new_var, 2 * out$Sepal.Width)
})


test_that("data_modify expression as character vector or list", {
  data(iris)
  x <- "var_a = Sepal.Width"
  y <- "Sepal_Wz_double = 2 * var_a"
  out <- data_modify(iris, c(x, y))
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "var_a", "Sepal_Wz_double"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)
  expect_identical(out$Sepal_Wz_double, 2 * out$Sepal.Width)

  out <- data_modify(iris, list("var_a = Sepal.Width", "Sepal_Wz_double = 2 * var_a"))
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "var_a", "Sepal_Wz_double"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)
  expect_identical(out$Sepal_Wz_double, 2 * out$Sepal.Width)

  foo1 <- function(data) {
    x1 <- "var_a = Sepal.Width"
    y1 <- "Sepal_Wz_double = 2 * var_a"
    data_modify(iris, c(x1, y1))
  }
  out <- foo1(iris)
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "var_a", "Sepal_Wz_double"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)
  expect_identical(out$Sepal_Wz_double, 2 * out$Sepal.Width)

  foo2 <- function(data, z3) {
    data_modify(data, z3)
  }
  out <- foo2(iris, c("var_a = Sepal.Width", "Sepal_Wz_double = 2 * var_a"))
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "var_a", "Sepal_Wz_double"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)
  expect_identical(out$Sepal_Wz_double, 2 * out$Sepal.Width)
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


test_that("data_modify works on grouped data, with character vectors", {
  data(efc)
  grouped_efc <- data_group(efc, "c172code")
  out <- data_modify(
    grouped_efc,
    c(
      "c12hour_c = center(c12hour)",
      "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)",
      "c12hour_z2 = standardize(c12hour)"
    )
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


test_that("data_modify works on grouped data, inside functions", {
  data(efc)
  foo4 <- function(data) {
    data_modify(
      data,
      c(
        "c12hour_c = center(c12hour)",
        "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)",
        "c12hour_z2 = standardize(c12hour)"
      )
    )
  }
  out <- foo4(data_group(efc, "c172code"))
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

  foo5 <- function(data, rec) {
    data_modify(data, rec)
  }
  out <- foo5(
    data_group(efc, "c172code"),
    list(
      "c12hour_c = center(c12hour)",
      "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)",
      "c12hour_z2 = standardize(c12hour)"
    )
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
