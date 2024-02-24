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
  out <- data_modify(iris, x = 1)
  expect_equal(out$x, rep(1, nrow(iris)), ignore_attr = TRUE)
  out <- data_modify(iris, x = c(1, 2))
  expect_equal(out$x, rep(c(1, 2), nrow(iris) / 2), ignore_attr = TRUE)
  expect_error(data_modify(iris, x = 1:4), regex = "same length")
  out <- data_modify(iris, x = "a")
  expect_equal(out$x, rep("a", nrow(iris)), ignore_attr = TRUE)
})


test_that("data_modify recycling works", {
  data(iris)
  d <- data_group(iris, "Species")
  expect_silent(data_modify(d, x = 1, test = 1:2))
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


test_that("data_modify expression as character vector", {
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

  # works with separated strings
  data(iris)
  out <- data_modify(iris, "var_a = Sepal.Width", "Sepal_Wz_double = 2 * var_a")
  expect_identical(out$var_a, out$Sepal.Width)
  expect_identical(out$Sepal_Wz_double, 2 * out$Sepal.Width)
})


test_that("data_modify remove variables with NULL", {
  data(iris)
  out <- data_modify(iris, PL_new = 2 * Petal.Length, Petal.Length = NULL)
  expect_identical(colnames(out), c("Sepal.Length", "Sepal.Width", "Petal.Width", "Species", "PL_new"))
  expect_identical(out$PL_new, 2 * iris$Petal.Length)
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


test_that("data_modify works on grouped data, preserves attributes and labels", {
  data(efc)
  grouped_efc <- data_group(efc, "c172code")
  out <- data_modify(
    grouped_efc,
    c12hour_c = center(c12hour)
  )
  expect_identical(
    attributes(out$c12hour)$label,
    attributes(efc$c12hour)$label
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


test_that("data_modify errors for non df", {
  expect_error(data_modify(iris$Sepal.Length, Sepal_W_z = standardize(Sepal.Width)))
})


test_that("data_modify errors for non df", {
  data(efc)
  a <- "center(c22hour)" # <---------------- error in variable name
  b <- "c12hour_c / sd(c12hour, na.rm = TRUE)"
  expect_error(
    data_modify(efc, c12hour_c = a, c12hour_z = b),
    regex = "c22hour"
  )

  a <- "center(c12hour)"
  b <- "c12hour_c / sd(c21hour, na.rm = TRUE)"
  expect_error(
    data_modify(efc, c12hour_c = a, c12hour_z = b),
    regex = "c12hour_c"
  )
  expect_error(
    data_modify(efc, c12hour_c = a, c12hour_z = b),
    regex = "second expression"
  )
})


test_that("data_modify message about recycling values", {
  expect_snapshot(head(data_modify(iris, Sepal.Width = 1)))
  expect_snapshot(head(data_modify(iris, Sepal.Width = 1:2)))
  expect_snapshot(head(data_modify(iris, Petal.Length = 1, Sepal.Width = 1)))
  expect_snapshot(head(data_modify(iris, Petal.Length = 1, Sepal.Width = 1:2)))
  expect_snapshot(head(data_modify(iris, Petal.Length = 2, Sepal.Width = 2)))
})


test_that("data_modify message about modified variables", {
  expect_snapshot(head(data_modify(iris, Sepal.Width = 2 * Sepal.Width)))
  expect_snapshot(head(data_modify(iris, Petal.Length = Sepal.Length, Sepal.Width = Petal.Width)))
})


test_that("data_modify works with character variables, and inside functions", {
  data(efc)
  a <- "center(c12hour)"
  b <- "c12hour_c / sd(c12hour, na.rm = TRUE)"
  d <- "standardize(c12hour)"
  out <- data_modify(
    efc,
    c12hour_c = a,
    c12hour_z = b,
    c12hour_z2 = d
  )
  expect_equal(
    out$c12hour_z2,
    as.vector(scale(efc$c12hour)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  expect_equal(
    out$c12hour_z,
    as.vector(scale(efc$c12hour)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  # when calling functions
  a1 <- "center(c12hour)"
  b1 <- "c12hour_c / sd(c12hour, na.rm = TRUE)"
  d1 <- "standardize(c12hour)"
  foo <- function(data, x1, x2, x3) {
    data_modify(
      efc,
      c12hour_c = x1,
      c12hour_z = x2,
      c12hour_z2 = x3
    )
  }
  out <- foo(efc, a1, b1, d1)
  expect_equal(
    out$c12hour_z2,
    as.vector(scale(efc$c12hour)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  expect_equal(
    out$c12hour_z,
    as.vector(scale(efc$c12hour)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )

  # when calling functions, arguments inside function defined
  foo2 <- function(data) {
    a2 <- "center(c12hour)"
    b2 <- "c12hour_c / sd(c12hour, na.rm = TRUE)"
    d2 <- "standardize(c12hour)"

    data_modify(
      efc,
      c12hour_c = a2,
      c12hour_z = b2,
      c12hour_z2 = d2
    )
  }
  out <- foo2(efc)
  expect_equal(
    out$c12hour_z2,
    as.vector(scale(efc$c12hour)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  expect_equal(
    out$c12hour_z,
    as.vector(scale(efc$c12hour)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
})


test_that("data_modify works with grouped df when overwriting existing variables", {
  data(iris)
  iris_grp <- data_group(iris, "Species")
  out <- data_modify(iris_grp, Sepal.Length = normalize(Sepal.Length))
  expect_equal(head(out$Sepal.Length), c(0.53333, 0.4, 0.26667, 0.2, 0.46667, 0.73333), tolerance = 1e-3)

  out <- data_modify(
    iris_grp,
    Sepal.Length = normalize(Sepal.Length),
    Sepal.Length2 = 2 * Sepal.Length
  )
  expect_equal(head(out$Sepal.Length2), 2 * c(0.53333, 0.4, 0.26667, 0.2, 0.46667, 0.73333), tolerance = 1e-3)
})


test_that("data_modify works with functions that return character vectors", {
  data(iris)
  set.seed(123)
  out <- data_modify(iris, grp = sample(letters[1:3], nrow(iris), TRUE))
  expect_identical(head(out$grp), c("c", "c", "c", "b", "c", "b"))
})


test_that("data_modify .if/.at arguments", {
  data(iris)
  d <- iris[1:5, ]
  # validate results
  out <- data_modify(d, .at = "Species", .modify = as.numeric)
  expect_identical(out$Species, c(1, 1, 1, 1, 1))
  out <- data_modify(d, .if = is.factor, .modify = as.numeric)
  expect_identical(out$Species, c(1, 1, 1, 1, 1))
  out <- data_modify(d, new_length = Petal.Length * 2, .at = "Species", .modify = as.numeric)
  expect_identical(out$Species, c(1, 1, 1, 1, 1))
  expect_named(out, c(
    "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
    "Species", "new_length"
  ))
  # .at and .if cannot be used at same timne
  expect_error(
    data_modify(d, .at = "Species", .if = is.factor, .modify = as.numeric),
    regex = "You cannot use both"
  )
  # modify must be a function
  expect_error(
    data_modify(d, .at = "Species", .modify = "a"),
    regex = "`.modify` must"
  )
  # unknown variable
  expect_error(
    data_modify(d, .at = c("Species", "Test"), .modify = as.numeric),
    regex = "Variable \"Test\""
  )
  # unknown variables
  expect_error(
    data_modify(d, .at = c("Species", "Hi", "Test"), .modify = as.numeric),
    regex = "Variables \"Hi\" and \"Test\""
  )
  # one of .at or .if must be specified
  expect_error(
    data_modify(d, .modify = as.numeric),
    regex = "You need to specify"
  )
  # function not applicable to factors
  expect_error(
    data_modify(d, .at = "Species", .modify = function(x) 2 / y + x),
    regex = "Error in modifying variable"
  )
  # function not applicable to factors
  expect_error(
    data_modify(d, .at = "Species", .modify = function(x) 2 * x),
    regex = "Error in modifying variable"
  )
  # .modify needs to be specified
  expect_error(
    data_modify(d, .at = "Species", .if = is.factor),
    regex = "You need to specify"
  )
  # newly created variables are processed by if/at
  out <- data_modify(d, new_length = Petal.Length * 2, .if = is.numeric, .modify = round)
  expect_equal(out$new_length, c(3, 3, 3, 3, 3), ignore_attr = TRUE)
})
