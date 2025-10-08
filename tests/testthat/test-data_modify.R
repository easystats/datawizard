## styler: off

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
    as_expr("Sepal_W_z = standardize(Sepal.Width)")
  )
  expect_equal(
    out$Sepal_W_z,
    as.vector(scale(iris$Sepal.Width)),
    ignore_attr = TRUE,
    tolerance = 1e-3
  )
  out <- data_modify(
    iris,
    as_expr(c(
      "Sepal_W_z = standardize(Sepal.Width)",
      "Sepal_Wz_double = 2 * Sepal_W_z"
    ))
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
    as_expr(c(
      "c12hour_c = center(c12hour)",
      "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)"
    ))
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


test_that("data_modify recycling works with grouped df", {
  data(iris)
  d <- data_group(iris, "Species")
  expect_silent(data_modify(d, x = 1, test = 1:2))
})


test_that("data_modify expression in character vector-1", {
  data(iris)
  x <- "var_a = Sepal.Width"
  out <- data_modify(iris, as_expr(x))
  expect_named(
    out,
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "var_a"
    )
  )
})


test_that("data_modify expression in character vector-2", {
  data(iris)
  foo <- function(data) {
    y <- "var_a = Sepal.Width"
    head(data_modify(data, as_expr(y)))
  }
  out <- foo(iris)
  expect_named(
    out,
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "var_a"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)

  foo2 <- function(data, z) {
    head(data_modify(data, as_expr(z)))
  }
  out <- foo2(iris, "var_a = Sepal.Width")
  expect_named(
    out,
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "var_a"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)
})


test_that("data_modify expression in character vector-3", {
  data(iris)
  aa <- "2 * Sepal.Width"
  out <- data_modify(iris, new_var = as_expr(aa))
  expect_named(
    out,
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "new_var"
    )
  )
  expect_identical(out$new_var, 2 * out$Sepal.Width)

  aa <- "2 * Sepal.Width"
  out <- data_modify(iris, new_var = as_expr(aa))
  expect_named(
    out,
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "new_var"
    )
  )
  expect_identical(out$new_var, 2 * out$Sepal.Width)

  foo_nv <- function(data, z) {
    head(data_modify(data, new_var = as_expr(z)))
  }
  out <- foo_nv(iris, "2 * Sepal.Width")
  expect_identical(
    colnames(out),
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "new_var"
    )
  )
  expect_identical(out$new_var, 2 * out$Sepal.Width)
})


test_that("data_modify expression as character vector-4", {
  data(iris)
  x <- "var_a = Sepal.Width"
  y <- "Sepal_Wz_double = 2 * var_a"
  out <- data_modify(iris, as_expr(c(x, y)))
  expect_named(
    out,
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "var_a",
      "Sepal_Wz_double"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)
  expect_identical(out$Sepal_Wz_double, 2 * out$Sepal.Width)

  foo1 <- function(data) {
    x1 <- "var_a = Sepal.Width"
    y1 <- "Sepal_Wz_double = 2 * var_a"
    combined <- c(x1, y1)
    data_modify(iris, as_expr(combined))
  }
  out <- foo1(iris)
  expect_named(
    out,
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "var_a",
      "Sepal_Wz_double"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)
  expect_identical(out$Sepal_Wz_double, 2 * out$Sepal.Width)

  foo2 <- function(data, z3) {
    data_modify(data, as_expr(z3))
  }
  out <- foo2(iris, c("var_a = Sepal.Width", "Sepal_Wz_double = 2 * var_a"))
  expect_named(
    out,
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "var_a",
      "Sepal_Wz_double"
    )
  )
  expect_identical(out$var_a, out$Sepal.Width)
  expect_identical(out$Sepal_Wz_double, 2 * out$Sepal.Width)

  # works with separated strings
  data(iris)
  out <- data_modify(
    iris,
    as_expr("var_a = Sepal.Width"),
    as_expr("Sepal_Wz_double = 2 * var_a")
  )
  expect_identical(out$var_a, out$Sepal.Width)
  expect_identical(out$Sepal_Wz_double, 2 * out$Sepal.Width)

  out <- data_modify(
    iris,
    as_expr(c("var_a = Sepal.Width", "Sepal_Wz_double = 2 * var_a"))
  )
  expect_identical(out$var_a, out$Sepal.Width)
  expect_identical(out$Sepal_Wz_double, 2 * out$Sepal.Width)
})


test_that("data_modify works with function as expression", {
  data(iris)
  out <- data_modify(iris, foo = grepl("a", Species)) # nolint
  expect_identical(out$foo, rep(c(TRUE, FALSE, TRUE), each = 50))
  out <- data_modify(iris, foo = as_expr("grepl(\"a\", Species)"))
  expect_identical(out$foo, rep(c(TRUE, FALSE, TRUE), each = 50))
  out <- data_modify(iris, as_expr("foo = grepl(\"a\", Species)"))
  expect_identical(out$foo, rep(c(TRUE, FALSE, TRUE), each = 50))
  out <- data_modify(iris, foo = as_expr("grepl('a', Species)"))
  expect_identical(out$foo, rep(c(TRUE, FALSE, TRUE), each = 50))
  out <- data_modify(iris, as_expr("foo = grepl('a', Species)"))
  expect_identical(out$foo, rep(c(TRUE, FALSE, TRUE), each = 50))
  out <- data_modify(iris, foo = as_expr('grepl(\'a\', Species)')) # nolint
  expect_identical(out$foo, rep(c(TRUE, FALSE, TRUE), each = 50))
  out <- data_modify(iris, as_expr('foo = grepl(\'a\', Species)')) # nolint
  expect_identical(out$foo, rep(c(TRUE, FALSE, TRUE), each = 50))
  out <- data_modify(iris, foo = as_expr('grepl(\"a\", Species)'))
  expect_identical(out$foo, rep(c(TRUE, FALSE, TRUE), each = 50))
  out <- data_modify(iris, as_expr('foo = grepl(\"a\", Species)'))
  expect_identical(out$foo, rep(c(TRUE, FALSE, TRUE), each = 50))
})


test_that("data_modify remove variables with NULL", {
  data(iris)
  out <- data_modify(iris, PL_new = 2 * Petal.Length, Petal.Length = NULL)
  expect_named(
    out,
    c("Sepal.Length", "Sepal.Width", "Petal.Width", "Species", "PL_new")
  )
  expect_identical(out$PL_new, 2 * iris$Petal.Length)

  out <- data_modify(iris, as_expr("Species = NULL"))
  expect_named(
    out,
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )
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
    as_expr(c(
      "c12hour_c = center(c12hour)",
      "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)",
      "c12hour_z2 = standardize(c12hour)"
    ))
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
      as_expr(c(
        "c12hour_c = center(c12hour)",
        "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)",
        "c12hour_z2 = standardize(c12hour)"
      ))
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
    data_modify(data, as_expr(rec))
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
  expect_error(data_modify(
    iris$Sepal.Length,
    Sepal_W_z = standardize(Sepal.Width)
  ))
})


test_that("data_modify errors for empty data frames", {
  data(mtcars)
  x <- mtcars[1, ]
  expect_error(
    data_modify(x[-1, ], new_var = 5),
    regex = "empty data frame"
  )
})


test_that("data_modify errors for typos", {
  data(efc)
  a <- "center(c22hour)" # <---------------- error in variable name
  b <- "c12hour_c / sd(c12hour, na.rm = TRUE)"
  expect_error(
    data_modify(efc, c12hour_c = as_expr(a), c12hour_z = as_expr(b)),
    regex = "c22hour"
  )

  a <- "center(c12hour)"
  b <- "c12hour_c / sd(c21hour, na.rm = TRUE)" # <------ error in variable name
  expect_error(
    data_modify(efc, c12hour_c = as_expr(a), c12hour_z = as_expr(b)),
    regex = "c12hour_c"
  )

  expect_error(
    data_modify(efc, c12hour_c = as_expr(a), c12hour_z = as_expr(b)),
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
  expect_snapshot(head(data_modify(
    iris,
    Petal.Length = Sepal.Length,
    Sepal.Width = Petal.Width
  )))
})


test_that("data_modify works with character variables, and inside functions", {
  data(efc)
  a <- "center(c12hour)"
  b <- "c12hour_c / sd(c12hour, na.rm = TRUE)"
  d <- "standardize(c12hour)"
  out <- data_modify(
    efc,
    c12hour_c = as_expr(a),
    c12hour_z = as_expr(b),
    c12hour_z2 = as_expr(d)
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
      c12hour_c = as_expr(x1),
      c12hour_z = as_expr(x2),
      c12hour_z2 = as_expr(x3)
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
      c12hour_c = as_expr(a2),
      c12hour_z = as_expr(b2),
      c12hour_z2 = as_expr(d2)
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
  expect_equal(
    head(out$Sepal.Length),
    c(0.53333, 0.4, 0.26667, 0.2, 0.46667, 0.73333),
    tolerance = 1e-3
  )

  out <- data_modify(
    iris_grp,
    Sepal.Length = normalize(Sepal.Length),
    Sepal.Length2 = 2 * Sepal.Length
  )
  expect_equal(
    head(out$Sepal.Length2),
    2 * c(0.53333, 0.4, 0.26667, 0.2, 0.46667, 0.73333),
    tolerance = 1e-3
  )
})


test_that("data_modify works with functions that return character vectors", {
  data(iris)
  set.seed(123)
  out <- data_modify(iris, grp = sample(letters[1:3], nrow(iris), TRUE))
  expect_identical(head(out$grp), c("a", "c", "b", "a", "c", "c"))
})


test_that("data_modify 1:n() and similar works in (grouped) data frames", {
  data(mtcars)
  out <- data_modify(mtcars, Trials = 1:n()) # nolint
  expect_identical(out$Trials, 1:32)
  x <- data_group(mtcars, "gear")
  out <- data_modify(x, Trials = 1:n()) # nolint
  expect_identical(out$Trials[out$gear == 3], 1:15)
  expect_identical(out$Trials[out$gear == 4], 1:12)
  out <- data_modify(x, Trials = 3:(n() + 2))
  expect_identical(out$Trials[out$gear == 3], 3:17)
  expect_identical(out$Trials[out$gear == 4], 3:14)
})


test_that("data_modify .if/.at arguments", {
  data(iris)
  d <- iris[1:5, ]
  # validate results
  out <- data_modify(d, .at = "Species", .modify = as.numeric)
  expect_identical(out$Species, c(1, 1, 1, 1, 1))
  out <- data_modify(d, .if = is.factor, .modify = as.numeric)
  expect_identical(out$Species, c(1, 1, 1, 1, 1))
  out <- data_modify(
    d,
    new_length = Petal.Length * 2,
    .at = "Species",
    .modify = as.numeric
  )
  expect_identical(out$Species, c(1, 1, 1, 1, 1))
  expect_named(
    out,
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "new_length"
    )
  )
  # using other functions with `.at`
  out <- data_modify(
    d,
    .at = extract_column_names(d, select = starts_with("Sepal")),
    .modify = as.factor
  )
  expect_s3_class(out$Sepal.Length, "factor")
  expect_s3_class(out$Sepal.Width, "factor")

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
  out <- data_modify(
    d,
    new_length = Petal.Length * 2,
    .if = is.numeric,
    .modify = round
  )
  expect_equal(out$new_length, c(3, 3, 3, 3, 3), ignore_attr = TRUE)
})


test_that("data_modify works with new expressions, different use cases same results", {
  data(iris)
  out1 <- data_modify(iris, as_expr("sepwid = 2 * Sepal.Width"))
  out2 <- data_modify(iris, sepwid = as_expr("2 * Sepal.Width"))
  e <- "sepwid = 2 * Sepal.Width"
  out3 <- data_modify(iris, as_expr(e))
  e <- "2 * Sepal.Width"
  out4 <- data_modify(iris, sepwid = as_expr(e))

  expect_equal(head(out1), head(out2), ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(head(out1), head(out3), ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(head(out1), head(out4), ignore_attr = TRUE, tolerance = 1e-4)

  out1b <- data_modify(
    iris,
    as_expr(c("sepwid = 2 * Sepal.Width", "seplen = 5 * Sepal.Length"))
  )
  out2b <- data_modify(
    iris,
    sepwid = as_expr("2 * Sepal.Width"),
    seplen = as_expr("5 * Sepal.Length")
  )
  e <- c("sepwid = 2 * Sepal.Width", "seplen = 5 * Sepal.Length")
  out3b <- data_modify(iris, as_expr(e))
  e <- "2 * Sepal.Width"
  out4b <- data_modify(iris, sepwid = as_expr(e), seplen = 5 * Sepal.Length)

  expect_equal(head(out1b), head(out2b), ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(head(out1b), head(out3b), ignore_attr = TRUE, tolerance = 1e-4)
  expect_equal(head(out1b), head(out4b), ignore_attr = TRUE, tolerance = 1e-4)

  # no expression
  out <- data_modify(iris, sepwid = "2 * Sepal.Widht")
  expect_identical(
    head(out$sepwid),
    c(
      "2 * Sepal.Widht",
      "2 * Sepal.Widht",
      "2 * Sepal.Widht",
      "2 * Sepal.Widht",
      "2 * Sepal.Widht",
      "2 * Sepal.Widht"
    )
  )

  # works with paste()
  to_standardize <- c("Petal.Length", "Sepal.Length")
  out <- data_modify(
    iris,
    as_expr(
      paste0(to_standardize, "_stand = standardize(", to_standardize, ")")
    )
  )
  expect_equal(
    head(out$Petal.Length_stand),
    c(-1.33575, -1.33575, -1.3924, -1.2791, -1.33575, -1.16581),
    tolerance = 1e-3
  )
  expect_equal(
    head(out$Sepal.Length_stand),
    c(-0.89767, -1.1392, -1.38073, -1.50149, -1.01844, -0.53538),
    tolerance = 1e-3
  )

  # complex example
  e <- "2 * Sepal.Width"
  f <- "half_petal = 0.5 * Petal.Length"
  a <- "string"
  num <- 1:5
  out_complex <- data_modify(
    iris,
    sepwid = as_expr(e),
    seplen = 5 * Sepal.Length,
    as_expr(f),
    new_var = a,
    new_num = num,
    new_var2 = "ho",
    new_num2 = 4:6,
    Sepal.Length = NULL,
    Petal.Length = NULL,
    Sepal.Width = NULL,
    Petal.Width = NULL
  )
  expect_snapshot(print(head(out_complex)))
})


test_that("data_modify works with new expressions, grouped_df, different use cases same results", {
  data(efc, package = "datawizard")
  grouped_efc <- data_group(efc, "c172code")
  new_efc1 <- data_modify(
    grouped_efc,
    c12hour_c = center(c12hour),
    c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE),
    c12hour_z2 = standardize(c12hour),
    id = 1:n() # nolint
  )

  new_efc2 <- data_modify(
    grouped_efc,
    as_expr("c12hour_c = center(c12hour)"),
    c12hour_z = as_expr("c12hour_c / sd(c12hour, na.rm = TRUE)"),
    c12hour_z2 = standardize(c12hour),
    id = 1:n() # nolint
  )
  expect_equal(
    head(new_efc1),
    head(new_efc2),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  s <- c(
    "c12hour_c = center(c12hour)",
    "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)",
    "c12hour_z2 = standardize(c12hour)"
  )
  new_efc3 <- data_modify(
    grouped_efc,
    as_expr(s),
    id = 1:n() # nolint
  )
  expect_equal(
    head(new_efc1),
    head(new_efc3),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )

  new_efc4 <- data_modify(
    grouped_efc,
    c12hour_c = center(c12hour),
    c12hour_z = as_expr("c12hour_c / sd(c12hour, na.rm = TRUE)"),
    c12hour_z2 = standardize(c12hour),
    id = 1:n() # nolint
  )
  expect_equal(
    head(new_efc1),
    head(new_efc4),
    ignore_attr = TRUE,
    tolerance = 1e-4
  )
})


test_that("data_modify errors with new expressions", {
  e <- "sepwid = 2 * Sepal.Widht"
  expect_error(
    data_modify(iris, as_expr(e)),
    regex = "in the first expression"
  )
  expect_error(
    data_modify(iris, as_expr(e)),
    regex = "Sepal.Widht"
  )

  expect_error(
    data_modify(iris, as_expr("sepwid = 2 * Sepal.Widht")),
    regex = "in the first expression"
  )
  expect_error(
    data_modify(iris, as_expr("sepwid = 2 * Sepal.Widht")),
    regex = "Sepal.Widht"
  )

  expect_error(
    data_modify(iris, sepwid = 2 * Sepal.Widht),
    regex = "in the first expression"
  )
  expect_error(
    data_modify(iris, sepwid = 2 * Sepal.Widht),
    regex = "Sepal.Widht"
  )

  expect_error(
    data_modify(iris, as_expr("2 * Sepal.Widht")),
    regex = "variable name"
  )

  e <- "2 * Sepal.Widht"
  expect_error(
    data_modify(iris, as_expr(e)),
    regex = "variable name"
  )

  data(efc, package = "datawizard")
  a <- "center(c22hour)" # <---------------- error in variable name
  b <- "c12hour_c / sd(c12hour, na.rm = TRUE)"
  expect_error(
    data_modify(efc, c12hour_c = as_expr(a), c12hour_z = as_expr(b)),
    regex = "c22hour"
  )

  expect_error(
    data_modify(iris, a = as_expr(c("1 + 1", "2 + 2"))),
    regex = "Could not evaluate expression"
  )
})


skip_if_not_installed("withr")

withr::with_environment(
  new.env(),
  test_that("data_modify 1:n() and similar works in (grouped) data frames inside function calls", {
    data(mtcars)
    x <- data_group(mtcars, "gear")

    foo <- function(d) {
      out <- data_modify(d, Trials = 1:n()) # nolint
      out$Trials
    }
    expect_identical(
      foo(x),
      c(
        1L,
        2L,
        3L,
        1L,
        2L,
        3L,
        4L,
        4L,
        5L,
        6L,
        7L,
        5L,
        6L,
        7L,
        8L,
        9L,
        10L,
        8L,
        9L,
        10L,
        11L,
        12L,
        13L,
        14L,
        15L,
        11L,
        1L,
        2L,
        3L,
        4L,
        5L,
        12L
      )
    )
  })
)

test_that("data_modify errors on non-defined function", {
  expect_error(data_modify(iris, Species = foo()))
})


withr::with_environment(
  new.env(),
  test_that("data_modify correctly assigns values from variables", {
    d <- data.frame()
    for (param in letters[c(1, 2, 5)]) {
      out <- data.frame(x = as.numeric(as.factor(param)))
      out <- data_modify(out, Parameter = param)
      d <- rbind(out, d)
    }
    expect_named(d, c("x", "Parameter"))
    expect_identical(d$Parameter, c("e", "b", "a"))

    d <- data.frame()
    for (param in c("a 1", "b 2")) {
      out <- data.frame(x = as.numeric(as.factor(param)))
      out <- data_modify(out, Parameter = param)
      d <- rbind(out, d)
    }
    expect_named(d, c("x", "Parameter"))
    expect_identical(d$Parameter, c("b 2", "a 1"))

    # variable is not copied, values is used
    a <- "x"
    d <- data.frame(x = 1)
    out <- data_modify(d, y = a)
    expect_identical(out$y, "x")
  })
)

withr::with_environment(
  new.env(),
  test_that("data_modify passes expression syntax to function", {
    foo1 <- function(data, ...) {
      head(data_modify(data, ...))
    }
    out1 <- foo1(iris, SW_fraction = Sepal.Width / 10)
    out2 <- foo1(iris, as_expr("SW_fraction = Sepal.Width / 10"))
    expect_identical(out1, out2)
  })
)

## styler: on
