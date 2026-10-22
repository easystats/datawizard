test_that("adjust multilevel", {
  skip_if_not_installed("lme4")
  adj <- adjust(
    iris[c("Sepal.Length", "Species")],
    multilevel = TRUE,
    bayesian = FALSE
  )
  # High tolerance to avoid issues on some R CMD check specification, see #592
  expect_equal(
    head(adj$Sepal.Length),
    c(0.08698, -0.11302, -0.31302, -0.41302, -0.01302, 0.38698),
    tolerance = 1e-1
  )
})

test_that("adjust", {
  adj <- adjust(
    iris[c("Sepal.Length", "Species")],
    multilevel = FALSE,
    bayesian = FALSE
  )
  expect_equal(
    head(adj$Sepal.Length),
    c(0.094, -0.106, -0.306, -0.406, -0.006, 0.394),
    tolerance = 1e-3
  )
})

# select helpers ------------------------------
test_that("adjust regex", {
  expect_identical(
    adjust(mtcars, select = "pg", regex = TRUE),
    adjust(mtcars, select = "mpg")
  )
  expect_identical(
    adjust(mtcars, select = "pg$", regex = TRUE),
    adjust(mtcars, select = "mpg")
  )
})

# select helpers ------------------------------
test_that("adjust, invalid column names", {
  data(iris)
  colnames(iris)[1] <- "I am"
  expect_error(
    adjust(iris[c("I am", "Species")], multilevel = FALSE, bayesian = FALSE),
    regex = "Bad column names"
  )
})

# passing arguments to the model-fitting function ------------------------------
test_that("adjust passes ... to lm()", {
  d <- iris[c("Sepal.Length", "Species")]
  w <- seq_len(nrow(d)) / nrow(d)
  adj <- adjust(d, weights = w)
  m <- stats::lm(Sepal.Length ~ Species, data = d, weights = w)
  expect_equal(
    adj$Sepal.Length,
    unname(stats::residuals(m)),
    tolerance = 1e-6
  )
})

test_that("adjust passes ... to lme4::lmer()", {
  skip_if_not_installed("lme4")
  d <- iris[c("Sepal.Length", "Species")]
  adj <- adjust(d, multilevel = TRUE, REML = FALSE)
  m <- lme4::lmer(Sepal.Length ~ (1 | Species), data = d, REML = FALSE)
  expect_equal(
    adj$Sepal.Length,
    unname(stats::residuals(m)),
    tolerance = 1e-6
  )
})

test_that("adjust passes ... to gamm4::gamm4()", {
  skip_if_not_installed("gamm4")
  d <- iris[c("Sepal.Length", "Sepal.Width")]
  adj <- adjust(
    d,
    effect = "Sepal.Width",
    select = "Sepal.Length",
    additive = TRUE,
    REML = FALSE
  )
  m <- gamm4::gamm4(Sepal.Length ~ s(Sepal.Width), data = d, REML = FALSE)
  expect_equal(
    adj$Sepal.Length,
    unname(insight::get_residuals(m)),
    tolerance = 1e-6
  )
})

test_that("adjust passes ... to rstanarm::stan_glm()", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  d <- iris[c("Sepal.Length", "Sepal.Width")]
  # `refresh` replaces the default of 0
  progress <- utils::capture.output({
    adj <- suppressWarnings(adjust(
      d,
      effect = "Sepal.Width",
      select = "Sepal.Length",
      bayesian = TRUE,
      chains = 1L,
      iter = 200L,
      seed = 1L,
      refresh = 50L
    ))
  })
  expect_true(any(grepl("Iteration", progress, fixed = TRUE)))
  m <- suppressWarnings(rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width,
    data = d,
    refresh = 0L,
    chains = 1L,
    iter = 200L,
    seed = 1L
  ))
  expect_equal(
    adj$Sepal.Length,
    unname(insight::get_residuals(m)),
    tolerance = 1e-6
  )
})

test_that("adjust passes ... to rstanarm::stan_glmer()", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  # should work without attaching rstanarm
  expect_false("package:rstanarm" %in% search())
  d <- iris[c("Sepal.Length", "Species")]
  adj <- suppressWarnings(adjust(
    d,
    multilevel = TRUE,
    bayesian = TRUE,
    chains = 1L,
    iter = 200L,
    seed = 1L
  ))
  m <- suppressWarnings(rstanarm::stan_glmer(
    Sepal.Length ~ (1 | Species),
    data = d,
    family = "gaussian",
    refresh = 0L,
    chains = 1L,
    iter = 200L,
    seed = 1L
  ))
  expect_equal(
    adj$Sepal.Length,
    unname(insight::get_residuals(m)),
    tolerance = 1e-6
  )
})

test_that("adjust passes ... to rstanarm::stan_gamm4()", {
  skip_on_cran()
  skip_if_not_installed("rstanarm")
  skip_if_not_installed("mgcv")
  d <- iris[c("Sepal.Length", "Sepal.Width")]
  adj <- suppressWarnings(adjust(
    d,
    effect = "Sepal.Width",
    select = "Sepal.Length",
    additive = TRUE,
    bayesian = TRUE,
    chains = 1L,
    iter = 200L,
    seed = 1L
  ))
  m <- suppressWarnings(rstanarm::stan_gamm4(
    Sepal.Length ~ s(Sepal.Width),
    data = d,
    refresh = 0L,
    chains = 1L,
    iter = 200L,
    seed = 1L
  ))
  expect_equal(
    adj$Sepal.Length,
    unname(insight::get_residuals(m)),
    tolerance = 1e-6
  )
})
