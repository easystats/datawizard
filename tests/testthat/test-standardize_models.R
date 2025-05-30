# standardize.lm ----------------------------------------------------------
test_that("standardize.lm", {
  iris2 <- na.omit(iris)
  iris_z <- standardize(iris2)

  m0 <- lm(Sepal.Length ~ Species * Petal.Width, data = iris_z)
  m1 <- lm(Sepal.Length ~ Species * Petal.Width, data = iris2)
  model <- standardize(m1)
  expect_identical(coef(m0), coef(model))
})

test_that("standardize, mlm", {
  m <- lm(cbind(mpg, hp) ~ cyl + am, data = mtcars)
  m2 <- lm(scale(cbind(mpg, hp)) ~ scale(cyl) + scale(am), data = mtcars)

  mz <- standardize(m)
  expect_equal(coef(mz), coef(m2), ignore_attr = TRUE, tolerance = 1e-4)
})

test_that("standardize | errors", {
  my_lm_external_formula <- function(.dat, predicted, predictor) {
    my_formula <- as.formula(paste0(predicted, "~", predictor))
    lm(formula = my_formula, data = .dat)
  }

  m <- my_lm_external_formula(mtcars, "mpg", "am")
  ers <- capture_error(standardize(m))
  expect_match(as.character(ers), "Try instead to standardize the data",
    fixed = TRUE
  )
})


test_that("standardize | problematic formulas", {
  data(mtcars)
  m <- lm(mpg ~ hp, data = mtcars)
  expect_equal(
    coef(standardise(m)),
    c(`(Intercept)` = -3.14935717633686e-17, hp = -0.776168371826586),
    tolerance = 1e-4
  )

  colnames(mtcars)[1] <- "1_mpg"
  m <- lm(`1_mpg` ~ hp, data = mtcars)
  expect_error(standardise(m), regex = "Looks like")

  # works interactive only
  # data(mtcars)
  # m <- lm(mtcars$mpg ~ mtcars$hp)
  # expect_error(standardise(m), regex = "model formulas")

  m <- lm(mtcars[, 1] ~ hp, data = mtcars)
  expect_error(standardise(m), regex = "indexed data")
})


# Transformations ---------------------------------------------------------
test_that("transformations", {
  skip_if_not_installed("effectsize")
  # deal with log / sqrt terms
  expect_message(standardize(lm(mpg ~ sqrt(cyl) + log(hp), mtcars)))
  expect_message(standardize(lm(mpg ~ sqrt(cyl), mtcars)))
  expect_message(standardize(lm(mpg ~ log(hp), mtcars)))

  # difference between stand-methods:
  mt <- mtcars
  mt$hp_100 <- mt$hp / 100
  fit_exp <- lm(mpg ~ exp(hp_100), mt)
  fit_scale1 <- lm(scale(mpg) ~ exp(scale(hp_100)), mt)
  fit_scale2 <- lm(scale(mpg) ~ scale(exp(hp_100)), mt)
  expect_equal(
    effectsize::standardize_parameters(fit_exp, method = "refit")[2, 2],
    unname(coef(fit_scale1)[2]),
    ignore_attr = TRUE
  )

  expect_equal(
    effectsize::standardize_parameters(fit_exp, method = "basic")[2, 2],
    unname(coef(fit_scale2)[2]),
    ignore_attr = TRUE
  )

  d <- data.frame(
    time = as.factor(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)),
    group = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
    sum = c(0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50, 0, 5, 10, 15, 20, 0, 20, 25, 45, 50) # nolint
  )
  m <- lm(log(sum + 1) ~ as.numeric(time) * group, data = d)


  expect_message({
    out <- standardize(m)
  })
  expect_identical(coef(m), c(
    `(Intercept)` = -0.4575, `as.numeric(time)` = 0.5492, group = 0.3379,
    `as.numeric(time):group` = 0.15779
  ), tolerance = 0.01)
})


# W/ weights --------------------------------------------------------------
test_that("weights", {
  expect_warning(standardize(mtcars, weights = "xx"))

  m <- lm(mpg ~ wt + hp, weights = cyl, mtcars)

  sm <- standardize(m, weights = TRUE)
  sm_data <- insight::get_data(sm, source = "frame")
  sm_data2 <- standardize(mtcars, select = c("mpg", "wt", "hp"), weights = "cyl")
  expect_identical(sm_data[, c("mpg", "wt", "hp")], sm_data2[, c("mpg", "wt", "hp")])

  expect_error(standardize(m, weights = TRUE, robust = TRUE), NA)

  # no weights in stding
  sm_xw <- standardize(m, weights = FALSE)
  sm_data_xw <- insight::get_data(sm_xw, source = "frame")
  expect_false(isTRUE(all.equal(coef(sm)[-1], coef(sm_xw)[-1])))

  skip_if_not_installed("effectsize")
  # refit and posthoc should give same results
  stdREFIT <- effectsize::standardize_parameters(m, method = "refit")
  expect_equal(
    stdREFIT[[2]],
    effectsize::standardize_parameters(m, method = "posthoc")[[2]],
    ignore_attr = TRUE
  )

  expect_equal(
    stdREFIT[[2]],
    effectsize::standardize_parameters(m, method = "basic")[[2]],
    ignore_attr = TRUE
  )
})


# weights + missing data --------------------------------------------------
test_that("weights + NA", {
  set.seed(1234)
  data(iris)

  # data setup
  iris$weight_me <- runif(nrow(iris))
  iris$Sepal.Length[sample(nrow(iris), size = 10)] <- NA
  iris$weight_me[sample(nrow(iris), size = 10)] <- NA

  # standardize 2nd data set
  iris2 <- standardize(iris,
    select = c("Sepal.Length", "Petal.Width"),
    remove_na = "all"
  )
  iris3 <- standardize(iris,
    select = c("Sepal.Length", "Petal.Width"),
    weights = "weight_me",
    remove_na = "selected"
  )


  m1 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris, weights = weight_me)


  # weights, missing data, but data isn't weight-stdized
  m2 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris2, weights = weight_me)
  sm2 <- standardize(m1, weights = FALSE)
  expect_identical(coef(m2), coef(sm2))

  # weights, missing data, and data is weight-stdized
  m3 <- lm(Sepal.Length ~ Species + Petal.Width, data = iris3, weights = weight_me)
  sm3 <- standardize(m1, weights = TRUE)
  expect_identical(coef(m3), coef(sm3))
})


# weights + missing data ´+ na.action = na.exclude --------------------------------------------------
test_that("weights + NA + na.exclude", {
  skip_if_not_installed("effectsize")
  set.seed(1234)
  data(iris)

  # data setup
  iris$weight_me <- runif(nrow(iris))
  iris$Sepal.Length[sample(nrow(iris), size = 25)] <- NA
  iris$weight_me[sample(nrow(iris), size = 15)] <- NA
  d <- iris

  m1 <- lm(Sepal.Length ~ Species + Petal.Width, data = d, weights = weight_me, na.action = na.exclude)
  m2 <- lm(Sepal.Length ~ Species + Petal.Width, data = d, weights = weight_me)

  expect_identical(coef(standardize(m2)), coef(standardize(m1)), tolerance = 1e-3)
  expect_identical(effectsize::standardize_parameters(m1, method = "basic")[[2]],
    effectsize::standardize_parameters(m2, method = "basic")[[2]],
    tolerance = 1e-3
  )
})

# subset ------------------
test_that("fail with subset", {
  data("mtcars")

  mod1 <- lm(mpg ~ hp,
    data = mtcars,
    subset = cyl > 4
  )

  expect_error(standardise(mod1), regexp = "subset")
})


# don't standardize non-Gaussian response ------------------------------------
test_that("standardize non-Gaussian response", {
  skip_on_cran()
  skip_if_not_installed("lme4")
  set.seed(1234)
  data(sleepstudy, package = "lme4")

  m1 <- glm(Reaction ~ Days, family = Gamma(), data = sleepstudy)
  m2 <- glm(Reaction ~ Days, family = Gamma(link = "identity"), data = sleepstudy)
  m3 <- glm(Reaction ~ Days, family = inverse.gaussian(), data = sleepstudy)

  expect_identical(coef(standardize(m1)), c(`(Intercept)` = 0.00338, Days = -0.00034), tolerance = 1e-2)
  expect_identical(coef(standardize(m2)), c(`(Intercept)` = 298.48571, Days = 29.70754), tolerance = 1e-3)
  expect_identical(coef(standardize(m3)), c(`(Intercept)` = 1e-05, Days = 0), tolerance = 1e-3)
})


# variables evaluated in the environment $$$ ------------------------------
test_that("variables evaluated in the environment", {
  m <- lm(mtcars$mpg ~ mtcars$cyl + am, data = mtcars)
  w <- capture_error(standardize(m))
  expect_true(any(grepl("Using `$`", w, fixed = TRUE)))

  ## Note:
  # No idea why this is suddenly not giving a warning on older R versions.
  m <- lm(mtcars$mpg ~ mtcars$cyl + mtcars$am, data = mtcars)
  w <- capture_error(standardize(m))
  expect_true(any(grepl("Using `$`", w, fixed = TRUE)))
})


# mediation models --------------------------------------------------------
test_that("standardize mediation", {
  skip_on_cran()
  skip_if_not_installed("mediation")
  set.seed(444)
  data(jobs, package = "mediation")
  jobs$econ_hard <- jobs$econ_hard * 20
  b.int <- lm(job_seek ~ treat * age + econ_hard + sex, data = jobs)
  d.int <- lm(depress2 ~ treat * job_seek * age + econ_hard + sex, data = jobs)

  med1 <- mediation::mediate(b.int, d.int, sims = 200, treat = "treat", mediator = "job_seek")
  med2 <- mediation::mediate(b.int, d.int,
    sims = 200, treat = "treat", mediator = "job_seek",
    covariates = list(age = mean(jobs$age))
  )

  out1 <- summary(standardize(med1))
  expect_message({
    out2 <- summary(standardize(med2))
  })
  expect_identical(unlist(out1[c("d0", "d1", "z0", "z1", "n0", "n1", "tau.coef")]),
    unlist(out2[c("d0", "d1", "z0", "z1", "n0", "n1", "tau.coef")]),
    tolerance = 0.1
  )

  med0 <- mediation::mediate(
    standardize(b.int),
    standardize(d.int),
    sims = 200,
    treat = "treat",
    mediator = "job_seek"
  )
  out0 <- summary(med0)
  medz <- standardize(mediation::mediate(
    b.int,
    d.int,
    sims = 200,
    treat = "treat",
    mediator = "job_seek"
  ))
  outz <- summary(medz)
  expect_identical(unlist(out0[c("d0", "d1", "z0", "z1", "n0", "n1", "tau.coef")]),
    unlist(outz[c("d0", "d1", "z0", "z1", "n0", "n1", "tau.coef")]),
    tolerance = 0.1
  )
})

# Offsets -----------------------------------------------------------------

test_that("offsets", {
  skip_if_not_installed("effectsize")
  skip_if_not_installed("parameters")

  m <- lm(mpg ~ hp + offset(wt), data = mtcars)

  expect_warning({
    mz1 <- standardize(m)
  })
  expect_warning({
    mz2 <- standardize(m, two_sd = TRUE)
  })
  expect_identical(c(1, 2) * coef(mz1), coef(mz2))


  m <- glm(cyl ~ hp + offset(wt), family = poisson(), data = mtcars)
  expect_warning(
    {
      mz <- standardize(m)
    },
    regexp = NA
  )

  par1 <- parameters::model_parameters(mz)
  par2 <- effectsize::standardize_parameters(m, method = "basic")
  expect_identical(par2[2, 2], par1[2, 2], tolerance = 0.05)
})


# BRMS --------------------------------------------------------------------

test_that("brms", {
  skip_on_cran()
  skip_on_os(c("windows", "mac"))
  skip_if_not_installed("brms")
  skip_if_not_installed("RcppEigen")
  skip_if_not_installed("BH")

  invisible(
    capture.output({
      mod <- brms::brm(mpg ~ hp,
        data = mtcars,
        refresh = 0, chains = 1, silent = 2
      )
    })
  )

  expect_warning(
    standardize(mod),
    regexp = "without adjusting priors may lead to bogus"
  )
})
