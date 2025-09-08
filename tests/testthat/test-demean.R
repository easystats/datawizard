test_that("demean works", {
  df <- iris

  set.seed(123)
  df$ID <- sample.int(4, nrow(df), replace = TRUE) # fake-ID

  set.seed(123)
  df$binary <- as.factor(rbinom(150, 1, 0.35)) # binary variable

  set.seed(123)
  x <- demean(df, select = c("Sepal.Length", "Petal.Length"), by = "ID", append = FALSE)
  expect_snapshot(head(x))

  set.seed(123)
  expect_message(
    {
      x <- demean(df, select = c("Sepal.Length", "binary", "Species"), by = "ID", append = FALSE)
    },
    "have been coerced to numeric"
  )
  expect_snapshot(head(x))

  set.seed(123)
  expect_message(
    {
      y <- demean(df, select = ~ Sepal.Length + binary + Species, by = ~ID, append = FALSE)
    },
    "have been coerced to numeric"
  )
  expect_message(
    {
      z <- demean(df, select = c("Sepal.Length", "binary", "Species"), by = "ID", append = FALSE)
    },
    "have been coerced to numeric"
  )
  expect_identical(y, z)

  set.seed(123)
  x <- demean(df, select = c("Sepal.Length", "Petal.Length"), by = "ID")
  expect_named(
    x,
    c(
      "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
      "Species", "ID", "binary", "Sepal.Length_between", "Petal.Length_between",
      "Sepal.Length_within", "Petal.Length_within"
    )
  )
  expect_snapshot(head(x))

  df$Sepal.Length_within <- df$Sepal.Length
  expect_error(
    demean(df, select = c("Sepal.Length", "Petal.Length"), by = "ID"),
    regex = "One or more of"
  )
})

test_that("demean interaction term", {
  dat <- data.frame(
    a = c(1, 2, 3, 4, 1, 2, 3, 4),
    x = c(4, 3, 3, 4, 1, 2, 1, 2),
    y = c(1, 2, 1, 2, 4, 3, 2, 1),
    ID = c(1, 2, 3, 1, 2, 3, 1, 2)
  )

  set.seed(123)
  expect_snapshot(demean(dat, select = c("a", "x*y"), by = "ID", append = FALSE))
})

test_that("demean shows message if some vars don't exist", {
  dat <- data.frame(
    a = c(1, 2, 3, 4, 1, 2, 3, 4),
    x = c(4, 3, 3, 4, 1, 2, 1, 2),
    y = c(1, 2, 1, 2, 4, 3, 2, 1),
    ID = c(1, 2, 3, 1, 2, 3, 1, 2)
  )

  set.seed(123)
  expect_error(
    demean(dat, select = "foo", by = "ID"),
    regexp = "not found"
  )
})


# see issue #520
test_that("demean for cross-classified designs (by > 1)", {
  skip_if_not_installed("poorman")

  data(efc, package = "datawizard")
  dat <- na.omit(efc)
  dat$e42dep <- factor(dat$e42dep)
  dat$c172code <- factor(dat$c172code)

  x2a <- dat %>%
    data_group(e42dep) %>%
    data_modify(
      c12hour_e42dep = mean(c12hour)
    ) %>%
    data_ungroup() %>%
    data_group(c172code) %>%
    data_modify(
      c12hour_c172code = mean(c12hour)
    ) %>%
    data_ungroup() %>%
    data_modify(
      c12hour_within = c12hour - c12hour_e42dep - c12hour_c172code
    )

  out <- degroup(
    dat,
    select = "c12hour",
    by = c("e42dep", "c172code"),
    suffix_demean = "_within"
  )

  expect_equal(
    out$c12hour_e42dep_between,
    x2a$c12hour_e42dep,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out$c12hour_within,
    x2a$c12hour_within,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  x2a <- dat %>%
    data_group(e42dep) %>%
    data_modify(
      c12hour_e42dep = mean(c12hour, na.rm = TRUE),
      neg_c_7_e42dep = mean(neg_c_7, na.rm = TRUE)
    ) %>%
    data_ungroup() %>%
    data_group(c172code) %>%
    data_modify(
      c12hour_c172code = mean(c12hour, na.rm = TRUE),
      neg_c_7_c172code = mean(neg_c_7, na.rm = TRUE)
    ) %>%
    data_ungroup() %>%
    data_modify(
      c12hour_within = c12hour - c12hour_e42dep - c12hour_c172code,
      neg_c_7_within = neg_c_7 - neg_c_7_e42dep - neg_c_7_c172code
    )

  out <- degroup(
    dat,
    select = c("c12hour", "neg_c_7"),
    by = c("e42dep", "c172code"),
    suffix_demean = "_within"
  )

  expect_equal(
    out$c12hour_e42dep_between,
    x2a$c12hour_e42dep,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out$neg_c_7_c172code_between,
    x2a$neg_c_7_c172code,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out$neg_c_7_within,
    x2a$neg_c_7_within,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out$c12hour_within,
    x2a$c12hour_within,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )


  # More than 2 groupings
  mu <- 100
  ul <- setNames(c(-1, -3, 0, 4), nm = letters[1:4])
  uL <- setNames(c(10, 30, 0, -40), nm = LETTERS[1:4])
  um <- setNames(c(100, 150, -250), nm = month.abb[1:3])

  dat <- expand.grid(l = letters[1:4], L = LETTERS[1:4], m = month.abb[1:3])

  set.seed(111)
  e <- rnorm(nrow(dat) - 1) |> round(2)
  e <- append(e, -sum(e))

  dat$y <- mu + ul[dat$l] + uL[dat$L] + um[dat$m] + e
  dat$z <- mu + ul[dat$l] + uL[dat$L] + um[dat$m] + 10 * e

  dat_dem <- datawizard::demean(dat, by = c("l", "L", "m"), select = c("y", "z"))

  expect_equal(dat_dem$y_l_between, ave(dat$y, dat$l), ignore_attr = TRUE)
  expect_equal(dat_dem$y_L_between, ave(dat$y, dat$L), ignore_attr = TRUE)
  expect_equal(dat_dem$y_m_between, ave(dat$y, dat$m), ignore_attr = TRUE)
  expect_equal(rowSums(dat_dem[grepl("^y_", colnames(dat_dem))]), dat$y)
  expect_equal(rowSums(dat_dem[grepl("^z_", colnames(dat_dem))]), dat$z)
})


test_that("demean, sanity checks", {
  data(efc, package = "datawizard")
  dat <- na.omit(efc)
  dat$e42dep <- factor(dat$e42dep)
  dat$c172code <- factor(dat$c172code)

  expect_error(
    degroup(
      dat,
      select = c("c12hour", "neg_c_8"),
      by = c("e42dep", "c172code"),
      suffix_demean = "_within"
    ),
    regex = "Variable \"neg_c_8\" was not found"
  )
  expect_error(
    degroup(
      dat,
      select = c("c12hour", "neg_c_8"),
      by = c("e42dep", "c173code"),
      suffix_demean = "_within"
    ),
    regex = "Variables \"neg_c_8\" and \"c173code\" were not found"
  )
})


test_that("demean for nested designs (by > 1), nested = TRUE", {
  data(efc, package = "datawizard")
  dat <- na.omit(efc)
  dat$e42dep <- factor(dat$e42dep)
  dat$c172code <- factor(dat$c172code)

  x_ijk <- dat$c12hour
  xbar_k <- ave(x_ijk, dat$e42dep, FUN = mean)
  xbar_jk <- ave(x_ijk, dat$e42dep, dat$c172code, FUN = mean)

  L3_between <- xbar_k
  L2_between <- xbar_jk - xbar_k
  L1_within <- x_ijk - xbar_jk

  out <- degroup(
    dat,
    select = "c12hour",
    by = c("e42dep", "c172code"),
    nested = TRUE,
    suffix_demean = "_within"
  )

  expect_equal(
    out$c12hour_within,
    L1_within,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out$c12hour_e42dep_between,
    L3_between,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )
  expect_equal(
    out$c12hour_c172code_between,
    L2_between,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # Following #635
  testdf <- data.frame(
    roman = c("I", "I", "I", "I", "II", "II", "II", "II"),
    alphabet = c("a", "a", "b", "b", "a", "b", "a", "b"),
    val1 = c(1, 2, 3, 4, 5, 6, 7, 8),
    val2 = c(1, 2, 3, 4, 5, 6, 7, 8),
    val3 = c(1, 2, 3, 4, 5, 6, 7, 8)
  )

  out <- datawizard::demean(testdf,
    select = c("val1", "val2", "val3"),
    by = "roman/alphabet", append = FALSE
  )

  expect_named(out, c(
    "val1_roman_between", "val1_alphabet_between", "val2_roman_between",
    "val2_alphabet_between", "val3_roman_between", "val3_alphabet_between",
    "val1_within", "val2_within", "val3_within"
  ))

  expect_equal(
    as.vector(out$val1_within),
    c(-0.5, 0.5, -0.5, 0.5, -1, -1, 1, 1)
  )
  expect_equal(out$val1_within, out$val2_within)
  expect_equal(out$val1_within, out$val3_within)

  expect_equal(
    as.vector(out$val1_roman_between),
    c(2.5, 2.5, 2.5, 2.5, 6.5, 6.5, 6.5, 6.5)
  )
  expect_equal(out$val1_roman_between, out$val2_roman_between)
  expect_equal(out$val1_roman_between, out$val3_roman_between)

  expect_equal(
    as.vector(out$val1_alphabet_between),
    c(-1, -1, 1, 1, -0.5, 0.5, -0.5, 0.5)
  )
  expect_equal(out$val1_alphabet_between, out$val2_alphabet_between)
  expect_equal(out$val1_alphabet_between, out$val3_alphabet_between)

  expect_equal(rowSums(out[, grepl("^val1", names(out))]), testdf$val1)
})
