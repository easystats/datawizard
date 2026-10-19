test_that("demean works", {
  df <- iris

  set.seed(123)
  df$ID <- sample.int(4, nrow(df), replace = TRUE) # fake-ID

  set.seed(123)
  df$binary <- as.factor(rbinom(150, 1, 0.35)) # binary variable

  set.seed(123)
  x <- demean(
    df,
    select = c("Sepal.Length", "Petal.Length"),
    by = "ID",
    append = FALSE
  )
  expect_snapshot(head(x))

  set.seed(123)
  expect_message(
    {
      x <- demean(
        df,
        select = c("Sepal.Length", "binary", "Species"),
        by = "ID",
        append = FALSE
      )
    },
    "have been coerced to numeric"
  )
  expect_snapshot(head(x))

  set.seed(123)
  expect_message(
    {
      y <- demean(
        df,
        select = ~ Sepal.Length + binary + Species,
        by = ~ID,
        append = FALSE
      )
    },
    "have been coerced to numeric"
  )
  expect_message(
    {
      z <- demean(
        df,
        select = c("Sepal.Length", "binary", "Species"),
        by = "ID",
        append = FALSE
      )
    },
    "have been coerced to numeric"
  )
  expect_identical(y, z)

  set.seed(123)
  x <- demean(df, select = c("Sepal.Length", "Petal.Length"), by = "ID")
  expect_named(
    x,
    c(
      "Sepal.Length",
      "Sepal.Width",
      "Petal.Length",
      "Petal.Width",
      "Species",
      "ID",
      "binary",
      "Sepal.Length_between",
      "Petal.Length_between",
      "Sepal.Length_within",
      "Petal.Length_within"
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
  expect_snapshot(demean(
    dat,
    select = c("a", "x*y"),
    by = "ID",
    append = FALSE
  ))
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

  dat_dem <- datawizard::demean(
    dat,
    by = c("l", "L", "m"),
    select = c("y", "z")
  )

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

  out <- datawizard::demean(
    testdf,
    select = c("val1", "val2", "val3"),
    by = "roman/alphabet",
    append = FALSE
  )

  expect_named(
    out,
    c(
      "val1_roman_between",
      "val1_alphabet_between",
      "val2_roman_between",
      "val2_alphabet_between",
      "val3_roman_between",
      "val3_alphabet_between",
      "val1_within",
      "val2_within",
      "val3_within"
    )
  )

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

test_that("degroup for nested designs with three or more levels", {
  # Unbalanced data. B and C ids repeat across higher-level clusters, so a
  # cluster is defined by all variables above it (as in `y ~ (1 | A/B/C)`).
  dat <- data.frame(
    A = c(1, 1, 1, 1, 1, 2, 2, 2),
    B = c(1, 1, 1, 2, 2, 1, 1, 2),
    C = c(1, 1, 2, 1, 1, 1, 2, 1),
    x = c(1, 3, 8, 4, 6, 10, 2, 7)
  )

  # Oracle (closed-form, Brincks et al. 2017, eq. 8-11, extended to three
  # nested variables). Means over rows:
  # A means: A=1 -> (1+3+8+4+6)/5 = 4.4, A=2 -> (10+2+7)/3 = 19/3
  # A:B means: (1,1) -> 4, (1,2) -> 5, (2,1) -> 6, (2,2) -> 7
  # A:B:C means: (1,1,1) -> 2, (1,1,2) -> 8, (1,2,1) -> 5, (2,1,1) -> 10,
  #              (2,1,2) -> 2, (2,2,1) -> 7
  expected_A <- c(4.4, 4.4, 4.4, 4.4, 4.4, 19 / 3, 19 / 3, 19 / 3)
  expected_B <- c(4, 4, 4, 5, 5, 6, 6, 7) - expected_A
  expected_C <- c(2, 2, 8, 5, 5, 10, 2, 7) - c(4, 4, 4, 5, 5, 6, 6, 7)
  expected_within <- dat$x - c(2, 2, 8, 5, 5, 10, 2, 7)

  for (by in list(c("A", "B", "C"), "A/B/C")) {
    out <- degroup(
      dat,
      select = "x",
      by = by,
      nested = TRUE,
      append = FALSE
    )
    expect_equal(out$x_A_between, expected_A, ignore_attr = TRUE)
    expect_equal(out$x_B_between, expected_B, ignore_attr = TRUE)
    expect_equal(out$x_C_between, expected_C, ignore_attr = TRUE)
    expect_equal(out$x_within, expected_within, ignore_attr = TRUE)
  }

  # Oracle (invariant): the components of each row sum to x, and each
  # component after the first has mean zero within the clusters above it.
  out <- degroup(dat, select = "x", by = "A/B/C", append = FALSE)
  expect_equal(rowSums(out), dat$x, ignore_attr = TRUE)
  expect_equal(
    as.vector(tapply(out$x_B_between, dat$A, mean)),
    c(0, 0),
    tolerance = 1e-8
  )
  expect_equal(
    as.vector(tapply(out$x_C_between, paste(dat$A, dat$B), mean)),
    c(0, 0, 0, 0),
    tolerance = 1e-8
  )

  # Oracle (invariant): with several variables in `select`, each variable gets
  # its own components. y = 2 * x + 1, so the components of y are twice those
  # of x, plus 1 for the highest level.
  dat$y <- 2 * dat$x + 1
  out <- demean(dat, select = c("x", "y"), by = "A/B/C", append = FALSE)
  expect_equal(out$y_A_between, 2 * expected_A + 1, ignore_attr = TRUE)
  expect_equal(out$y_B_between, 2 * expected_B, ignore_attr = TRUE)
  expect_equal(out$y_C_between, 2 * expected_C, ignore_attr = TRUE)
  expect_equal(out$y_within, 2 * expected_within, ignore_attr = TRUE)
  expect_equal(out$x_C_between, expected_C, ignore_attr = TRUE)
})

test_that("degroup for nested designs with four levels and median", {
  # cluster statistic computed with an explicit loop, independent of
  # `stats::ave()`; a cluster is a combination of the given variables
  cluster_stat <- function(x, groups, fun = mean) {
    key <- do.call(paste, c(groups, sep = "|"))
    out <- numeric(length(x))
    for (k in unique(key)) {
      out[key == k] <- fun(x[key == k])
    }
    out
  }

  set.seed(123)
  dat <- data.frame(
    A = sample.int(3, 120, replace = TRUE),
    B = sample.int(3, 120, replace = TRUE),
    C = sample.int(2, 120, replace = TRUE),
    D = sample.int(2, 120, replace = TRUE),
    x = rnorm(120)
  )

  # Oracle (closed-form, Brincks et al. 2017, eq. 8-11, extended to four
  # nested variables): the component of each variable is its cluster mean
  # minus the cluster mean of the variable above it.
  m1 <- cluster_stat(dat$x, dat["A"])
  m2 <- cluster_stat(dat$x, dat[c("A", "B")])
  m3 <- cluster_stat(dat$x, dat[c("A", "B", "C")])
  m4 <- cluster_stat(dat$x, dat[c("A", "B", "C", "D")])

  out <- degroup(dat, select = "x", by = "A/B/C/D", append = FALSE)
  expect_equal(out$x_A_between, m1, ignore_attr = TRUE)
  expect_equal(out$x_B_between, m2 - m1, ignore_attr = TRUE)
  expect_equal(out$x_C_between, m3 - m2, ignore_attr = TRUE)
  expect_equal(out$x_D_between, m4 - m3, ignore_attr = TRUE)
  expect_equal(out$x_within, dat$x - m4, ignore_attr = TRUE)

  # same definitions with the median in place of the mean, three levels
  md1 <- cluster_stat(dat$x, dat["A"], stats::median)
  md2 <- cluster_stat(dat$x, dat[c("A", "B")], stats::median)
  md3 <- cluster_stat(dat$x, dat[c("A", "B", "C")], stats::median)

  out <- degroup(
    dat,
    select = "x",
    by = c("A", "B", "C"),
    nested = TRUE,
    center = "median",
    append = FALSE
  )
  expect_equal(out$x_A_between, md1, ignore_attr = TRUE)
  expect_equal(out$x_B_between, md2 - md1, ignore_attr = TRUE)
  expect_equal(out$x_C_between, md3 - md2, ignore_attr = TRUE)
  expect_equal(out$x_within, dat$x - md3, ignore_attr = TRUE)
})
