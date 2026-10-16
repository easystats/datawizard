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


test_that("degroup, formula in `by` with `/` is nested, with `+` is crossed", {
  # three nested levels; the ids of B and C repeat across higher clusters
  d <- data.frame(
    A = rep(c("I", "II"), each = 6),
    B = rep(c("a", "a", "b", "b", "c", "c"), 2),
    C = rep(c("x", "y"), 6),
    v = c(1, 4, 2, 8, 5, 7, 3, 9, 6, 2, 10, 4)
  )

  expect_identical(
    degroup(d, select = "v", by = ~ A / B),
    degroup(d, select = "v", by = "A/B")
  )
  expect_identical(
    degroup(d, select = "v", by = ~ A / B / C),
    degroup(d, select = "v", by = "A/B/C")
  )
  # a formula with `/` is nested even when `nested = FALSE` is given, as the
  # string form is
  expect_identical(
    degroup(d, select = "v", by = ~ A / B, nested = FALSE),
    degroup(d, select = "v", by = c("A", "B"), nested = TRUE)
  )
  # the nested result differs from the crossed result for these data
  expect_false(isTRUE(all.equal(
    degroup(d, select = "v", by = ~ A / B)$v_within,
    degroup(d, select = "v", by = c("A", "B"))$v_within
  )))

  expect_identical(
    degroup(d, select = "v", by = ~ A + B),
    degroup(d, select = "v", by = c("A", "B"), nested = FALSE)
  )
})


test_that("degroup, mixed nested and crossed `by` stops with an error", {
  d <- data.frame(
    A = rep(c("I", "II"), each = 6),
    B = rep(c("a", "a", "b", "b", "c", "c"), 2),
    C = rep(c("x", "y"), 6),
    v = c(1, 4, 2, 8, 5, 7, 3, 9, 6, 2, 10, 4)
  )

  expect_error(
    degroup(d, select = "v", by = ~ C + A / B),
    regex = "mix nested and crossed grouping are not supported"
  )
  expect_error(
    degroup(d, select = "v", by = ~ A / B + C),
    regex = "mix nested and crossed grouping are not supported"
  )
  expect_error(
    degroup(d, select = "v", by = c("C", "A/B")),
    regex = "mix nested and crossed grouping are not supported"
  )
  expect_error(
    demean(d, select = "v", by = c("A/B", "C")),
    regex = "mix nested and crossed grouping are not supported"
  )
})


# warnings with whitespace collapsed, because insight wraps long messages
squished_warnings <- function(expr) {
  gsub("\\s+", " ", testthat::capture_warnings(expr))
}


test_that("degroup, crossed `by` warns when one variable is nested in another", {
  # nested data with unique class ids, given as crossed. `class` is a factor
  # with an unused level, and the two rows with a missing class are in
  # different schools: counted as a value, the missing class would occur with
  # two schools and hide the nesting.
  d <- data.frame(
    school = c("s1", "s1", "s1", "s1", "s2", "s2", "s2", "s2"),
    class = factor(
      c("c1", "c1", "c2", NA, "c3", "c3", "c4", NA),
      levels = c("c1", "c2", "c3", "c4", "c9")
    ),
    v = c(1, 3, 2, 6, 5, 9, 4, 8)
  )

  w <- squished_warnings(
    out <- degroup(d, select = "v", by = c("school", "class"))
  )
  expect_length(w, 1)
  expect_match(w, "`class` is nested in `school`", fixed = TRUE)
  expect_match(w, "set `nested = TRUE`", fixed = TRUE)
  expect_identical(
    out,
    degroup(d, select = "v", by = c("school", "class"), verbose = FALSE)
  )
  expect_no_warning(
    degroup(d, select = "v", by = c("school", "class"), verbose = FALSE)
  )

  # a one-to-one pair warns once
  d_one <- data.frame(
    a = c("x", "x", "y", "y"),
    b = c("p", "p", "q", "q"),
    v = c(1, 2, 3, 5)
  )
  w <- squished_warnings(degroup(d_one, select = "v", by = c("a", "b")))
  expect_length(w, 1)
  expect_match(w, "`a` is nested in `b`", fixed = TRUE)
  expect_match(w, "set `nested = TRUE`", fixed = TRUE)

  # no warning when no row has both values
  d_na <- data.frame(
    a = c("x", "x", NA, NA),
    b = c(NA, NA, "p", "q"),
    v = c(1, 2, 3, 5)
  )
  expect_no_warning(degroup(d_na, select = "v", by = c("a", "b")))
})


test_that("degroup, crossed `by` gives no warning for crossed data", {
  # balanced crossed fixture
  d <- expand.grid(person = c("p1", "p2", "p3"), item = c("i1", "i2"))
  d$v <- c(4, 2, 7, 1, 8, 3)
  expect_no_warning(degroup(d, select = "v", by = c("person", "item")))

  # data of the cross-classified test above
  data(efc, package = "datawizard")
  dat <- na.omit(efc)
  dat$e42dep <- factor(dat$e42dep)
  dat$c172code <- factor(dat$c172code)
  expect_no_warning(
    degroup(dat, select = "c12hour", by = c("e42dep", "c172code"))
  )
})


test_that("degroup, nested `by` warns when levels are listed from lowest to highest", {
  # unique class ids, so each class is in exactly one school. `class` is a
  # factor with an unused level and has missing values in two schools.
  d <- data.frame(
    school = c("s1", "s1", "s1", "s1", "s2", "s2", "s2", "s2"),
    class = factor(
      c("c1", "c1", "c2", NA, "c3", "c3", "c4", NA),
      levels = c("c1", "c2", "c3", "c4", "c9")
    ),
    v = c(1, 3, 2, 6, 5, 9, 4, 8)
  )

  w <- squished_warnings(
    out <- degroup(d, select = "v", by = c("class", "school"), nested = TRUE)
  )
  expect_length(w, 1)
  expect_match(w, "lists levels from highest to lowest", fixed = TRUE)
  expect_match(w, "`class` is nested in `school`", fixed = TRUE)
  expect_identical(
    out,
    degroup(
      d,
      select = "v",
      by = c("class", "school"),
      nested = TRUE,
      verbose = FALSE
    )
  )
  w <- squished_warnings(degroup(d, select = "v", by = "class/school"))
  expect_length(w, 1)
  expect_match(w, "lists levels from highest to lowest", fixed = TRUE)
  expect_match(w, "`class` is nested in `school`", fixed = TRUE)

  # the correct order gives no warning
  expect_no_warning(
    degroup(d, select = "v", by = c("school", "class"), nested = TRUE)
  )

  # correct order, lower-level ids that repeat across clusters
  d_rep <- data.frame(
    school = c("s1", "s1", "s1", "s1", "s2", "s2", "s2", "s2"),
    class = c("c1", "c1", "c2", "c2", "c1", "c1", "c2", "c2"),
    v = c(1, 3, 2, 6, 5, 9, 4, 8)
  )
  expect_no_warning(
    degroup(d_rep, select = "v", by = c("school", "class"), nested = TRUE)
  )
  expect_no_warning(degroup(d_rep, select = "v", by = "school/class"))
})


test_that("degroup, crossed components on unbalanced data follow Guo et al.", {
  # persons crossed with items, with 1 or 2 rows per cell:
  # (p1,i1) 2, (p1,i2) 1, (p2,i1) 1, (p2,i2) 1, (p3,i1) 1, (p3,i2) 2
  d <- data.frame(
    person = c("p1", "p1", "p1", "p2", "p2", "p3", "p3", "p3"),
    item = c("i1", "i1", "i2", "i1", "i2", "i2", "i2", "i1"),
    x = c(2, 4, 9, 5, 1, 6, 10, 3)
  )
  out <- demean(d, select = "x", by = c("person", "item"), append = FALSE)

  # Oracle (closed-form, Guo et al. 2024, eq. 4): the components are the
  # person mean, the item mean, and x minus both. Means over rows:
  # person: p1 -> (2+4+9)/3 = 5, p2 -> (5+1)/2 = 3, p3 -> (6+10+3)/3 = 19/3
  # item:   i1 -> (2+4+5+3)/4 = 3.5, i2 -> (9+1+6+10)/4 = 6.5
  expected_person <- c(5, 5, 5, 3, 3, 19 / 3, 19 / 3, 19 / 3)
  expected_item <- c(3.5, 3.5, 6.5, 3.5, 6.5, 6.5, 6.5, 3.5)
  # within: 2-5-3.5 = -6.5, 4-5-3.5 = -4.5, 9-5-6.5 = -2.5, 5-3-3.5 = -1.5,
  # 1-3-6.5 = -8.5, 6-19/3-6.5 = -41/6, 10-19/3-6.5 = -17/6, 3-19/3-3.5 = -41/6
  expected_within <- c(-6.5, -4.5, -2.5, -1.5, -8.5, -41 / 6, -17 / 6, -41 / 6)
  expect_equal(out$x_person_between, expected_person, ignore_attr = TRUE)
  expect_equal(out$x_item_between, expected_item, ignore_attr = TRUE)
  expect_equal(out$x_within, expected_within, ignore_attr = TRUE)

  # Oracle (invariant): the components of each row sum to x
  expect_equal(rowSums(out), d$x, ignore_attr = TRUE)

  # Oracle (reference implementation): the steps of `cc.gmc()` in Guo et al.
  # 2024, Appendix C: `aggregate()` the mean per cluster, `merge()` it back,
  # and subtract each cluster mean from x
  d$row <- seq_len(nrow(d))
  mn1 <- stats::aggregate(d["x"], d["person"], mean, na.rm = TRUE)
  mn2 <- stats::aggregate(d["x"], d["item"], mean, na.rm = TRUE)
  names(mn1)[2] <- "x_mn1"
  names(mn2)[2] <- "x_mn2"
  ref <- merge(merge(d, mn1, by = "person"), mn2, by = "item")
  ref <- ref[order(ref$row), ]
  ref$x_dev <- ref$x - ref$x_mn1 - ref$x_mn2
  expect_equal(out$x_person_between, ref$x_mn1, ignore_attr = TRUE)
  expect_equal(out$x_item_between, ref$x_mn2, ignore_attr = TRUE)
  expect_equal(out$x_within, ref$x_dev, ignore_attr = TRUE)

  # the means are over rows, so they differ from the mean of the cell means:
  # p1 -> mean(c(3, 9)) = 6, not 5
  expect_false(isTRUE(all.equal(out$x_person_between[1], 6)))
})
