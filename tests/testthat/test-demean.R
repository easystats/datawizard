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
    v = c(1, 4, 2, 8, 5, 7, 3, 9, 6, 2, 10, 4),
    w = c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8),
    stringsAsFactors = FALSE
  )

  expect_identical(
    demean(d, select = c("v", "w"), by = ~ A / B),
    demean(d, select = c("v", "w"), by = "A/B")
  )
  # only the right-hand side of a formula is read
  expect_identical(
    degroup(d, select = "v", by = v ~ A / B),
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
  # a formula with only `+` keeps the value of `nested`
  expect_identical(
    degroup(d, select = "v", by = ~ A + B, nested = TRUE),
    degroup(d, select = "v", by = c("A", "B"), nested = TRUE)
  )
})


test_that("degroup, mixed nested and crossed `by` stops with an error", {
  d <- data.frame(
    A = rep(c("I", "II"), each = 6),
    B = rep(c("a", "a", "b", "b", "c", "c"), 2),
    C = rep(c("x", "y"), 6),
    v = c(1, 4, 2, 8, 5, 7, 3, 9, 6, 2, 10, 4),
    stringsAsFactors = FALSE
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
  expect_error(
    degroup(d, select = "v", by = "C + A/B"),
    regex = "mix nested and crossed grouping are not supported"
  )

  # a column whose name contains `/` is not split in a vector of names
  d2 <- d
  names(d2)[names(d2) == "A"] <- "A/B"
  expect_identical(
    unname(degroup(d2, select = "v", by = c("A/B", "C"), append = FALSE)),
    unname(degroup(d, select = "v", by = c("A", "C"), append = FALSE))
  )

  # nor in a formula, where the name is backticked
  expect_identical(
    unname(degroup(d2, select = "v", by = ~`A/B`, append = FALSE)),
    unname(degroup(d, select = "v", by = ~A, append = FALSE))
  )
  expect_identical(
    unname(degroup(d2, select = "v", by = ~ C + `A/B`, append = FALSE)),
    unname(degroup(d, select = "v", by = ~ C + A, append = FALSE))
  )
})


test_that(".nesting_of_pair handles many levels without integer overflow", {
  # the product of the numbers of levels exceeds .Machine$integer.max, and
  # the last value of p occurs with two values of q
  p <- c(1:50000, 50000)
  q <- c(1:50000, 49999)
  expect_silent(nesting <- .nesting_of_pair(p, q)) # nolint: implicit_assignment_linter.
  expect_identical(nesting, c(p_in_q = FALSE, q_in_p = FALSE))
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
    v = c(1, 3, 2, 6, 5, 9, 4, 8),
    stringsAsFactors = FALSE
  )

  w <- squished_warnings(degroup(d, select = "v", by = c("school", "class")))
  out <- suppressWarnings(degroup(d, select = "v", by = c("school", "class")))
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
    v = c(1, 2, 3, 5),
    stringsAsFactors = FALSE
  )
  w <- squished_warnings(degroup(d_one, select = "v", by = c("a", "b")))
  expect_length(w, 1)
  expect_match(w, "`a` is nested in `b`", fixed = TRUE)
  expect_match(w, "set `nested = TRUE`", fixed = TRUE)

  # with three variables, all nested pairs are named in one warning
  d_three <- data.frame(
    sch = c("s1", "s1", "s1", "s1", "s2", "s2", "s2", "s2"),
    cls = c("c1", "c1", "c2", "c2", "c3", "c3", "c4", "c4"),
    stu = c("u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8"),
    v = c(1, 3, 2, 6, 5, 9, 4, 8),
    stringsAsFactors = FALSE
  )
  w <- squished_warnings(
    degroup(d_three, select = "v", by = c("sch", "cls", "stu"))
  )
  expect_length(w, 1)
  expect_match(w, "`cls` is nested in `sch`", fixed = TRUE)
  expect_match(w, "`stu` is nested in `sch`", fixed = TRUE)
  expect_match(w, "`stu` is nested in `cls`", fixed = TRUE)

  # a crossed variable with a single value also warns
  d_single <- data.frame(
    person = c("p1", "p1", "p2", "p2"),
    site = c("s1", "s1", "s1", "s1"),
    v = c(1, 2, 3, 5),
    stringsAsFactors = FALSE
  )
  w <- squished_warnings(
    degroup(d_single, select = "v", by = c("person", "site"))
  )
  expect_length(w, 1)
  expect_match(w, "`person` is nested in `site`", fixed = TRUE)

  # nested data whose class ids repeat across schools look crossed: no warning
  d_rep <- data.frame(
    school = c("s1", "s1", "s1", "s1", "s2", "s2", "s2", "s2"),
    class = c("c1", "c1", "c2", "c2", "c1", "c1", "c2", "c2"),
    v = c(1, 3, 2, 6, 5, 9, 4, 8),
    stringsAsFactors = FALSE
  )
  expect_no_warning(degroup(d_rep, select = "v", by = c("school", "class")))

  # no warning when no row has both values
  d_na <- data.frame(
    a = c("x", "x", NA, NA),
    b = c(NA, NA, "p", "q"),
    v = c(1, 2, 3, 5),
    stringsAsFactors = FALSE
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
    v = c(1, 3, 2, 6, 5, 9, 4, 8),
    stringsAsFactors = FALSE
  )

  w <- squished_warnings(
    degroup(d, select = "v", by = c("class", "school"), nested = TRUE)
  )
  out <- suppressWarnings(
    degroup(d, select = "v", by = c("class", "school"), nested = TRUE)
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
    v = c(1, 3, 2, 6, 5, 9, 4, 8),
    stringsAsFactors = FALSE
  )
  expect_no_warning(
    degroup(d_rep, select = "v", by = c("school", "class"), nested = TRUE)
  )
  expect_no_warning(degroup(d_rep, select = "v", by = "school/class"))

  # three levels: only the adjacent pair in the wrong order is named
  d_three <- data.frame(
    sch = c("s1", "s1", "s1", "s1", "s2", "s2", "s2", "s2"),
    cls = c("c1", "c1", "c2", "c2", "c3", "c3", "c4", "c4"),
    stu = c("u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8"),
    v = c(1, 3, 2, 6, 5, 9, 4, 8),
    stringsAsFactors = FALSE
  )
  # one class per school with repeated class ids: a warning, although the
  # order is correct
  d_one_class <- data.frame(
    school = c("s1", "s1", "s2", "s2", "s3", "s3"),
    class = c("1", "1", "1", "1", "1", "1"),
    v = c(1, 3, 2, 6, 5, 9),
    stringsAsFactors = FALSE
  )
  w <- squished_warnings(
    degroup(d_one_class, select = "v", by = c("school", "class"), nested = TRUE)
  )
  expect_length(w, 1)
  expect_match(w, "`school` is nested in `class`", fixed = TRUE)
  # wrong order with class ids that repeat across schools: not found
  expect_no_warning(
    degroup(d_rep, select = "v", by = c("class", "school"), nested = TRUE)
  )

  w <- squished_warnings(degroup(d_three, select = "v", by = "sch/stu/cls"))
  expect_length(w, 1)
  expect_match(w, "`stu` is nested in `cls`", fixed = TRUE)
  expect_no_warning(degroup(d_three, select = "v", by = "sch/cls/stu"))
})


test_that("degroup, crossed components on unbalanced data follow Guo et al.", {
  # persons crossed with items, with 1 or 2 rows per cell:
  # (p1,i1) 2, (p1,i2) 1, (p2,i1) 1, (p2,i2) 1, (p3,i1) 1, (p3,i2) 2
  d <- data.frame(
    person = c("p1", "p1", "p1", "p2", "p2", "p3", "p3", "p3"),
    item = c("i1", "i1", "i2", "i1", "i2", "i2", "i2", "i1"),
    x = c(2, 4, 9, 5, 1, 6, 10, 3),
    stringsAsFactors = FALSE
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

  # a missing x is left out of the means, and its within component is NA;
  # a missing `by` value keeps the row's own value as the mean for that
  # variable
  d_na <- d
  d_na$x[2] <- NA
  d_na$person[8] <- NA
  out_na <- demean(d_na, select = "x", by = c("person", "item"), append = FALSE)
  # person p1 -> (2+9)/2 = 5.5; item i1 -> (2+5+3)/3 = 10/3
  expect_equal(out_na$x_person_between[1], 5.5)
  expect_equal(out_na$x_item_between[1], 10 / 3, tolerance = 1e-10)
  expect_true(is.na(out_na$x_within[2]))
  expect_identical(out_na$x_person_between[8], 3)

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

  # with another `center`, the cluster statistic replaces the mean:
  # person medians: p1 -> 4, p2 -> 3, p3 -> 6; item medians: i1 -> 3.5,
  # i2 -> 7.5
  out <- degroup(
    d,
    select = "x",
    by = c("person", "item"),
    center = "median",
    append = FALSE
  )
  expected_person <- c(4, 4, 4, 3, 3, 6, 6, 6)
  expected_item <- c(3.5, 3.5, 7.5, 3.5, 7.5, 7.5, 7.5, 3.5)
  expect_equal(out$x_person_between, expected_person, ignore_attr = TRUE)
  expect_equal(out$x_item_between, expected_item, ignore_attr = TRUE)
  expect_equal(
    out$x_within,
    d$x - expected_person - expected_item,
    ignore_attr = TRUE
  )
})


test_that("degroup, three crossed variables follow Guo et al.", {
  d <- data.frame(
    person = c("p1", "p1", "p2", "p2", "p3", "p3", "p1", "p2", "p3"),
    item = c("i1", "i2", "i1", "i2", "i1", "i2", "i2", "i1", "i1"),
    day = c("d1", "d2", "d2", "d1", "d1", "d2", "d1", "d2", "d2"),
    x = c(2, 7, 4, 1, 9, 3, 5, 8, 6),
    stringsAsFactors = FALSE
  )
  out <- demean(
    d,
    select = "x",
    by = c("person", "item", "day"),
    append = FALSE
  )

  # Oracle (reference implementation): the steps of `cc.gmc()` in Guo et al.
  # 2024, Appendix C, which footnote 6 extends to any number of crossed
  # variables: subtract the mean of each crossed variable from x
  d$row <- seq_len(nrow(d))
  ref <- d
  for (j in c("person", "item", "day")) {
    mn <- stats::aggregate(d["x"], d[j], mean, na.rm = TRUE)
    names(mn)[2] <- paste0("x_", j)
    ref <- merge(ref, mn, by = j)
  }
  ref <- ref[order(ref$row), ]
  ref$x_dev <- ref$x - ref$x_person - ref$x_item - ref$x_day

  expect_equal(out$x_person_between, ref$x_person, ignore_attr = TRUE)
  expect_equal(out$x_item_between, ref$x_item, ignore_attr = TRUE)
  expect_equal(out$x_day_between, ref$x_day, ignore_attr = TRUE)
  expect_equal(out$x_within, ref$x_dev, ignore_attr = TRUE)

  # Oracle (closed-form) for the first row: person p1 -> (2+7+5)/3 = 14/3,
  # item i1 -> (2+4+9+8+6)/5 = 29/5, day d1 -> (2+1+9+5)/4 = 17/4, so
  # within = 2 - 14/3 - 29/5 - 17/4
  expect_equal(
    out$x_within[1],
    2 - 14 / 3 - 29 / 5 - 17 / 4,
    ignore_attr = TRUE
  )
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


test_that("degroup, a function in `center`", {
  trim_mean <- function(v) mean(v, trim = 0.2)
  set.seed(42)
  d <- data.frame(
    A = rep(1:3, each = 10),
    B = rep(1:2, 15),
    C = sample.int(4, 30, replace = TRUE),
    x = rnorm(30)
  )
  d$x[c(3, 17)] <- NA

  # Oracle (closed-form): the trimmed mean of the non-missing values of each
  # group, computed with `stats::ave()`
  group_stat <- function(...) {
    stats::ave(d$x, ..., FUN = function(v) trim_mean(v[!is.na(v)]))
  }

  # one `by` variable
  out <- degroup(d, select = "x", by = "A", center = trim_mean, append = FALSE)
  expect_equal(out$x_between, group_stat(d$A), ignore_attr = TRUE)
  expect_equal(out$x_within, d$x - group_stat(d$A), ignore_attr = TRUE)
  # the function is used: the trimmed mean is not the mean here
  expect_false(isTRUE(all.equal(
    out$x_between,
    demean(d, select = "x", by = "A", append = FALSE)$x_between
  )))

  # two crossed `by` variables
  out <- degroup(
    d,
    select = "x",
    by = c("A", "C"),
    center = trim_mean,
    append = FALSE
  )
  expect_equal(out$x_A_between, group_stat(d$A), ignore_attr = TRUE)
  expect_equal(out$x_C_between, group_stat(d$C), ignore_attr = TRUE)
  expect_equal(
    out$x_within,
    d$x - group_stat(d$A) - group_stat(d$C),
    ignore_attr = TRUE
  )

  # two nested `by` variables
  out <- degroup(d, select = "x", by = "A/B", center = trim_mean, append = FALSE)
  expect_equal(out$x_A_between, group_stat(d$A), ignore_attr = TRUE)
  expect_equal(
    out$x_B_between,
    group_stat(d$A, d$B) - group_stat(d$A),
    ignore_attr = TRUE
  )
  expect_equal(out$x_within, d$x - group_stat(d$A, d$B), ignore_attr = TRUE)

  # Oracle (invariant): a function and the string that names the same
  # statistic give the same result
  for (by in list("A", c("A", "C"), "A/B")) {
    expect_identical(
      degroup(d, select = "x", by = by, center = stats::median),
      degroup(d, select = "x", by = by, center = "median")
    )
  }
})


test_that("degroup, a function in `center` must return a single number", {
  d <- data.frame(g = rep(1:2, each = 3), x = c(1, 2, 4, 3, 5, 9))
  expect_error(
    degroup(d, select = "x", by = "g", center = range),
    "must return a single number",
    fixed = TRUE
  )
  expect_error(
    degroup(d, select = "x", by = "g", center = function(v) "a"),
    "must return a single number",
    fixed = TRUE
  )

  # a group with only missing values: the function returns `NA`, no error
  d$x[d$g == 2] <- NA
  mean_or_na <- function(v) if (length(v)) mean(v) else NA
  expect_no_error(degroup(d, select = "x", by = "g", center = mean_or_na))
  out <- degroup(d, select = "x", by = "g", center = mean_or_na, append = FALSE)
  expect_equal(
    out$x_between,
    c(7 / 3, 7 / 3, 7 / 3, NA, NA, NA),
    ignore_attr = TRUE
  )
})


test_that("degroup, `grand_center = TRUE` subtracts the mean of each component", {
  trim_mean <- function(v) mean(v, trim = 0.2)
  set.seed(7)
  d <- data.frame(
    A = rep(1:3, each = 8),
    B = rep(1:4, 6),
    C = sample.int(2, 24, replace = TRUE),
    x = as.numeric(sample.int(5, 24, replace = TRUE))
  )
  d$x[c(2, 13)] <- NA
  rows <- !is.na(d$x)

  for (center in list("mean", "median", "mode", trim_mean)) {
    for (by in list("A", c("A", "B", "C"), "A/B")) {
      args <- list(d, select = "x", by = by, center = center, append = FALSE)
      base <- do.call(degroup, c(args, verbose = FALSE))
      out <- do.call(degroup, c(args, verbose = FALSE, grand_center = TRUE))
      expect_named(out, names(base))
      for (col in names(base)) {
        # Oracle (closed-form): the column minus its mean over the rows where
        # x is not missing
        expect_equal(
          out[[col]],
          base[[col]] - mean(base[[col]][rows]),
          ignore_attr = TRUE
        )
        # Oracle (invariant): the grand-centered column has mean 0
        expect_equal(mean(out[[col]][rows]), 0, tolerance = 1e-10)
      }
    }
  }

  # a product column is centered over the rows where the product is not
  # missing, not over the rows where x is not missing
  d$w <- rep(c(1, 2, 3), 8)
  d$w[5] <- NA
  base <- degroup(d, select = "x*w", by = "A", append = FALSE)
  out <- degroup(d, select = "x*w", by = "A", append = FALSE, grand_center = TRUE)
  rows_xw <- !is.na(d$x * d$w)
  expect_equal(
    out$x_w_between,
    base$x_w_between - mean(base$x_w_between[rows_xw]),
    ignore_attr = TRUE
  )
  expect_false(isTRUE(all.equal(
    out$x_w_between,
    base$x_w_between - mean(base$x_w_between[rows])
  )))

  # Oracle (closed-form, issue #637): balanced crossed data whose effects sum
  # to 0, so mean(y_within) = mean(y) - 3 * mean(y) = -2 * 100
  mu <- 100
  ul <- stats::setNames(c(-1, -3, 0, 4), nm = letters[1:4])
  uL <- stats::setNames(c(10, 30, 0, -40), nm = LETTERS[1:4])
  um <- stats::setNames(c(100, 150, -250), nm = month.abb[1:3])
  dat <- expand.grid(l = letters[1:4], L = LETTERS[1:4], m = month.abb[1:3])
  set.seed(111)
  e <- round(rnorm(nrow(dat) - 1), 2)
  e <- append(e, -sum(e))
  dat$y <- mu + ul[dat$l] + uL[dat$L] + um[dat$m] + e
  dat_dem <- demean(dat, by = c("l", "L", "m"), select = "y")
  expect_equal(mean(dat_dem$y_within), -200, tolerance = 1e-8)
  dat_dem <- demean(dat, by = c("l", "L", "m"), select = "y", grand_center = TRUE)
  expect_equal(mean(dat_dem$y_within), 0, tolerance = 1e-8)
})


test_that("degroup, `grand_center` with names centers only those components", {
  set.seed(3)
  d <- data.frame(
    ID = rep(1:4, each = 5),
    sub_ID = rep(1:5, 4),
    `A B` = rep(1:2, 10),
    x = rnorm(20),
    check.names = FALSE
  )

  changed <- function(by, grand_center, ...) {
    base <- degroup(d, select = "x", by = by, append = FALSE, ...)
    out <- degroup(
      d,
      select = "x",
      by = by,
      append = FALSE,
      grand_center = grand_center,
      ...
    )
    expect_named(out, names(base))
    names(base)[!vapply(
      names(base),
      function(i) isTRUE(all.equal(out[[i]], base[[i]])),
      logical(1)
    )]
  }

  # "ID" names only the ID column, although "_ID_between" also ends the
  # sub_ID column
  expect_identical(changed(c("ID", "sub_ID"), "ID"), "x_ID_between")
  expect_identical(changed(c("ID", "sub_ID"), "within"), "x_within")
  expect_identical(
    sort(changed(c("ID", "sub_ID"), c("within", "sub_ID"))),
    c("x_sub_ID_between", "x_within")
  )
  # with one `by` variable, its name names the `_between` column
  expect_identical(changed("ID", "ID"), "x_between")
  # a column name that `as.data.frame()` changes
  expect_identical(changed(c("A B", "ID"), "A B"), "x_A.B_between")
  # "within" names the de-meaned columns, whatever `suffix_demean` is
  # (crossed, so that the de-meaned column does not already have mean 0)
  expect_identical(
    changed(c("ID", "sub_ID"), "within", suffix_demean = "_w"),
    "x_w"
  )
  # an empty vector is the same as FALSE
  expect_identical(changed(c("ID", "sub_ID"), character(0)), character(0))
  # the named column changes as with `grand_center = TRUE`
  expect_identical(
    degroup(d, select = "x", by = c("ID", "sub_ID"), grand_center = "ID")$x_ID_between,
    degroup(d, select = "x", by = c("ID", "sub_ID"), grand_center = TRUE)$x_ID_between
  )

  # errors
  expect_error(
    degroup(d, select = "x", by = "ID", grand_center = "foo"),
    "`grand_center` can only name these components: \"within\" and \"ID\"",
    fixed = TRUE
  )
  expect_error(
    degroup(d, select = "x", by = "ID", grand_center = NA_character_),
    "`grand_center` can only name these components",
    fixed = TRUE
  )
  d$within <- d$ID
  expect_error(
    degroup(d, select = "x", by = "within", grand_center = "within"),
    "A `by` variable is named \"within\"",
    fixed = TRUE
  )
  expect_error(
    degroup(d, select = "x", by = "ID", grand_center = 1),
    "`grand_center` must be `TRUE`, `FALSE`, or a character vector",
    fixed = TRUE
  )
})
