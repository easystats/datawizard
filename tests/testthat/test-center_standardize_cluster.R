# unbalanced two-level data from #672: five schools, one funding value per school
school <- rep(1:5, times = c(2, 5, 10, 50, 100))
funding <- rep(c(10, 20, 30, 40, 50), times = c(2, 5, 10, 50, 100))
# one value per cluster, in order of first appearance
funding_l2 <- funding[!duplicated(school)]
expand <- function(z) as.vector(z)[match(school, unique(school))]


# numeric methods -------------------------------------------

test_that("center, cluster", {
  z <- center(funding, cluster = school)
  # oracle (closed-form): mean(c(10, 20, 30, 40, 50)) = 150 / 5 = 30
  expect_identical(attr(z, "center"), 30)
  expect_identical(attr(z, "scale"), 1)
  expect_equal(as.vector(z), funding - 30, ignore_attr = TRUE)
  # without cluster, the mean is weighted by cluster size
  expect_equal(attr(center(funding), "center"), mean(funding))
  expect_false(isTRUE(all.equal(attr(center(funding), "center"), 30)))

  # oracle (invariant): same as centering the cluster-level vector
  ref <- center(funding_l2)
  expect_identical(attr(z, "center"), attr(ref, "center"))
  expect_equal(as.vector(z), expand(ref))
})

test_that("standardize, cluster", {
  z <- standardize(funding, cluster = school)
  # oracle (closed-form): deviations from 30 are -20, -10, 0, 10, 20,
  # squares sum to 400 + 100 + 0 + 100 + 400 = 1000, and 1000 / (5 - 1) = 250
  expect_identical(attr(z, "center"), 30)
  expect_equal(attr(z, "scale"), sqrt(250))
  expect_equal(as.vector(z), (funding - 30) / sqrt(250))

  # oracle (invariant): same as standardizing the cluster-level vector
  ref <- standardize(funding_l2)
  expect_identical(attr(z, "center"), attr(ref, "center"))
  expect_identical(attr(z, "scale"), attr(ref, "scale"))
  expect_equal(as.vector(z), expand(ref))
})

test_that("center and standardize, cluster, robust", {
  zc <- center(funding, cluster = school, robust = TRUE)
  zs <- standardize(funding, cluster = school, robust = TRUE)
  # oracle (closed-form): median is 30, absolute deviations are
  # 20, 10, 0, 10, 20 with median 10, so MAD = 1.4826 * 10
  expect_identical(attr(zc, "center"), 30)
  expect_identical(attr(zs, "center"), 30)
  expect_equal(attr(zs, "scale"), 1.4826 * 10)

  # oracle (invariant)
  ref_c <- center(funding_l2, robust = TRUE)
  ref_s <- standardize(funding_l2, robust = TRUE)
  expect_identical(attr(zs, "scale"), attr(ref_s, "scale"))
  expect_equal(as.vector(zc), expand(ref_c))
  expect_equal(as.vector(zs), expand(ref_s))
})

test_that("standardize, cluster, two_sd", {
  z <- standardize(funding, cluster = school, two_sd = TRUE)
  # the scale attribute keeps the SD, the values are divided by twice the SD
  expect_equal(attr(z, "scale"), sqrt(250))
  expect_equal(as.vector(z), (funding - 30) / (2 * sqrt(250)))

  # oracle (invariant)
  ref <- standardize(funding_l2, two_sd = TRUE)
  expect_identical(attr(z, "scale"), attr(ref, "scale"))
  expect_equal(as.vector(z), expand(ref))
})

test_that("cluster, factor and character ids give the same result", {
  expect_identical(
    standardize(funding, cluster = as.character(school)),
    standardize(funding, cluster = school)
  )
  expect_identical(
    standardize(funding, cluster = factor(school, levels = 5:1)),
    standardize(funding, cluster = school)
  )
})

test_that("cluster, one row per cluster gives the result without cluster", {
  expect_identical(
    standardize(funding_l2, cluster = 1:5),
    standardize(funding_l2)
  )
  expect_identical(center(funding_l2, cluster = 1:5), center(funding_l2))
})

test_that("cluster, factor input with force = TRUE", {
  f <- factor(funding)
  z <- standardize(f, cluster = school, force = TRUE)
  # factor levels 10, 20, ..., 50 are numeric, so they convert to their values
  expect_equal(as.vector(z), (funding - 30) / sqrt(250))
  z <- center(f, cluster = school, force = TRUE)
  expect_equal(as.vector(z), funding - 30)
})

test_that("cluster, NA ids are excluded and returned as NA", {
  x <- c(funding, 1000)
  cl <- c(school, NA)
  z <- standardize(x, cluster = cl)
  expect_identical(attr(z, "center"), 30)
  expect_equal(attr(z, "scale"), sqrt(250))
  expect_true(is.na(z[length(z)]))
  expect_equal(as.vector(z)[seq_along(funding)], (funding - 30) / sqrt(250))

  z <- center(x, cluster = cl)
  expect_identical(attr(z, "center"), 30)
  expect_true(is.na(z[length(z)]))
})

test_that("cluster, NA in x does not count as a second value", {
  x <- c(funding, NA)
  z <- standardize(x, cluster = c(school, 1))
  expect_identical(attr(z, "center"), 30)
  expect_true(is.na(z[length(z)]))
})

test_that("standardize, cluster, all clusters with the same value", {
  expect_warning(
    expect_message(
      z <- standardize(rep(3, 6), cluster = rep(1:3, each = 2)),
      "only one unique value"
    ),
    "SD is 0"
  )
  expect_equal(as.vector(z), rep(0, 6))
})


# errors, numeric methods -------------------------------------------

for (fun_name in c("center", "standardize")) {
  fun <- get(fun_name)
  test_that(paste(fun_name, "cluster errors, numeric"), {
    expect_error(
      fun(c(1, 2, 3, 4), cluster = c(1, 1, 2, 2)),
      "more than one value within a cluster"
    )
    expect_error(
      fun(funding, cluster = school, weights = rep(1, length(funding))),
      "`cluster` cannot be combined with `weights`"
    )
    expect_error(
      fun(funding, cluster = school, reference = funding),
      "`cluster` cannot be combined with `reference`"
    )
    expect_error(
      fun(funding, cluster = school, center = 0),
      "`cluster` cannot be combined with `reference`"
    )
    expect_error(
      fun(funding, cluster = school[-1]),
      "`cluster` must have the same length"
    )
    expect_error(
      fun(c(1, 1, 1), cluster = c(1, 1, 1), verbose = FALSE),
      "at least two clusters"
    )
    expect_error(
      fun(c(1, 2, 3), cluster = c(NA, NA, NA)),
      "at least two clusters"
    )
  })
}

test_that("cluster errors, standardize scale", {
  expect_error(
    standardize(funding, cluster = school, scale = 2),
    "`cluster` cannot be combined with `reference`"
  )
})


# data frame methods -------------------------------------------

d <- data.frame(school = school, funding = funding, budget = funding * 3)

for (fun_name in c("center", "standardize")) {
  fun <- get(fun_name)

  test_that(paste(fun_name, "data frame, cluster as column name or vector"), {
    out <- fun(d, cluster = "school")
    expect_identical(out$school, d$school)
    expect_equal(
      out$funding,
      as.vector(fun(funding, cluster = school)),
      ignore_attr = TRUE
    )
    expect_equal(
      out$budget,
      as.vector(fun(d$budget, cluster = school)),
      ignore_attr = TRUE
    )
    out_vector <- fun(d, select = "funding", cluster = school)
    expect_identical(out_vector$funding, out$funding)
    expect_identical(out_vector$budget, d$budget)

    out <- fun(d, cluster = school, robust = TRUE)
    expect_equal(
      out$funding,
      as.vector(fun(funding, cluster = school, robust = TRUE)),
      ignore_attr = TRUE
    )
  })

  test_that(paste(fun_name, "data frame, the cluster column is not transformed"), {
    out <- fun(d, cluster = "school", append = TRUE)
    expect_identical(out$school, d$school)
    expect_false("school_c" %in% names(out) || "school_z" %in% names(out))
    expect_named(out, c(names(d), paste0(c("funding", "budget"), if (fun_name == "center") "_c" else "_z")))

    out <- fun(d, select = c("school", "funding"), cluster = "school")
    expect_identical(out$school, d$school)

    d_f <- d
    d_f$school <- factor(d_f$school)
    out <- fun(d_f, cluster = "school", force = TRUE)
    expect_identical(out$school, d_f$school)
    expect_equal(
      out$funding,
      as.vector(fun(funding, cluster = school)),
      ignore_attr = TRUE
    )
  })

  test_that(paste(fun_name, "data frame, vector cluster loses the rows remove_na drops"), {
    d_na <- d
    d_na$budget[1] <- NA
    out <- fun(d_na, cluster = school, remove_na = "all")
    expect_identical(nrow(out), nrow(d) - 1L)
    expect_equal(
      out$funding,
      as.vector(fun(funding[-1], cluster = school[-1])),
      ignore_attr = TRUE
    )
    out_name <- fun(d_na, cluster = "school", remove_na = "all")
    expect_identical(out_name$funding, out$funding)
  })
}


# grouped data frames -------------------------------------------

g <- data.frame(
  region = rep(c("a", "b"), times = c(17, 51)),
  school = rep(1:6, times = c(2, 5, 10, 3, 8, 40)),
  funding = rep(c(10, 20, 60, 5, 15, 25), times = c(2, 5, 10, 3, 8, 40))
)

for (fun_name in c("center", "standardize")) {
  fun <- get(fun_name)

  test_that(paste(fun_name, "grouped data frame, cluster within each group"), {
    skip_if_not_installed("poorman")
    out <- g |>
      poorman::group_by(region) |>
      fun(select = "funding", cluster = "school") |>
      poorman::ungroup()
    a <- g$region == "a"
    expected <- c(
      as.vector(fun(g$funding[a], cluster = g$school[a])),
      as.vector(fun(g$funding[!a], cluster = g$school[!a]))
    )
    expect_equal(out$funding, expected, ignore_attr = TRUE)
    expect_identical(out$school, g$school)

    out <- g |>
      poorman::group_by(region) |>
      fun(cluster = "school", append = TRUE) |>
      poorman::ungroup()
    expect_identical(out$school, g$school)
    expect_false(any(c("school_c", "school_z") %in% names(out)))
  })
}


# errors, data frame methods -------------------------------------------

for (fun_name in c("center", "standardize")) {
  fun <- get(fun_name)

  test_that(paste(fun_name, "cluster errors, data frame"), {
    d_var <- data.frame(school = c(1, 1, 2, 2), y = c(1, 2, 3, 4))
    expect_error(
      fun(d_var, cluster = "school"),
      "more than one value within a cluster"
    )
    expect_error(
      fun(d, cluster = "school", weights = "budget"),
      "`cluster` cannot be combined with `weights`"
    )
    expect_error(
      fun(d, cluster = "school", reference = d),
      "`cluster` cannot be combined with `reference`"
    )
    expect_error(
      fun(d, cluster = "school", center = 0),
      "`cluster` cannot be combined with `reference`"
    )
    expect_error(
      fun(d, cluster = school[-1]),
      "`cluster` must have the same length"
    )
    expect_error(
      fun(d, cluster = "district"),
      "Could not find the `cluster` column"
    )
    expect_error(
      fun(d[d$school == 1, ], cluster = "school", verbose = FALSE),
      "at least two clusters"
    )
  })

  test_that(paste(fun_name, "cluster errors, grouped data frame"), {
    skip_if_not_installed("poorman")
    gd <- poorman::group_by(g, region)
    g_var <- poorman::group_by(
      data.frame(region = "a", school = c(1, 1, 2, 2), y = c(1, 2, 3, 4)),
      region
    )
    expect_error(
      fun(g_var, select = "y", cluster = "school"),
      "more than one value within a cluster"
    )
    expect_error(
      fun(gd, select = "funding", cluster = "school", weights = "school"),
      "`cluster` cannot be combined with `weights`"
    )
    expect_error(
      fun(gd, select = "funding", cluster = "school", reference = g),
      "`cluster` cannot be combined with `reference`"
    )
    expect_error(
      fun(gd, select = "funding", cluster = "school", center = 0),
      "`cluster` cannot be combined with `reference`"
    )
    expect_error(
      fun(gd, select = "funding", cluster = "district"),
      "Could not find the `cluster` column"
    )
    expect_error(
      fun(gd, select = "funding", cluster = g$school),
      "`cluster` must be a column name"
    )
    g_one <- poorman::group_by(g[g$school %in% c(1, 2, 4), ], region)
    expect_error(
      fun(g_one, select = "funding", cluster = "school", verbose = FALSE),
      "at least two clusters"
    )
  })
}
