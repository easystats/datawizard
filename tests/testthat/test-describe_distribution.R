skip_if_not_installed("bayestestR")

# numeric ---------------------------------------

test_that("describe_distribution - numeric: works with basic numeric vector", {
  x <- describe_distribution(mtcars$mpg)
  expect_identical(dim(x), c(1L, 9L))
  expect_identical(round(x$Mean), 20)
})

test_that("describe_distribution - numeric: correctly handles missing values", {
  no_missing <- describe_distribution(mtcars$mpg)
  test <- mtcars$mpg
  test[1] <- NA
  with_missing <- describe_distribution(test)
  expect_identical(with_missing$n, 31L)
  expect_identical(with_missing$n_Missing, 1L)
  expect_false(with_missing$Mean == no_missing$Mean)
})

test_that("describe_distribution - numeric: works with quartiles", {
  x <- describe_distribution(
    mtcars$mpg,
    indices = c(
      "dispersion",
      "iqr",
      "range",
      "quartiles",
      "skewness",
      "kurtosis"
    )
  )
  expect_identical(dim(x), c(1L, 11L))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - numeric: works with range", {
  x <- describe_distribution(
    mtcars$mpg,
    indices = c("dispersion", "iqr", "skewness", "kurtosis")
  )
  expect_identical(dim(x), c(1L, 7L))
  expect_false("Min" %in% names(x))
  expect_false("Max" %in% names(x))
})

test_that("describe_distribution - NULL for date", {
  v <- as.Date(c("2022-01-01", "2022-01-02"))
  expect_warning(expect_null(describe_distribution(v)))
})


# data frame ---------------------------------------

test_that("describe_distribution - data frame: works with basic data frame", {
  x <- describe_distribution(mtcars)
  expect_identical(dim(x), c(11L, 10L))
  expect_identical(round(x[1, "Mean"]), 20)
})

test_that("describe_distribution - data frame: correctly handles missing values", {
  no_missing <- describe_distribution(mtcars)
  test <- mtcars
  test[1, ] <- NA
  with_missing <- describe_distribution(test)
  expect_identical(unique(with_missing$n), 31L)
  expect_identical(unique(with_missing$n_Missing), 1L)
  expect_false(unique(with_missing$Mean == no_missing$Mean))
})

test_that("describe_distribution - data frame: works with quartiles", {
  x <- describe_distribution(
    mtcars,
    indices = c(
      "dispersion",
      "iqr",
      "range",
      "quartiles",
      "skewness",
      "kurtosis"
    )
  )
  expect_identical(dim(x), c(11L, 12L))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - data frame: works with range", {
  x <- describe_distribution(
    mtcars,
    indices = c("dispersion", "iqr", "skewness", "kurtosis")
  )
  expect_identical(dim(x), c(11L, 8L))
  expect_false("Min" %in% names(x))
  expect_false("Max" %in% names(x))
})


# factor ---------------------------------------

test_that("describe_distribution - factor", {
  expect_snapshot(describe_distribution(factor(substring(
    "statistics",
    1:10,
    1:10
  ))))
})


# character ---------------------------------------

test_that("describe_distribution - character", {
  expect_snapshot(describe_distribution(as.character(ToothGrowth$supp)))
})


# list ---------------------------------------

test_that("describe_distribution - list: works with basic list", {
  x <- list(mtcars$mpg, mtcars$cyl)
  stored <- describe_distribution(x)
  unnamed <- describe_distribution(list(mtcars$mpg, mtcars$cyl))
  named <- describe_distribution(list(foo = mtcars$mpg, foo2 = mtcars$cyl))
  mix <- describe_distribution(list(foo = mtcars$mpg, mtcars$cyl))

  expect_identical(dim(stored), c(2L, 10L))
  expect_identical(round(stored$Mean), c(20, 6))
  expect_identical(dim(unnamed), c(2L, 10L))
  expect_identical(round(unnamed$Mean), c(20, 6))
  expect_identical(dim(named), c(2L, 10L))
  expect_identical(round(named$Mean), c(20, 6))
  expect_identical(dim(mix), c(2L, 10L))
  expect_identical(round(mix$Mean), c(20, 6))
})

test_that("describe_distribution - list: works with include_factors", {
  x1 <- describe_distribution(list(mtcars$mpg, factor(mtcars$cyl)))
  y <- describe_distribution(list(mtcars$mpg))
  expect_identical(x1, y)

  x2 <- describe_distribution(
    list(mtcars$mpg, factor(mtcars$cyl)),
    include_factors = TRUE
  )
  expect_identical(dim(x2), c(2L, 10L))
  expect_identical(x2$Variable, c("mtcars$mpg", "factor(mtcars$cyl)"))

  x3 <- describe_distribution(
    list(mtcars$mpg, foo = factor(mtcars$cyl)),
    include_factors = TRUE
  )
  expect_identical(dim(x3), c(2L, 10L))
  expect_identical(x3$Variable, c("mtcars$mpg", "foo"))
})

test_that("describe_distribution - list: correctly removes character elements", {
  x <- describe_distribution(list(mtcars$mpg, "something"))
  y <- describe_distribution(list(mtcars$mpg))
  expect_identical(x, y)
})

test_that("describe_distribution - list: correctly handles variable names", {
  x <- list(mtcars$mpg, mtcars$cyl)
  stored <- describe_distribution(x)
  unnamed <- describe_distribution(list(mtcars$mpg, mtcars$cyl))
  named <- describe_distribution(list(foo = mtcars$mpg, foo2 = mtcars$cyl))
  mix <- describe_distribution(list(foo = mtcars$mpg, mtcars$cyl))

  expect_identical(stored$Variable, c("Var_1", "Var_2"))
  expect_identical(unnamed$Variable, c("mtcars$mpg", "mtcars$cyl"))
  expect_identical(named$Variable, c("foo", "foo2"))
  expect_identical(mix$Variable, c("foo", "mtcars$cyl"))
})

test_that("describe_distribution - list: correctly handles missing values", {
  no_missing <- describe_distribution(list(mtcars$mpg, mtcars$cyl))
  test <- mtcars$mpg
  test2 <- mtcars$cyl
  test[1] <- NA
  test2[1] <- NA
  with_missing <- describe_distribution(list(test, test2))
  expect_identical(unique(with_missing$n), 31L)
  expect_identical(unique(with_missing$n_Missing), 1L)
  expect_false(unique(with_missing$Mean == no_missing$Mean))
})

test_that("describe_distribution - list: works with quartiles", {
  x <- describe_distribution(
    list(mtcars$mpg, mtcars$cyl),
    indices = c(
      "dispersion",
      "iqr",
      "range",
      "quartiles",
      "skewness",
      "kurtosis"
    )
  )
  expect_identical(dim(x), c(2L, 12L))
  expect_true("Q1" %in% names(x))
  expect_true("Q3" %in% names(x))
})

test_that("describe_distribution - list: works with range", {
  x <- describe_distribution(
    list(mtcars$mpg, mtcars$cyl),
    indices = c("dispersion", "iqr", "skewness", "kurtosis")
  )
  expect_identical(dim(x), c(2L, 8L))
  expect_false("Min" %in% names(x))
  expect_false("Max" %in% names(x))
})


# select ----------------------

test_that("describe_distribution - select", {
  data(iris)
  out <- describe_distribution(iris, select = starts_with("Petal"))

  expect_identical(out$Variable, c("Petal.Length", "Petal.Width"))
  expect_equal(out$Mean, c(3.758000, 1.199333), tolerance = 1e-3)

  expect_null(describe_distribution(iris, select = "Species"))
  out <- describe_distribution(iris, select = "Species", include_factors = TRUE)
  exp <- describe_distribution(iris$Species)
  expect_identical(out$Range, exp$Range)
})


# select and grouped df ----------------------

test_that("describe_distribution - grouped df", {
  data(iris)
  x <- data_group(iris, Species)
  out <- describe_distribution(x, select = starts_with("Petal"))

  expect_snapshot(out)
  expect_equal(
    out$Mean,
    c(1.462, 0.246, 4.26, 1.326, 5.552, 2.026),
    tolerance = 1e-3
  )
})

# Mostly to test printing
test_that("describe_distribution - grouped df and multiple groups", {
  x <- data.frame(
    grp1 = rep(letters[1:3], each = 20),
    grp2 = rep(letters[1:3], 20),
    values = 1:30
  )
  x <- data_group(x, c("grp1", "grp2"))
  expect_snapshot(describe_distribution(x))
})

test_that("argument 'by' works", {
  # basic
  grouped <- data_group(mtcars, c("am", "vs"))
  expect_identical(
    describe_distribution(grouped),
    describe_distribution(mtcars, by = c("am", "vs")),
    ignore_attr = TRUE
  )

  # mixing data_group() and arg 'by'
  grouped <- data_group(mtcars, c("am", "vs"))
  half_grouped <- data_group(mtcars, "am")
  expect_identical(
    describe_distribution(grouped),
    describe_distribution(half_grouped, by = "vs"),
    ignore_attr = TRUE
  )

  expect_error(
    describe_distribution(mtcars, by = 2),
    "must be a character vector"
  )
})

test_that("empty groups are discarded from the output, #608", {
  dat <- data.frame(
    grp1 = factor("a", levels = c("a", "b")),
    grp2 = factor(c("A", "B")),
    value = 1:2
  )
  dat <- data_group(dat, c("grp1", "grp2"))
  expect_no_error(
    suppressWarnings(describe_distribution(dat, ci = 0.95))
  )
})

# distribution_mode --------------------------
test_that("distribution_mode works as expected", {
  # atomic vector
  expect_equal(distribution_mode(c(1, 2, 3, 3, 4, 5)), 3, ignore_attr = TRUE)
  expect_equal(
    distribution_mode(c(1, 2, 3, 3, 4, 4, 5), verbose = FALSE),
    3,
    ignore_attr = TRUE
  )
  expect_equal(
    distribution_mode(c(1.5, 2.3, 3.7, 3.7, 4.0, 5)),
    3.7,
    ignore_attr = TRUE
  )

  # list
  expect_identical(distribution_mode(list(1, 2, 3, 3, 4, 5)), list(3))

  # scalar
  expect_identical(distribution_mode("a"), "a")

  # empty
  expect_null(distribution_mode(NULL))

  # message for tied mode values
  data(iris) #
  expect_message(
    {
      out <- distribution_mode(iris$Species)
    },
    regex = "Multiple modes detected"
  )
  expect_equal(as.character(out), "setosa", ignore_attr = TRUE)
  expect_equal(
    as.character(attributes(out)$tied_values),
    c("setosa", "versicolor", "virginica")
  )
  expect_silent(distribution_mode(iris$Species, verbose = FALSE))
})

# select helpers ------------------------------
test_that("describe_distribution regex", {
  expect_equal(
    describe_distribution(mtcars, select = "pg", regex = TRUE),
    describe_distribution(mtcars, select = "mpg"),
    ignore_attr = TRUE
  )
})

# formatting ------------------------------
test_that("describe_distribution formatting", {
  data(iris)
  x <- describe_distribution(
    iris$Sepal.Width,
    indices = c(
      "dispersion",
      "iqr",
      "range",
      "quartiles",
      "skewness",
      "kurtosis"
    )
  )
  expect_snapshot(format(x))
})

# other -----------------------------------
test_that("return NA in CI if sample is too sparse", {
  set.seed(123456)
  expect_silent(expect_message(
    {
      res <- describe_distribution(
        mtcars[mtcars$cyl == "6", ],
        wt,
        centrality = "map",
        ci = 0.95
      )
    },
    regex = "Bootstrapping"
  ))
  expect_equal(res$CI_low_map, 2.6462, tolerance = 1e-2)
  expect_equal(res$CI_high_map, 3.4531, tolerance = 1e-2)

  x <- c(
    2.5,
    2.5,
    2.5,
    2.5,
    2.5,
    2.5,
    2.2,
    2.2,
    2.2,
    2.5,
    2.5,
    2.5,
    2.5,
    2.5,
    2.5
  )
  expect_message(
    {
      out <- describe_distribution(x, centrality = "map")
    },
    regex = "Could not calculate"
  )
  expect_identical(out$MAP, NA_real_)
  expect_silent(describe_distribution(x, centrality = "map", verbose = FALSE))
})

# check for reserved column names
test_that("errors on invalid column names (reserved word)", {
  data(mtcars)

  out <- data_to_long(
    mtcars,
    cols = 1:3,
    names_to = "Variable",
    values_to = "Values"
  )
  out <- data_group(out, c("gear", "Variable"))
  expect_error(
    describe_distribution(out, select = "Values"),
    regex = "Following variable names are reserved"
  )

  out <- data_to_long(
    mtcars,
    cols = 1:3,
    names_to = "Variable",
    values_to = "Values"
  )
  expect_error(
    describe_distribution(out, select = "Variable"),
    regex = "Following variable names are reserved"
  )
})

# multiple centralities
test_that("multiple centralities work", {
  data(iris)
  out <- describe_distribution(
    iris,
    select = "Petal.Width",
    centrality = c("median", "mean")
  )
  expect_identical(dim(out), c(1L, 12L))
  expect_named(
    out,
    c(
      "Variable",
      "Median",
      "MAD",
      "Mean",
      "SD",
      "IQR",
      "Min",
      "Max",
      "Skewness",
      "Kurtosis",
      "n",
      "n_Missing"
    )
  )
  out <- describe_distribution(
    iris,
    select = "Petal.Width",
    centrality = list("median", "mean")
  )
  expect_identical(dim(out), c(1L, 12L))
  expect_named(
    out,
    c(
      "Variable",
      "Median",
      "MAD",
      "Mean",
      "SD",
      "IQR",
      "Min",
      "Max",
      "Skewness",
      "Kurtosis",
      "n",
      "n_Missing"
    )
  )
})


test_that("(multiple) centralities with CIs", {
  data(iris)
  x <- iris$Sepal.Width
  set.seed(123456)
  expect_message(
    {
      out <- describe_distribution(
        x,
        centrality = "all",
        ci = 0.95,
        iterations = 100
      )
    },
    regex = "For more stable intervals"
  )
  expect_named(
    out,
    c(
      "Median",
      "MAD",
      "Mean",
      "SD",
      "MAP",
      "IQR",
      "CI_low_mean",
      "CI_high_mean",
      "CI_low_median",
      "CI_high_median",
      "CI_low_MAP",
      "CI_high_MAP",
      "Min",
      "Max",
      "Skewness",
      "Kurtosis",
      "n",
      "n_Missing"
    )
  )
  expect_snapshot(print(out, table_width = Inf))
  expect_silent(describe_distribution(
    x,
    centrality = "all",
    ci = 0.95,
    iterations = 100,
    verbose = FALSE
  ))

  set.seed(123456)
  out <- describe_distribution(
    x,
    centrality = "mean",
    ci = 0.95,
    iterations = 100,
    verbose = FALSE
  )
  expect_named(
    out,
    c(
      "Mean",
      "SD",
      "IQR",
      "CI_low_mean",
      "CI_high_mean",
      "Min",
      "Max",
      "Skewness",
      "Kurtosis",
      "n",
      "n_Missing"
    )
  )
  expect_snapshot(print(out, table_width = Inf))

  set.seed(123456)
  out <- describe_distribution(
    x,
    centrality = c("MAP", "median"),
    ci = 0.95,
    iterations = 100,
    verbose = FALSE
  )
  expect_named(
    out,
    c(
      "Median",
      "MAD",
      "MAP",
      "IQR",
      "CI_low_MAP",
      "CI_high_MAP",
      "CI_low_median",
      "CI_high_median",
      "Min",
      "Max",
      "Skewness",
      "Kurtosis",
      "n",
      "n_Missing"
    )
  )
  expect_snapshot(print(out, table_width = Inf))

  # only one message for data frame
  expect_silent(expect_message(describe_distribution(iris, ci = 0.95)))
})


test_that("display() method exports to markdown", {
  skip_if_not_installed("knitr")
  data(iris)
  out <- describe_distribution(iris)
  expect_error(display(out, format = "invalid"), regex = "Invalid option")
  expect_snapshot(display(out))
})


test_that("display() method exports to tinytable", {
  skip_if_not_installed("tinytable")
  data(iris)
  out <- describe_distribution(iris)
  expect_snapshot(display(out, format = "tt"))
})


# indices -----------------------------------

dd_columns <- list(
  dispersion = "SD",
  iqr = "IQR",
  range = c("Min", "Max"),
  quartiles = c("Q1", "Q3"),
  skewness = "Skewness",
  kurtosis = "Kurtosis"
)
dd_optional <- c("SD", "MAD", unlist(dd_columns[-1L], use.names = FALSE))

dd_fixtures <- list(
  numeric = function(...) describe_distribution(mtcars$mpg, ...),
  factor = function(...) describe_distribution(factor(mtcars$cyl), ...),
  character = function(...) {
    describe_distribution(as.character(mtcars$cyl), ...)
  },
  data.frame = function(...) {
    describe_distribution(iris, include_factors = TRUE, ...)
  },
  grouped_df = function(...) {
    describe_distribution(
      data_group(mtcars, "cyl"),
      select = c("mpg", "wt"),
      ...
    )
  },
  list = function(...) {
    describe_distribution(
      list(mtcars$mpg, factor(mtcars$cyl)),
      include_factors = TRUE,
      ...
    )
  },
  by = function(...) {
    describe_distribution(mtcars, select = c("mpg", "wt"), by = "am", ...)
  }
)

dd_default <- function(name) {
  if (name %in% c("factor", "character")) {
    c("dispersion", "range", "skewness", "kurtosis")
  } else {
    c("dispersion", "iqr", "range", "skewness", "kurtosis")
  }
}

test_that("describe_distribution - indices: every subset on a numeric vector", {
  x <- mtcars$mpg
  x[3L] <- NA
  x_ok <- x[!is.na(x)]
  full <- describe_distribution(x, indices = names(dd_columns))

  # Oracle (closed form): each optional column equals the base R or
  # datawizard function of `x` without its NA, for example SD = sd(x_ok) and
  # IQR = IQR(x_ok, type = 6).
  expect_identical(full$SD, stats::sd(x_ok))
  expect_identical(full$IQR, stats::IQR(x_ok, type = 6L))
  expect_identical(full$Min, min(x_ok))
  expect_identical(full$Max, max(x_ok))
  expect_identical(full$Q1, unname(stats::quantile(x_ok, probs = 0.25)))
  expect_identical(full$Q3, unname(stats::quantile(x_ok, probs = 0.75)))
  expect_identical(full$Skewness, as.numeric(skewness(x_ok)))
  expect_identical(full$Kurtosis, as.numeric(kurtosis(x_ok)))

  subsets <- unlist(
    lapply(0L:6L, function(k) {
      utils::combn(names(dd_columns), k, simplify = FALSE)
    }),
    recursive = FALSE
  )
  expect_length(subsets, 64L)

  for (s in subsets) {
    keep <- c(
      "Mean",
      unlist(dd_columns[s], use.names = FALSE),
      "n",
      "n_Missing"
    )
    expected_names <- intersect(names(full), keep)
    out <- describe_distribution(x, indices = s)
    expect_named(out, expected_names)
    for (column in expected_names) {
      expect_identical(out[[column]], full[[column]])
    }
    expect_identical(describe_distribution(x, indices = rev(s)), out)
  }
})

test_that("describe_distribution - indices: optional columns of each method", {
  sets <- c(
    list(NULL, character(0L)),
    as.list(names(dd_columns)),
    list(names(dd_columns))
  )
  for (name in names(dd_fixtures)) {
    for (s in sets) {
      out <- dd_fixtures[[name]](indices = s)
      expect_identical(
        sort(intersect(names(out), dd_optional)),
        sort(as.character(unlist(dd_columns[s], use.names = FALSE))),
        info = paste(name, toString(s))
      )
    }
  }

  # printed output has no optional column
  printed_names <- c(
    "SD",
    "MAD",
    "IQR",
    "Range",
    "Quartiles",
    "Skewness",
    "Kurtosis"
  )
  for (name in c("numeric", "data.frame", "grouped_df")) {
    printed <- utils::capture.output(
      print(dd_fixtures[[name]](indices = NULL))
    )
    for (column in printed_names) {
      expect_false(
        any(grepl(paste0("\\b", column, "\\b"), printed)),
        info = paste(name, column)
      )
    }
  }
})

test_that("describe_distribution - deprecated flags warn and set indices", {
  # one deprecated flag, given to one fixture, with or without `indices`
  dd_check_flag <- function(name, flag, value, start) {
    call_args <- stats::setNames(list(value), flag)
    if (is.null(start)) {
      start_indices <- dd_default(name)
    } else {
      call_args$indices <- start
      start_indices <- start
    }
    info <- paste(name, flag, value, toString(start))
    caught <- testthat::capture_warnings(
      do.call(dd_fixtures[[name]], call_args)
    )
    expect_length(caught, 1L)
    expect_match(caught, paste0("`", flag, "`"), fixed = TRUE, info = info)
    expect_match(caught, "`indices`", fixed = TRUE, info = info)

    out <- suppressWarnings(do.call(dd_fixtures[[name]], call_args))
    if (value) {
      expected_indices <- union(start_indices, flag)
    } else {
      expected_indices <- setdiff(start_indices, flag)
    }
    expect_identical(
      out,
      dd_fixtures[[name]](indices = expected_indices),
      info = info
    )
  }

  settings <- expand.grid(
    name = names(dd_fixtures),
    flag = c("dispersion", "iqr", "range", "quartiles"),
    value = c(TRUE, FALSE),
    start = c("default", "skewness"),
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nrow(settings))) {
    dd_check_flag(
      settings$name[i],
      settings$flag[i],
      settings$value[i],
      if (settings$start[i] == "skewness") "skewness"
    )
  }
})

test_that("describe_distribution - invalid indices", {
  for (name in names(dd_fixtures)) {
    expect_error(
      dd_fixtures[[name]](indices = "foo"),
      regexp = "`indices`.*foo",
      info = name
    )
  }
  # no column is described here, but `indices` is still checked
  expect_error(
    describe_distribution(iris, select = "Species", indices = "foo"),
    regexp = "`indices`.*foo"
  )
  expect_error(
    describe_distribution(list("a"), indices = "foo"),
    regexp = "`indices`.*foo"
  )
  for (bad in list("IQR", c("iqr", "foo"), NA_character_, 1.0)) {
    expect_error(
      describe_distribution(mtcars$mpg, indices = bad),
      regexp = "`indices`",
      fixed = TRUE
    )
  }
  # an empty string is named in the message
  expect_error(
    describe_distribution(mtcars$mpg, indices = c("foo", "")),
    regexp = "`indices`: \"foo\", \"\".",
    fixed = TRUE
  )
})

test_that("describe_distribution - deprecated flags must be TRUE or FALSE", {
  for (bad in list(NA, "yes", "TRUE", 1, 0L, NULL, c(TRUE, FALSE))) {
    expect_error(
      describe_distribution(mtcars$mpg, range = bad),
      regexp = "`range` must be `TRUE` or `FALSE`.",
      fixed = TRUE
    )
    expect_error(
      describe_distribution(factor(1L:3L), iqr = bad),
      regexp = "`iqr` must be `TRUE` or `FALSE`.",
      fixed = TRUE
    )
  }
  # positional and partially matched flags still warn and apply
  expect_warning(
    describe_distribution(mtcars$mpg, "mean", FALSE),
    regexp = "`dispersion` is deprecated",
    fixed = TRUE
  )
  out <- suppressWarnings(describe_distribution(mtcars$mpg, "mean", FALSE))
  expect_false("SD" %in% names(out))
  expect_warning(
    describe_distribution(mtcars$mpg, quart = TRUE),
    regexp = "`quartiles` is deprecated",
    fixed = TRUE
  )
  out <- suppressWarnings(describe_distribution(mtcars$mpg, quart = TRUE))
  expect_true(all(c("Q1", "Q3") %in% names(out)))
})

test_that("describe_distribution - indices is the last formal before ...", {
  methods <- c(
    "list",
    "numeric",
    "factor",
    "character",
    "data.frame",
    "grouped_df"
  )
  for (method in methods) {
    arguments <- names(
      formals(utils::getS3method("describe_distribution", method))
    )
    expect_false("shape" %in% arguments, info = method)
    expect_identical(
      utils::tail(arguments, 2L),
      c("indices", "..."),
      info = method
    )
  }
})
