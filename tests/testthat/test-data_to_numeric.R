test_that("convert data frame to numeric", {
  expect_snapshot(to_numeric(head(ToothGrowth), dummy_factors = TRUE))
  expect_snapshot(to_numeric(head(ToothGrowth), dummy_factors = FALSE))
})

test_that("convert character to numeric", {
  expect_identical(to_numeric(c("xyz", "ab")), c(2, 1))
})

test_that("convert character to numeric Date", {
  expect_warning(expect_identical(to_numeric(as.Date("2022-01-01")), as.numeric(as.Date("2022-01-01"))))
  expect_warning(expect_identical(to_numeric(as.POSIXct("2022-01-01")), as.numeric(as.POSIXct("2022-01-01"))))
  expect_warning(expect_identical(to_numeric(as.POSIXlt("2022-01-01")), as.numeric(as.POSIXlt("2022-01-01"))))
})

test_that("convert character to numeric preserve levels", {
  x <- head(as.factor(mtcars$gear))
  expect_identical(
    to_numeric(x, dummy_factors = FALSE),
    c(2, 2, 2, 1, 1, 1)
  )
  expect_identical(
    to_numeric(x, dummy_factors = FALSE, preserve_levels = TRUE),
    c(4, 4, 4, 3, 3, 3)
  )
})

test_that("convert character to numeric lowest", {
  d <- head(mtcars)
  d$vs <- as.factor(d$vs)
  model <- glm(vs ~ wt + mpg, data = d, family = "binomial")
  expect_identical(
    to_numeric(insight::get_response(model), dummy_factors = FALSE),
    c(1, 1, 2, 2, 1, 2)
  )
  expect_identical(
    to_numeric(insight::get_response(model), dummy_factors = FALSE, lowest = 0),
    c(0, 0, 1, 1, 0, 1)
  )
})

test_that("convert factor to numeric", {
  f <- factor(substring("statistics", 1:10, 1:10))
  expect_snapshot(to_numeric(f, dummy_factors = TRUE))
})

test_that("convert factor to numeric", {
  expect_identical(to_numeric(c("abc", "xyz")), c(1, 2))
  expect_identical(to_numeric(c("123", "789")), c(123, 789))
  expect_identical(to_numeric(c("1L", "2e-3")), c(1, 0.002))
  expect_identical(to_numeric(c("1L", "2e-3", "ABC")), c(1, 2, 3))
})

test_that("convert factor to numeric, dummy factors", {
  expect_identical(
    to_numeric(c("abc", "xyz"), dummy_factors = TRUE),
    data.frame(abc = c(1, 0), xyz = c(0, 1)),
    ignore_attr = TRUE
  )
  expect_identical(
    to_numeric(c("1L", "2e-3", "ABC"), dummy_factors = TRUE),
    data.frame(`1L` = c(1, 0, 0), `2e-3` = c(0, 1, 0), ABC = c(0, 0, 1)),
    ignore_attr = TRUE
  )
})

test_that("convert factor to numeric, append", {
  data(efc)
  expect_identical(
    colnames(to_numeric(efc, dummy_factors = TRUE)),
    c("c12hour", "e16sex", "e42dep.1", "e42dep.2", "e42dep.3", "e42dep.4", "c172code", "neg_c_7"),
    ignore_attr = TRUE
  )
  expect_identical(
    colnames(to_numeric(efc, dummy_factors = TRUE, append = TRUE)),
    c(
      "c12hour", "e16sex", "e42dep", "c172code", "neg_c_7", "e42dep_n",
      "e42dep_n.1", "e42dep_n.2", "e42dep_n.3", "e42dep_n.4"
    ),
    ignore_attr = TRUE
  )
  expect_identical(
    colnames(to_numeric(efc, append = TRUE, dummy_factors = FALSE)),
    c("c12hour", "e16sex", "e42dep", "c172code", "neg_c_7", "e42dep_n"),
    ignore_attr = TRUE
  )
  expect_identical(
    colnames(to_numeric(efc, append = FALSE, dummy_factors = FALSE)),
    c("c12hour", "e16sex", "e42dep", "c172code", "neg_c_7"),
    ignore_attr = TRUE
  )
})

test_that("convert factor to numeric, all numeric", {
  data(mtcars)
  expect_identical(to_numeric(mtcars), mtcars)
})

test_that("convert factor to numeric, dummy factors, with NA", {
  x1 <- factor(rep(c("a", "b"), 3))
  x2 <- factor(c("a", NA_character_, "a", "b", "a", "b"))
  x3 <- factor(c(NA_character_, "b", "a", "b", "a", "b"))
  x4 <- factor(c("a", "b", "a", "b", "a", NA_character_))
  x5 <- factor(c(NA_character_, "b", "a", "b", "a", NA_character_))
  x6 <- factor(c(NA_character_, "b", NA_character_, "b", "a", NA_character_))
  x7 <- factor(c(
    NA_character_, "b", "a", "b", "a", "b", NA_character_, "b",
    "a", NA_character_, "a", "b", "a", "b", "a", NA_character_
  ))

  # same observations are missing
  expect_identical(
    which(!complete.cases(to_numeric(x1, dummy_factors = TRUE))),
    which(is.na(x1))
  )
  expect_identical(
    which(!complete.cases(to_numeric(x2, dummy_factors = TRUE))),
    which(is.na(x2))
  )
  expect_identical(
    which(!complete.cases(to_numeric(x3, dummy_factors = TRUE))),
    which(is.na(x3))
  )
  expect_identical(
    which(!complete.cases(to_numeric(x4, dummy_factors = TRUE))),
    which(is.na(x4))
  )
  expect_identical(
    which(!complete.cases(to_numeric(x5, dummy_factors = TRUE))),
    which(is.na(x5))
  )
  expect_identical(
    which(!complete.cases(to_numeric(x6, dummy_factors = TRUE))),
    which(is.na(x6))
  )
  expect_identical(
    which(!complete.cases(to_numeric(x7, dummy_factors = TRUE))),
    which(is.na(x7))
  )

  # output has same number of observation as input
  expect_identical(nrow(to_numeric(x1, dummy_factors = TRUE)), length(x1))
  expect_identical(nrow(to_numeric(x2, dummy_factors = TRUE)), length(x2))
  expect_identical(nrow(to_numeric(x3, dummy_factors = TRUE)), length(x3))
  expect_identical(nrow(to_numeric(x4, dummy_factors = TRUE)), length(x4))
  expect_identical(nrow(to_numeric(x5, dummy_factors = TRUE)), length(x5))
  expect_identical(nrow(to_numeric(x6, dummy_factors = TRUE)), length(x6))
  expect_identical(nrow(to_numeric(x7, dummy_factors = TRUE)), length(x7))
})

test_that("to_numeric, inverse factor levels", {
  f <- c(0, 0, 1, 1, 1, 0)
  x1 <- factor(f, levels = c(0, 1))
  x2 <- factor(f, levels = c(1, 0))
  out <- to_numeric(x1, dummy_factors = FALSE, preserve_levels = FALSE)
  expect_identical(out, c(1, 1, 2, 2, 2, 1))
  out <- to_numeric(x2, dummy_factors = FALSE, preserve_levels = FALSE)
  expect_identical(out, c(2, 2, 1, 1, 1, 2))
  out <- to_numeric(x1, dummy_factors = FALSE, preserve_levels = TRUE)
  expect_identical(out, c(0, 0, 1, 1, 1, 0))
  out <- to_numeric(x2, dummy_factors = FALSE, preserve_levels = TRUE)
  expect_identical(out, c(1, 1, 0, 0, 0, 1))
})

# select helpers ------------------------------
test_that("to_numeric regex", {
  expect_identical(
    to_numeric(mtcars, select = "pg", regex = TRUE),
    to_numeric(mtcars, select = "mpg")
  )
})


test_that("to_numeric works with haven_labelled, convert many labels correctly", {
  skip_on_cran()
  skip_if_not_installed("httr")
  skip_if_not_installed("haven")
  skip_if_not_installed("withr")
  skip_if_not_installed("curl")
  skip_if_offline()

  withr::with_tempfile("temp_file", fileext = ".sav", code = {
    request <- httr::GET("https://raw.github.com/easystats/circus/main/data/EFC.sav")
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)

    d <- haven::read_spss(temp_file)
    x <- to_numeric(d$c172code)
    expect_identical(as.vector(table(x)), c(180L, 506L, 156L))
  })
})


test_that("to_numeric preserves correct label order", {
  x <- factor(c(1, 2, 3, 4))
  x <- assign_labels(x, values = c("one", "two", "three", "four"))
  out <- to_numeric(x, dummy_factors = FALSE)
  expect_identical(
    attributes(out)$labels,
    c(one = 1, two = 2, three = 3, four = 4)
  )
  # correctly reverse scale
  out <- to_numeric(reverse_scale(x), dummy_factors = FALSE)
  expect_identical(
    attributes(out)$labels,
    c(one = 4, two = 3, three = 2, four = 1)
  )
  # factor with alphabetical values
  x <- factor(letters[1:4])
  x <- assign_labels(x, values = c("one", "two", "three", "four"))
  out <- to_numeric(x, dummy_factors = FALSE)
  expect_identical(
    attributes(out)$labels,
    c(one = 1, two = 2, three = 3, four = 4)
  )
  # correctly reverse scale
  out <- to_numeric(reverse_scale(x), dummy_factors = FALSE)
  expect_identical(
    attributes(out)$labels,
    c(one = 4, two = 3, three = 2, four = 1)
  )
})
