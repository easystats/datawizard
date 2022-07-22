test_that("convert data frame to numeric", {
  expect_snapshot(to_numeric(head(ToothGrowth)))
  expect_snapshot(to_numeric(head(ToothGrowth), dummy_factors = FALSE))
})

test_that("convert character to numeric", {
  expect_equal(to_numeric(c("xyz", "ab")), c(2, 1))
})

test_that("convert character to numeric Date", {
  expect_warning(expect_equal(to_numeric(as.Date("2022-01-01")), 18993))
})

test_that("convert character to numeric preserve levels", {
  x <- as.factor(mtcars$gear)
  expect_equal(
    to_numeric(x, dummy_factors = FALSE),
    c(2, 2, 2, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 2, 3, 3, 3, 3, 3, 2)
  )
  expect_equal(
    to_numeric(x, dummy_factors = FALSE, preserve_levels = TRUE),
    c(4, 4, 4, 3, 3, 3, 3, 4, 4, 4, 4, 3, 3, 3, 3, 3, 3, 4, 4, 4, 3, 3, 3, 3, 3, 4, 5, 5, 5, 5, 5, 4)
  )
})

test_that("convert character to numeric lowest", {
  mtcars$vs <- as.factor(mtcars$vs)
  model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
  expect_equal(
    to_numeric(insight::get_response(model), dummy_factors = FALSE),
    c(1, 1, 2, 2, 1, 2, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 2, 1, 2, 1, 1, 1, 2)
  )
  expect_equal(
    to_numeric(insight::get_response(model), dummy_factors = FALSE, lowest = 0),
    c(0, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1)
  )
})

test_that("convert factor to numeric", {
  f <- factor(substring("statistics", 1:10, 1:10))
  expect_snapshot(to_numeric(f))
})


test_that("convert factor to numeric", {
  expect_equal(to_numeric(c("abc", "xyz")), c(1, 2))
  expect_equal(to_numeric(c("123", "789")), c(123, 789))
  expect_equal(to_numeric(c("1L", "2e-3")), c(1, 0.002))
  expect_equal(to_numeric(c("1L", "2e-3", "ABC")), c(1, 2, 3))
})


test_that("convert factor to numeric, dummy factors", {
  expect_equal(
    to_numeric(c("abc", "xyz"), dummy_factors = TRUE),
    data.frame(abc = c(1, 0), xyz = c(0, 1)),
    ignore_attr = TRUE
  )
  expect_equal(
    to_numeric(c("1L", "2e-3", "ABC"), dummy_factors = TRUE),
    data.frame(`1L` = c(1, 0, 0), `2e-3` = c(0, 1, 0), ABC = c(0, 0, 1)),
    ignore_attr = TRUE
  )
})


test_that("convert factor to numeric, append", {
  data(efc)
  expect_equal(
    colnames(to_numeric(efc)),
    c("c12hour", "e16sex", "e42dep.1", "e42dep.2", "e42dep.3", "e42dep.4", "c172code", "neg_c_7"),
    ignore_attr = TRUE
  )
  expect_equal(
    colnames(to_numeric(efc, append = TRUE)),
    c(
      "c12hour", "e16sex", "e42dep", "c172code", "neg_c_7", "e42dep_n",
      "e42dep_n.1", "e42dep_n.2", "e42dep_n.3", "e42dep_n.4"
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    colnames(to_numeric(efc, append = TRUE, dummy_factors = FALSE)),
    c("c12hour", "e16sex", "e42dep", "c172code", "neg_c_7", "e42dep_n"),
    ignore_attr = TRUE
  )
  expect_equal(
    colnames(to_numeric(efc, append = FALSE, dummy_factors = FALSE)),
    c("c12hour", "e16sex", "e42dep", "c172code", "neg_c_7"),
    ignore_attr = TRUE
  )
})


test_that("convert factor to numeric, all numeric", {
  data(mtcars)
  expect_equal(to_numeric(mtcars), mtcars)
})


test_that("convert factor to numeric, dummy factors, with NA", {
  x1 <- factor(rep(c("a", "b"), 3))
  x2 <- factor(c("a", NA_character_, "a", "b", "a", "b"))
  x3 <- factor(c(NA_character_, "b", "a", "b", "a", "b"))
  x4 <- factor(c("a", "b", "a", "b", "a", NA_character_))
  x5 <- factor(c(NA_character_, "b", "a", "b", "a", NA_character_))
  x6 <- factor(c(NA_character_, "b", NA_character_, "b", "a", NA_character_))
  x7 <- factor(c(NA_character_, "b", "a", "b", "a", "b", NA_character_, "b", "a", NA_character_, "a", "b", "a", "b", "a", NA_character_))

  # same observations are missing
  expect_equal(
    which(!complete.cases(to_numeric(x1, dummy_factors = TRUE))),
    which(is.na(x1))
  )
  expect_equal(
    which(!complete.cases(to_numeric(x2, dummy_factors = TRUE))),
    which(is.na(x2))
  )
  expect_equal(
    which(!complete.cases(to_numeric(x3, dummy_factors = TRUE))),
    which(is.na(x3))
  )
  expect_equal(
    which(!complete.cases(to_numeric(x4, dummy_factors = TRUE))),
    which(is.na(x4))
  )
  expect_equal(
    which(!complete.cases(to_numeric(x5, dummy_factors = TRUE))),
    which(is.na(x5))
  )
  expect_equal(
    which(!complete.cases(to_numeric(x6, dummy_factors = TRUE))),
    which(is.na(x6))
  )
  expect_equal(
    which(!complete.cases(to_numeric(x7, dummy_factors = TRUE))),
    which(is.na(x7))
  )

  # output has same number of observation as input
  expect_equal(nrow(to_numeric(x1, dummy_factors = TRUE)), length(x1))
  expect_equal(nrow(to_numeric(x2, dummy_factors = TRUE)), length(x2))
  expect_equal(nrow(to_numeric(x3, dummy_factors = TRUE)), length(x3))
  expect_equal(nrow(to_numeric(x4, dummy_factors = TRUE)), length(x4))
  expect_equal(nrow(to_numeric(x5, dummy_factors = TRUE)), length(x5))
  expect_equal(nrow(to_numeric(x6, dummy_factors = TRUE)), length(x6))
  expect_equal(nrow(to_numeric(x7, dummy_factors = TRUE)), length(x7))
})
