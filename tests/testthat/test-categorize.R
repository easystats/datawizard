set.seed(123)
d <- sample.int(10, size = 500, replace = TRUE)

test_that("recode median", {
  expect_identical(categorize(d), ifelse(d >= median(d), 2, 1))
  expect_identical(categorize(d, lowest = 0), as.numeric(d >= median(d)))
})

test_that("recode mean", {
  expect_identical(categorize(d, split = "mean"), ifelse(d >= mean(d), 2, 1))
  expect_identical(categorize(d, split = "mean", lowest = 0), as.numeric(d >= mean(d)))
})

test_that("recode quantile", {
  expect_error(categorize(d, split = "quantile"))

  q <- quantile(d, probs = c(1 / 3, 2 / 3, 1))
  f <- cut(d, breaks = unique(c(min(d), q, max(d))), include.lowest = TRUE, right = FALSE)
  levels(f) <- 1:nlevels(f)
  expect_identical(categorize(d, split = "quantile", n_groups = 3), as.numeric(f))
  expect_identical(categorize(d, split = "quantile", n_groups = 3, lowest = 0), as.numeric(f) - 1)
})

set.seed(123)
d <- sample.int(100, size = 1000, replace = TRUE)

test_that("recode range", {
  expect_error(categorize(d, split = "range"))
  d2 <- d
  d2[d <= 20] <- 1
  d2[d > 20 & d <= 40] <- 2
  d2[d > 40 & d <= 60] <- 3
  d2[d > 60 & d <= 80] <- 4
  d2[d > 80] <- 5
  expect_equal(table(categorize(d, split = "equal_range", range = 20)), table(d2), ignore_attr = TRUE)
  expect_equal(
    table(categorize(
      d,
      split = "equal_range",
      range = 20,
      lowest = 1
    )),
    table(d2),
    ignore_attr = TRUE
  )

  d2 <- d
  d2[d < 20] <- 0
  d2[d >= 20 & d < 40] <- 1
  d2[d >= 40 & d < 60] <- 2
  d2[d >= 60 & d < 80] <- 3
  d2[d >= 80] <- 4
  expect_equal(
    table(categorize(
      d,
      split = "equal_range",
      range = 20,
      lowest = 0
    )),
    table(d2),
    ignore_attr = TRUE
  )
})

test_that("recode length", {
  expect_error(categorize(d, split = "equal_length"))
  d2 <- d
  d2[d <= 20] <- 1
  d2[d > 20 & d <= 40] <- 2
  d2[d > 40 & d <= 60] <- 3
  d2[d > 60 & d <= 80] <- 4
  d2[d > 80] <- 5
  expect_equal(table(categorize(d, split = "equal_length", n_groups = 5)), table(d2), ignore_attr = TRUE)
  expect_equal(
    table(categorize(
      d,
      split = "equal_length",
      n_groups = 5,
      lowest = 1
    )),
    table(d2),
    ignore_attr = TRUE
  )
})

set.seed(123)
x <- sample.int(10, size = 30, replace = TRUE)
test_that("recode factor labels", {
  expect_type(categorize(x, "equal_length", n_groups = 3), "double")
  expect_s3_class(categorize(x, "equal_length", n_groups = 3, labels = c("low", "mid", "high")), "factor")
  expect_identical(
    levels(categorize(
      x,
      "equal_length",
      n_groups = 3,
      labels = c("low", "mid", "high")
    )),
    c("low", "mid", "high")
  )
  t1 <- table(categorize(x, "equal_length", n_groups = 3))
  t2 <- table(categorize(x, "equal_length", n_groups = 3, labels = c("low", "mid", "high")))
  expect_equal(t1, t2, ignore_attr = TRUE)
})

test_that("recode data frame", {
  data(iris)
  x <- iris
  out <- categorize(x, split = "median", select = c("Sepal.Length", "Sepal.Width"))
  expect_s3_class(out, "data.frame")
  expect_identical(out$Sepal.Length, ifelse(iris$Sepal.Length >= median(iris$Sepal.Length), 2, 1))
  expect_identical(out$Petal.Length, iris$Petal.Length)

  out <- categorize(x, split = "median", select = starts_with("Sepal"))
  expect_s3_class(out, "data.frame")
  expect_identical(out$Sepal.Length, ifelse(iris$Sepal.Length >= median(iris$Sepal.Length), 2, 1))
  expect_identical(out$Petal.Length, iris$Petal.Length)

  out <- categorize(x, split = "median", select = ~ Sepal.Width + Sepal.Length)
  expect_s3_class(out, "data.frame")
  expect_identical(out$Sepal.Length, ifelse(iris$Sepal.Length >= median(iris$Sepal.Length), 2, 1))
  expect_identical(out$Petal.Length, iris$Petal.Length)

  out <- categorize(x, split = "median", select = Sepal.Length)
  expect_s3_class(out, "data.frame")
  expect_identical(out$Sepal.Length, ifelse(iris$Sepal.Length >= median(iris$Sepal.Length), 2, 1))
  expect_identical(out$Petal.Length, iris$Petal.Length)

  expect_warning(
    expect_warning(
      out <- categorize(x, split = "median", select = c("sepal.Length", "sepal.Width"), ignore_case = FALSE),
      "not found"
    ),
    "not found"
  )
  expect_identical(out$Sepal.Length, iris$Sepal.Length)

  out <- categorize(x, split = "median", select = starts_with("sepal"), ignore_case = TRUE)
  expect_s3_class(out, "data.frame")
  expect_identical(out$Sepal.Length, ifelse(iris$Sepal.Length >= median(iris$Sepal.Length), 2, 1))
  expect_identical(out$Petal.Length, iris$Petal.Length)

  out <- categorize(x, split = "median", select = starts_with("sepal"), ignore_case = FALSE)
  expect_identical(out$Sepal.Length, iris$Sepal.Length)

  out <- categorize(x, split = "median", select = starts_with("sepal"), ignore_case = TRUE, append = "_r")
  expect_identical(colnames(out), c(
    "Sepal.Length", "Sepal.Width", "Petal.Length",
    "Petal.Width", "Species", "Sepal.Length_r", "Sepal.Width_r"
  ))

  out <- categorize(iris, split = "median", select = starts_with("Sepal"))
  expect_identical(
    out$Sepal.Length,
    c(
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
      1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2,
      2, 2, 1, 2, 1, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 2,
      2, 1, 2, 2, 2, 1, 1, 1, 2, 2, 1, 1, 1, 1, 2, 1, 1, 2, 2, 2, 2,
      2, 2, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2,
      2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
      2, 2, 2, 2
    )
  )

  skip_if_not_installed("poorman")

  x <- poorman::group_by(iris, Species)
  out <- categorize(x, split = "median", select = starts_with("Sepal"))
  expect_identical(
    out$Sepal.Length,
    c(
      2, 1, 1, 1, 2, 2, 1, 2, 1, 1, 2, 1, 1, 1, 2, 2, 2, 2, 2, 2,
      2, 2, 1, 2, 1, 2, 2, 2, 2, 1, 1, 2, 2, 2, 1, 2, 2, 1, 1, 2, 2,
      1, 1, 2, 2, 1, 2, 1, 2, 2, 2, 2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 2,
      2, 2, 1, 2, 1, 1, 2, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1,
      2, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 2, 1,
      2, 2, 1, 2, 2, 2, 2, 1, 2, 1, 1, 1, 2, 2, 2, 1, 2, 1, 2, 1, 2,
      2, 1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 1, 1, 1, 2, 2, 2, 1, 2, 2, 2,
      1, 2, 1, 1
    )
  )
})


test_that("recode all NA", {
  x <- rep(NA, 10)
  expect_message(
    y <- categorize(x),
    "can't be recoded"
  )
  expect_identical(y, x)

  x <- rep(NA_real_, 10)
  expect_message(
    y <- categorize(x),
    "only missing values"
  )
  expect_identical(y, x)
})



test_that("recode numeric", {
  expect_identical(
    categorize(mtcars$hp, split = c(100, 150)),
    c(
      2, 2, 1, 2, 3, 2, 3, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 1, 1, 1,
      1, 3, 3, 3, 3, 1, 1, 2, 3, 3, 3, 2
    )
  )
  x <- mtcars$hp
  x[mtcars$hp < 100] <- 1
  x[mtcars$hp >= 100 & mtcars$hp < 150] <- 2
  x[mtcars$hp >= 150] <- 3
  expect_identical(categorize(mtcars$hp, split = c(100, 150)), x)
  expect_identical(categorize(mtcars$hp, split = c(100, 150), lowest = NULL), x)

  expect_identical(
    categorize(mtcars$hp, split = "equal_range", range = 50, lowest = NULL),
    c(
      2, 2, 1, 2, 3, 2, 4, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 1, 1, 1,
      1, 2, 2, 4, 3, 1, 1, 2, 5, 3, 6, 2
    )
  )
})

# select helpers ------------------------------
test_that("categorize regex", {
  expect_identical(
    categorize(mtcars, select = "pg", regex = TRUE),
    categorize(mtcars, select = "mpg")
  )
})


# labelling ranges ------------------------------
test_that("categorize labelling ranged", {
  data(mtcars)
  expect_snapshot(categorize(mtcars$mpg, "equal_length", n_groups = 5))
  expect_snapshot(categorize(mtcars$mpg, "equal_length", n_groups = 5, labels = "range"))
  expect_snapshot(categorize(mtcars$mpg, "equal_length", n_groups = 5, labels = "observed"))
})

test_that("categorize breaks", {
  data(mtcars)
  expect_snapshot(categorize(mtcars$mpg, "equal_length", n_groups = 5, labels = "range", breaks = "inclusive"))
  expect_error(
    categorize(mtcars$mpg, "equal_length", n_groups = 5, breaks = "something"),
    regex = "should be one of"
  )
})
