data(efc)
data(iris)

test_that("data_codebook iris", {
  expect_snapshot(data_codebook(iris))
})


test_that("data_codebook iris, reordered", {
  expect_snapshot(data_codebook(iris[c(1, 2, 5, 3, 4)]))
})


test_that("data_codebook NaN and Inf", {
  d <- data.frame(
    x = c(1, 4, NA, Inf, 4, NaN, 2, 1, 1)
  )
  expect_snapshot(data_codebook(d))

  set.seed(123)
  d <- data.frame(
    x = c(sample(1:15, 100, TRUE), Inf, Inf)
  )
  expect_snapshot(data_codebook(d))
  expect_snapshot(data_codebook(d, range_at = 100))
  expect_snapshot(data_codebook(d, range_at = 100, max_values = 4))
})


test_that("data_codebook iris, select", {
  expect_snapshot(data_codebook(iris, select = starts_with("Sepal")))
})


test_that("data_codebook iris, select, ID", {
  expect_snapshot(data_codebook(iris, select = starts_with("Petal")))
})


test_that("data_codebook efc", {
  expect_snapshot(data_codebook(efc))
})


test_that("data_codebook efc, variable_label_width", {
  expect_snapshot(data_codebook(efc, variable_label_width = 30))
})


test_that("data_codebook efc, value_label_width", {
  expect_snapshot(data_codebook(efc, variable_label_width = 30, value_label_width = 15))
})


test_that("data_codebook truncated data", {
  set.seed(123)
  d <- data.frame(
    a = sample(1:15, 100, TRUE),
    b = sample(letters[1:18], 100, TRUE),
    stringsAsFactors = FALSE
  )
  expect_snapshot(data_codebook(d, max_values = 5))
})


test_that("data_codebook mixed numeric lengths", {
  set.seed(123)
  d <- data.frame(
    a = sample(1:4, 100, TRUE),
    b = sample(5:15, 100, TRUE),
    stringsAsFactors = FALSE
  )
  expect_snapshot(data_codebook(d))
})

test_that("data_codebook mixed range_at", {
  set.seed(123)
  d <- data.frame(
    a = sample(1:4, 100, TRUE),
    b = sample(5:15, 100, TRUE),
    stringsAsFactors = FALSE
  )
  expect_snapshot(data_codebook(d, range_at = 3))
})


test_that("data_codebook logicals", {
  set.seed(123)
  d <- data.frame(
    a = sample(1:15, 100, TRUE),
    b = sample(letters[1:3], 100, TRUE),
    c = sample(c(TRUE, FALSE), 100, TRUE),
    stringsAsFactors = FALSE
  )
  expect_snapshot(data_codebook(d))
})


test_that("data_codebook labelled data exceptions", {
  set.seed(123)

  f1 <- sample(1:5, 100, TRUE)
  f1[f1 == 4] <- NA
  attr(f1, "labels") <- setNames(1:5, c("One", "Two", "Three", "Four", "Five"))

  f2 <- sample(1:5, 100, TRUE)
  attr(f2, "labels") <- setNames(c(1:3, 5), c("One", "Two", "Three", "Five"))

  f3 <- sample(1:5, 100, TRUE)
  attr(f3, "labels") <- setNames(1:5, c("One", "Two", "Three", "Four", "Five"))

  d <- data.frame(f1, f2, f3)
  expect_snapshot(data_codebook(d))
})


test_that("data_codebook labelled data factors", {
  set.seed(123)

  f1 <- factor(sample(c("c", "b", "a"), 100, TRUE))
  attr(f1, "labels") <- setNames(c("c", "b", "a"), c("Cee", "Bee", "A"))

  f2 <- factor(sample(c("a", "b", "c"), 100, TRUE))
  attr(f2, "labels") <- setNames(c("c", "b", "a"), c("Cee", "Bee", "A"))

  f3 <- factor(sample(c("c", "b", "a"), 100, TRUE))
  attr(f3, "labels") <- setNames(c("a", "c", "b"), c("A", "Cee", "Bee"))

  d <- data.frame(f1, f2, f3)
  expect_snapshot(data_codebook(d))
})


test_that("data_codebook works with numbers < 1", {
  d <- data.frame(
    a = c(1, 1, 2, 2, 3, 3),
    b = c(0, 0, 0, 1, 1, 2)
  )
  expect_snapshot(data_codebook(d))
})


test_that("data_codebook, big marks", {
  set.seed(123)
  f1 <- factor(sample(c("c", "b", "a"), 1e6, TRUE))
  f2 <- factor(sample(1:3, 1e6, TRUE))
  d <- data.frame(f1, f2)
  expect_snapshot(data_codebook(d))
})


test_that("data_codebook, tagged NA", {
  skip_if_not_or_load_if_installed("haven")
  x <- labelled(
    x = c(
      1:3, tagged_na("a", "c", "z"),
      4:1, tagged_na("a", "a", "c"),
      1:3, tagged_na("z", "c", "c"),
      1:4, tagged_na("a", "c", "z")
    ),
    labels = c(
      "Agreement" = 1, "Disagreement" = 4,
      "First" = tagged_na("c"), "Refused" = tagged_na("a"),
      "Not home" = tagged_na("z")
    )
  )
  expect_snapshot(data_codebook(data.frame(x)))

  x <- labelled(
    x = c(
      1:3, tagged_na("a", "c"),
      4:1, tagged_na("a", "a", "c"),
      1:3, tagged_na("c", "c"),
      1:4, tagged_na("a", "c")
    ),
    labels = c(
      "Agreement" = 1, "Disagreement" = 4,
      "First" = tagged_na("c"), "Refused" = tagged_na("a"),
      "Not home" = tagged_na("z")
    )
  )
  expect_snapshot(data_codebook(data.frame(x)))
})


test_that("data_codebook, negative label values #334", {
  skip_if_not_or_load_if_installed("haven")
  x1 <- labelled(
    x = 1:4,
    labels = c("Agreement" = 1, "Disagreement" = 4, "Missing" = -9)
  )
  x2 <- labelled(
    x = c(1:3, -9),
    labels = c("Agreement" = 1, "Disagreement" = 4, "Missing" = -9)
  )
  expect_snapshot(data_codebook(data.frame(x1, x2)))
})
