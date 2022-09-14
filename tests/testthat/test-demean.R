test_that("demean works", {
  df <- iris

  set.seed(123)
  df$ID <- sample(1:4, nrow(df), replace = TRUE) # fake-ID

  set.seed(123)
  df$binary <- as.factor(rbinom(150, 1, .35)) # binary variable

  set.seed(123)
  x <- demean(df, select = c("Sepal.Length", "Petal.Length"), group = "ID")
  expect_snapshot(head(x))

  set.seed(123)
  x <- demean(df, select = c("Sepal.Length", "binary", "Species"), group = "ID")
  expect_snapshot(head(x))

  set.seed(123)
  expect_equal(
    demean(df, select = ~ Sepal.Length + binary + Species, group = ~ID),
    demean(df, select = c("Sepal.Length", "binary", "Species"), group = "ID")
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
  expect_snapshot(demean(dat, select = c("a", "x*y"), group = "ID"))
})

test_that("demean shows message if some vars don't exist", {
  dat <- data.frame(
    a = c(1, 2, 3, 4, 1, 2, 3, 4),
    x = c(4, 3, 3, 4, 1, 2, 1, 2),
    y = c(1, 2, 1, 2, 4, 3, 2, 1),
    ID = c(1, 2, 3, 1, 2, 3, 1, 2)
  )

  set.seed(123)
  expect_message(
    demean(dat, select = c("foo"), group = "ID"),
    regexp = "not found"
  )
})
