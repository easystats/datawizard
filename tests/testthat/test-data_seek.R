test_that("data_seek - simple use case", {
  data(iris)
  out <- data_seek(iris, "Length")
  expect_identical(out$index, c(1L, 3L))
  expect_identical(out$labels, c("Sepal.Length", "Petal.Length"))
})

test_that("data_seek - seek label attribute", {
  data(efc)
  out <- data_seek(efc, "dependency")
  expect_identical(out$index, which(colnames(efc) == out$column))
  expect_identical(out$labels, "elder's dependency")
})

test_that("data_seek - seek label attribute", {
  data(efc)
  out <- data_seek(efc, "female")
  expect_identical(nrow(out), 0L)
  out <- data_seek(efc, "female", seek = "all")
  expect_identical(out$index, which(colnames(efc) == out$column))
  expect_identical(out$labels, "elder's gender")
})

test_that("data_seek - fuzzy match", {
  data(iris)
  out <- data_seek(iris, "Lenght")
  expect_identical(nrow(out), 0L)
  out <- data_seek(iris, "Lenght", fuzzy = TRUE)
  expect_identical(out$index, which(colnames(iris) %in% out$column))
  expect_identical(out$labels, c("Sepal.Length", "Petal.Length"))
})

test_that("data_seek - fuzzy match, value labels", {
  data(efc)
  out <- data_seek(efc, "femlae", seek = "all", fuzzy = TRUE)
  expect_identical(nrow(out), 1L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, "elder's gender")
})

test_that("data_seek - multiple pattern", {
  data(efc)
  out <- data_seek(efc, c("e16", "e42"))
  expect_identical(nrow(out), 2L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, c("elder's gender", "elder's dependency"))
  # only one match, typo
  out <- data_seek(efc, c("femlae", "dependency"))
  expect_identical(nrow(out), 1L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, "elder's dependency")
  # only one match, not searching in value labels
  out <- data_seek(efc, c("female", "dependency"))
  expect_identical(nrow(out), 1L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, "elder's dependency")
  # two matches
  out <- data_seek(efc, c("female", "dependency"), seek = "all")
  expect_identical(nrow(out), 2L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, c("elder's gender", "elder's dependency"))
  # only one match, typo
  out <- data_seek(efc, c("femlae", "dependency"), seek = "all")
  expect_identical(nrow(out), 1L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, "elder's dependency")
  # two matches, despite typo
  out <- data_seek(efc, c("femlae", "dependency"), seek = "all", fuzzy = TRUE)
  expect_identical(nrow(out), 2L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, c("elder's gender", "elder's dependency"))
})

test_that("data_seek - valid input", {
  expect_error(data_seek(rnorm(10), "Length"), regex = "`data` must be a data frame.")
  expect_error(data_seek(iris, "Length", seek = "somewhere"), regex = "`seek` must be")
})

test_that("data_seek - print", {
  expect_snapshot(data_seek(iris, "Length"))
  expect_snapshot(data_seek(iris, "abc"))
})
