test_that("seek_variables - simple use case", {
  data(iris)
  out <- seek_variables(iris, "Length")
  expect_identical(out$index, c(1L, 3L))
  expect_identical(out$labels, c("Sepal.Length", "Petal.Length"))
})

test_that("seek_variables - search label attribute", {
  data(efc)
  out <- seek_variables(efc, "dependency")
  expect_identical(out$index, which(colnames(efc) == out$column))
  expect_identical(out$labels, "elder's dependency")
})

test_that("seek_variables - search label attribute", {
  data(efc)
  out <- seek_variables(efc, "female")
  expect_identical(nrow(out), 0L)
  out <- seek_variables(efc, "female", search = "all")
  expect_identical(out$index, which(colnames(efc) == out$column))
  expect_identical(out$labels, "elder's gender")
})

test_that("seek_variables - fuzzy match", {
  data(iris)
  out <- seek_variables(iris, "Lenght")
  expect_identical(nrow(out), 0L)
  out <- seek_variables(iris, "Lenght", fuzzy = TRUE)
  expect_identical(out$index, which(colnames(iris) %in% out$column))
  expect_identical(out$labels, c("Sepal.Length", "Petal.Length"))
})

test_that("seek_variables - fuzzy match, value labels", {
  data(efc)
  out <- seek_variables(efc, "femlae", search = "all", fuzzy = TRUE)
  expect_identical(nrow(out), 1L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, "elder's gender")
})

test_that("seek_variables - multiple pattern", {
  data(efc)
  out <- seek_variables(efc, c("e16", "e42"))
  expect_identical(nrow(out), 2L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, c("elder's gender", "elder's dependency"))
  # only one match, typo
  out <- seek_variables(efc, c("femlae", "dependency"))
  expect_identical(nrow(out), 1L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, "elder's dependency")
  # only one match, not searching in value labels
  out <- seek_variables(efc, c("female", "dependency"))
  expect_identical(nrow(out), 1L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, "elder's dependency")
  # two matches
  out <- seek_variables(efc, c("female", "dependency"), search = "all")
  expect_identical(nrow(out), 2L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, c("elder's gender", "elder's dependency"))
  # only one match, typo
  out <- seek_variables(efc, c("femlae", "dependency"), search = "all")
  expect_identical(nrow(out), 1L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, "elder's dependency")
  # two matches, despite typo
  out <- seek_variables(efc, c("femlae", "dependency"), search = "all", fuzzy = TRUE)
  expect_identical(nrow(out), 2L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, c("elder's gender", "elder's dependency"))
})

test_that("seek_variables - valid input", {
  expect_error(seek_variables(rnorm(10), "Length"), regex = "`data` must be a data frame.")
  expect_error(seek_variables(iris, "Length", search = "somewhere"), regex = "`search` must be")
})
