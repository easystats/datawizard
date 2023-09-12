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
  out <- seek_variables(efc, "female", source = "all")
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
  out <- seek_variables(efc, "femlae", source = "all", fuzzy = TRUE)
  expect_identical(nrow(out), 1L)
  expect_identical(out$index, which(colnames(efc) %in% out$column))
  expect_identical(out$labels, "elder's gender")
})
