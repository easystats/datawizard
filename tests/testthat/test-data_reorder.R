test_that("data_reorder works as expected", {
  expect_equal(
    names(data_reorder(iris, c("Species", "Sepal.Length"))),
    c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  )

  expect_warning(expect_equal(
    names(data_reorder(iris, c("Species", "dupa"))),
    c("Species", "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
  ))
})


# preserve attributes --------------------------

test_that("data_reorder preserves attributes", {
  skip_if_not_or_load_if_installed("parameters")

  m <- lm(Sepal.Length ~ Species, data = iris)
  out <- parameters::parameters(m)
  a1 <- attributes(out)

  out2 <- data_reorder(out, 4:6)
  a2 <- attributes(out2)

  # attributes may not be in the same order
  expect_true(all(names(a1) %in% names(a2)) && length(a1) == length(a2))
})
