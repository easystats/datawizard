test_that("extract from data frame", {
  expect_equal(
    extract(mtcars, cyl, name = gear),
    setNames(mtcars$cyl, mtcars$gear)
  )
  expect_equal(
    extract(mtcars, -1, name = gear),
    setNames(mtcars[[11]], mtcars$gear)
  )
  expect_equal(
    extract(mtcars, cyl, name = 0),
    setNames(mtcars$cyl, rownames(mtcars))
  )
  expect_equal(
    extract(mtcars, cyl, name = "row.names"),
    setNames(mtcars$cyl, rownames(mtcars))
  )
})


test_that("extract from data frame", {
  # vector
  x <- head(data_extract(iris, "Species", name = "Sepal.Length"))
  expect_equal(x, head(setNames(iris$Species, iris$Sepal.Length)))

  # vector
  x <- head(data_extract(iris, "Species", name = c("Sepal.Length", "Petal.Length")))
  expect_equal(x, head(setNames(iris$Species, NULL)))

  # data frame
  x <- head(data_extract(iris, c("Sepal.Length", "Petal.Length"), name = "Species"))
  expect_equal(x, head(iris[c("Sepal.Length", "Petal.Length")]))
  expect_equal(colnames(x), c("Sepal.Length", "Petal.Length"))

  # data frame
  x <- head(data_extract(iris, c("Sepal.Length", "Petal.Length"), name = c("Species", "Sepal.Width")))
  expect_equal(x, head(iris[c("Sepal.Length", "Petal.Length")]))
  expect_equal(colnames(x), c("Sepal.Length", "Petal.Length"))

  # data frame
  x <- head(data_extract(iris, "Species", name = "Sepal.Length", as_data_frame = TRUE))
  expect_equal(x, head(iris["Species"]))
  expect_equal(colnames(x), "Species")
})
