if (require("testthat")) {
  d <- iris[1:4, ]

  # standardize -----------------------------------------------------
  test_that("standardize.data.frame", {
    x <- standardise(d, select = c("Sepal.Length", "Sepal.Width"))
    expect_equal(as.vector(x$Sepal.Length), as.vector(scale(d$Sepal.Length)), tolerance = 0.001)
    expect_equal(as.vector(x$Petal.Length), as.vector(d$Petal.Length), tolerance = 0.001)
    expect_equal(colnames(x), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"))

    x <- standardise(d, select = c("Sepal.Length", "Sepal.Width"), append = TRUE)
    expect_equal(as.vector(x$Sepal.Length_z), as.vector(scale(d$Sepal.Length)), tolerance = 0.001)
    expect_equal(as.vector(x$Sepal.Length), as.vector(d$Sepal.Length), tolerance = 0.001)
    expect_equal(as.vector(x$Petal.Length), as.vector(d$Petal.Length), tolerance = 0.001)
    expect_equal(colnames(x), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "Sepal.Length_z", "Sepal.Width_z"))
  })

  # standardize -----------------------------------------------------
  test_that("center.data.frame", {
    x <- center(d, select = c("Sepal.Length", "Sepal.Width"))
    expect_equal(as.vector(x$Sepal.Length), as.vector(d$Sepal.Length - mean(d$Sepal.Length)), tolerance = 0.001)
    expect_equal(as.vector(x$Petal.Length), as.vector(d$Petal.Length), tolerance = 0.001)
    expect_equal(colnames(x), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species"))

    x <- center(d, select = c("Sepal.Length", "Sepal.Width"), append = TRUE)
    expect_equal(as.vector(x$Sepal.Length_c), as.vector(d$Sepal.Length - mean(d$Sepal.Length)), tolerance = 0.001)
    expect_equal(as.vector(x$Sepal.Length), as.vector(d$Sepal.Length), tolerance = 0.001)
    expect_equal(as.vector(x$Petal.Length), as.vector(d$Petal.Length), tolerance = 0.001)
    expect_equal(colnames(x), c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species", "Sepal.Length_c", "Sepal.Width_c"))
  })
}
