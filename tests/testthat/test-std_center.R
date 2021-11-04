if (require("testthat")) {

  d <- iris[1:4, ]

  # standardize -----------------------------------------------------
  test_that("standardize.data.frame", {
    x <- standardise(d, select = c("Sepal.Length", "Sepal.Width"))
    expect_equal(as.vector(x$Sepal.Length), as.vector(scale(d$Sepal.Length)), tolerance = 0.001)

    x <- standardise(d, select = c("Sepal.Length", "Sepal.Width"), append = TRUE)
    expect_equal(as.vector(x$Sepal.Length_z), as.vector(scale(d$Sepal.Length)), tolerance = 0.001)
    expect_equal(as.vector(x$Sepal.Length), as.vector(d$Sepal.Length), tolerance = 0.001)
  })

  # standardize -----------------------------------------------------
  test_that("center.data.frame", {
    x <- center(d, select = c("Sepal.Length", "Sepal.Width"))
    expect_equal(as.vector(x$Sepal.Length), as.vector(d$Sepal.Length - mean(d$Sepal.Length)), tolerance = 0.001)

    x <- center(d, select = c("Sepal.Length", "Sepal.Width"), append = TRUE)
    expect_equal(as.vector(x$Sepal.Length_c), as.vector(d$Sepal.Length - mean(d$Sepal.Length)), tolerance = 0.001)
    expect_equal(as.vector(x$Sepal.Length), as.vector(d$Sepal.Length), tolerance = 0.001)
  })
}
