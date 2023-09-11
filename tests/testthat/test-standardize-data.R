# standardize.numeric -----------------------------------------------------
test_that("standardize.numeric", {
  x <- standardize(seq(0, 1, length.out = 100))
  expect_equal(mean(x), 0, tolerance = 0.01)

  x <- standardize(seq(0, 1, length.out = 100), two_sd = TRUE)
  expect_equal(sd(x), 0.5, tolerance = 0.01)

  x <- standardize(seq(0, 1, length.out = 100), robust = TRUE)
  expect_equal(median(x), 0, tolerance = 0.01)

  x <- standardize(seq(0, 1, length.out = 100), robust = TRUE, two_sd = TRUE)
  expect_equal(mad(x), 0.5, tolerance = 0.01)

  expect_message(standardize(c(0, 0, 0, 1, 1)))

  x <- standardize(c(-1, 0, 1), reference = seq(3, 4, length.out = 100))
  expect_equal(mean(x), -11.943, tolerance = 0.01)
})


# standardize factor / Date -----------------------------------------------
test_that("standardize.numeric", {
  f <- factor(c("c", "a", "b"))
  expect_identical(standardize(f), f)
  expect_equal(standardize(f, force = TRUE), c(1, -1, 0), ignore_attr = TRUE)

  d <- as.Date(c("1989/08/06", "1989/08/04", "1989/08/05"))
  expect_identical(standardize(d), d)
  expect_equal(standardize(d, force = TRUE), c(1, -1, 0), ignore_attr = TRUE)
})


# standardize.data.frame --------------------------------------------------

test_that("standardize.data.frame", {
  skip_if_not_installed("poorman")

  data(iris)
  x <- standardize(iris)
  expect_equal(mean(x$Sepal.Length), 0, tolerance = 0.01)
  expect_length(levels(x$Species), 3)
  expect_equal(mean(subset(x, Species == "virginica")$Sepal.Length), 0.90, tolerance = 0.01)

  # check class attributes
  expect_identical(
    vapply(x, class, character(1)),
    c(
      Sepal.Length = "numeric", Sepal.Width = "numeric", Petal.Length = "numeric",
      Petal.Width = "numeric", Species = "factor"
    )
  )

  x2 <- standardize(x = iris[1, ], reference = iris)
  expect_true(all(x2[1, ] == x[1, ]))


  x <- standardize(poorman::group_by(iris, Species))
  expect_equal(mean(x$Sepal.Length), 0, tolerance = 0.01)
  expect_length(levels(x$Species), 3)
  expect_equal(mean(subset(x, Species == "virginica")$Sepal.Length), 0, tolerance = 0.01)
})


test_that("standardize.data.frame, NAs", {
  skip_if_not_installed("poorman")

  data(iris)
  iris$Sepal.Width[c(148, 65, 33, 58, 54, 93, 114, 72, 32, 23)] <- NA
  iris$Sepal.Length[c(11, 30, 141, 146, 13, 149, 6, 8, 48, 101)] <- NA

  x <- standardize(iris)
  expect_equal(head(x$Sepal.Length), c(-0.9163, -1.1588, -1.4013, -1.5226, -1.0376, NA), tolerance = 0.01)
  expect_equal(head(x$Sepal.Width), c(1.0237, -0.151, 0.3189, 0.0839, 1.2586, 1.9635), tolerance = 0.01)
  expect_identical(mean(x$Sepal.Length), NA_real_)

  x <- standardize(iris, two_sd = TRUE)
  expect_equal(head(x$Sepal.Length), c(-0.4603, -0.5811, -0.7019, -0.7623, -0.5207, NA), tolerance = 0.01)
  expect_equal(head(x$Sepal.Width), c(0.5118, -0.0755, 0.1594, 0.042, 0.6293, 0.9817), tolerance = 0.01)
  expect_identical(mean(x$Sepal.Length), NA_real_)


  x <- standardize(poorman::group_by(iris, .data$Species))
  expect_equal(head(x$Sepal.Length), c(0.2547, -0.3057, -0.8661, -1.1463, -0.0255, NA), tolerance = 0.01)
  expect_equal(head(x$Sepal.Width), c(0.2369, -1.0887, -0.5584, -0.8235, 0.502, 1.2974), tolerance = 0.01)
  expect_identical(mean(x$Sepal.Length), NA_real_)
})


test_that("standardize.data.frame, apend", {
  skip_if_not_installed("poorman")

  data(iris)
  iris$Sepal.Width[c(26, 43, 56, 11, 66, 132, 23, 133, 131, 28)] <- NA
  iris$Sepal.Length[c(32, 12, 109, 92, 119, 49, 83, 113, 64, 30)] <- NA

  x <- standardize(iris, append = TRUE)
  expect_identical(colnames(x), c(
    "Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width",
    "Species", "Sepal.Length_z", "Sepal.Width_z", "Petal.Length_z",
    "Petal.Width_z"
  ))
  expect_equal(head(x$Sepal.Length_z), c(-0.8953, -1.1385, -1.3816, -1.5032, -1.0169, -0.5306), tolerance = 0.01)
  expect_equal(head(x$Sepal.Width_z), c(1.04, -0.1029, 0.3543, 0.1257, 1.2685, 1.9542), tolerance = 0.01)
  expect_identical(mean(x$Sepal.Length_z), NA_real_)

  x <- standardize(iris, two_sd = TRUE, append = TRUE)
  expect_equal(head(x$Sepal.Length_z), c(-0.4477, -0.5692, -0.6908, -0.7516, -0.5084, -0.2653), tolerance = 0.01)
  expect_equal(head(x$Sepal.Width_z), c(0.52, -0.0514, 0.1771, 0.0629, 0.6343, 0.9771), tolerance = 0.01)
  expect_identical(mean(x$Sepal.Length_z), NA_real_)


  x <- standardize(poorman::group_by(iris, .data$Species), append = TRUE)
  expect_equal(head(x$Sepal.Length_z), c(0.2746, -0.2868, -0.8483, -1.129, -0.0061, 1.1168), tolerance = 0.01)
  expect_equal(head(x$Sepal.Width_z), c(0.1766, -1.1051, -0.5924, -0.8487, 0.4329, 1.2019), tolerance = 0.01)
  expect_identical(mean(x$Sepal.Length_z), NA_real_)
})


test_that("standardize.data.frame, weights", {
  skip_if_not_installed("poorman")

  x <- rexp(30)
  w <- rpois(30, 20) + 1

  expect_equal(
    sqrt(cov.wt(cbind(x, x), w)$cov[1, 1]),
    attr(standardize(x, weights = w), "scale"),
    tolerance = 1e-4
  )
  expect_equal(
    standardize(x, weights = w),
    standardize(data.frame(x), weights = w)$x,
    tolerance = 1e-4,
    ignore_attr = TRUE
  )

  # name and vector give same results
  expect_equal(
    standardize(mtcars, exclude = "cyl", weights = mtcars$cyl),
    standardize(mtcars, weights = "cyl"),
    tolerance = 1e-4
  )

  d <- poorman::group_by(mtcars, am)
  expect_warning(standardize(d, weights = d$cyl))
})


# Unstandardize -----------------------------------------------------------
test_that("unstandardize, numeric", {
  data(iris)
  x <- standardize(iris$Petal.Length)
  rez <- unstandardize(x)
  expect_equal(rez, iris$Petal.Length, tolerance = 1e-3, ignore_attr = TRUE)

  rez <- unstandardize(x, reference = iris$Petal.Length)
  expect_equal(rez, iris$Petal.Length, tolerance = 1e-3, ignore_attr = TRUE)

  rez <- unstandardize(x, center = mean(iris$Petal.Length), scale = stats::sd(iris$Petal.Length))
  expect_equal(rez, iris$Petal.Length, tolerance = 1e-3, ignore_attr = TRUE)

  rez <- unstandardize(0, center = mean(iris$Petal.Length), scale = stats::sd(iris$Petal.Length))
  expect_equal(rez, mean(iris$Petal.Length), tolerance = 1e-3)

  x <- standardize(iris$Petal.Length, robust = TRUE, two_sd = TRUE)
  rez <- unstandardize(x, robust = TRUE, two_sd = TRUE)
  expect_equal(rez, iris$Petal.Length, tolerance = 1e-3, ignore_attr = TRUE)

  x <- scale(iris$Petal.Length)
  rez <- unstandardize(x)
  expect_equal(rez, iris$Petal.Length, tolerance = 1e-3, ignore_attr = TRUE)

  x <- scale(iris$Petal.Length, center = 3, scale = 2)
  rez <- unstandardize(x)
  expect_equal(rez, iris$Petal.Length, tolerance = 1e-3, ignore_attr = TRUE)
})

test_that("unstandardize, data frame", {
  skip_if_not_installed("poorman")

  data(iris)
  x <- standardize(iris)
  rez <- unstandardize(x)
  expect_equal(rez, iris, tolerance = 0.1, ignore_attr = TRUE)

  x <- standardize(iris, select = "Petal.Length")
  rez <- unstandardize(x)
  expect_equal(rez, iris, tolerance = 0.1, ignore_attr = TRUE)

  x <- standardize(iris, select = starts_with("Pet"))
  rez <- unstandardize(x, select = starts_with("Pet"))
  expect_equal(rez, iris, tolerance = 0.1, ignore_attr = TRUE)

  x <- standardize(iris, select = "Petal.Length")
  rez <- unstandardize(x,
    center = c(Petal.Length = mean(iris$Petal.Length)),
    scale = c(Petal.Length = stats::sd(iris$Petal.Length))
  )
  expect_equal(rez, iris, tolerance = 0.1, ignore_attr = TRUE)

  expect_error(unstandardize(x,
    center = mean(iris$Petal.Length),
    scale = stats::sd(iris$Petal.Length)
  ))

  x <- standardize(iris)
  rez <- unstandardize(x, center = rep(0, 4), scale = rep(1, 4))
  expect_equal(rez, x, tolerance = 0.1, ignore_attr = TRUE)

  data(iris)
  x <- standardize(iris, robust = TRUE, two_sd = TRUE)
  rez <- unstandardize(x, robust = TRUE, two_sd = TRUE)
  expect_equal(rez, iris, tolerance = 0.1, ignore_attr = TRUE)
})

test_that("un/standardize, matrix", {
  set.seed(4)
  x <- matrix(sample(8), nrow = 4)
  colnames(x) <- letters[1:2]
  rownames(x) <- LETTERS[1:4]

  z1 <- standardize(x)
  z2 <- scale(x)

  expect_equal(z1, z2, ignore_attr = TRUE)
  expect_equal(unstandardize(z1), x, ignore_attr = TRUE)
  expect_identical(unstandardize(z2), unstandardize(z1))
})

test_that("unstandardize with reference (data frame)", {
  x <- standardize(x = iris, reference = iris)
  x2 <- unstandardize(x, reference = iris)
  expect_equal(x2, iris, ignore_attr = TRUE)

  x <- standardize(x = iris, reference = iris, robust = TRUE)
  x2 <- unstandardize(x, reference = iris, robust = TRUE)
  expect_equal(x2, iris, ignore_attr = TRUE)
})

test_that("unstandardize does nothing with characters and factors", {
  expect_identical(
    unstandardise(c("a", "b")),
    c("a", "b")
  )
  expect_identical(
    unstandardise(factor(c(1, 2))),
    factor(c(1, 2))
  )
})

# select helpers ------------------------------
test_that("standardize regex", {
  expect_identical(
    standardize(mtcars, select = "pg", regex = TRUE),
    standardize(mtcars, select = "mpg")
  )
})

# standardize when only providing one of center/scale ---------------
test_that("standardize when only providing one of center/scale", {
  x <- 1:10
  expect_identical(
    as.vector(datawizard::standardize(x, center = FALSE)),
    x / sd(x)
  )
  expect_identical(
    as.vector(datawizard::standardize(x, center = 2)),
    (x - 2) / sd(x)
  )
  expect_identical(
    as.vector(datawizard::standardize(x, scale = FALSE)),
    as.vector(datawizard::center(x))
  )
  expect_identical(
    as.vector(datawizard::standardize(x, scale = 1.5)),
    (x - mean(x)) / 1.5
  )
})


# grouped data

test_that("unstandardize: grouped data", {
  skip_if_not_installed("poorman")

  # 1 group, 1 standardized var
  stand <- poorman::group_by(mtcars, cyl)
  stand <- standardize(stand, "mpg")
  unstand <- unstandardize(stand, select = "mpg")
  expect_identical(
    poorman::ungroup(unstand),
    mtcars,
    ignore_attr = TRUE
  )

  expect_s3_class(unstand, "grouped_df")

  # 2 groups, 1 standardized var
  set.seed(123)
  test <- iris
  test$grp <- sample(c("A", "B"), nrow(test), replace = TRUE)
  stand <- poorman::group_by(test, Species, grp)
  stand <- standardize(stand, "Sepal.Length")
  expect_identical(
    poorman::ungroup(unstandardize(stand, select = "Sepal.Length")),
    test
  )

  # 2 groups, 2 standardized vars
  set.seed(123)
  test <- iris
  test$grp <- sample(c("A", "B"), nrow(test), replace = TRUE)
  stand <- poorman::group_by(test, Species, grp)
  stand <- standardize(stand, c("Sepal.Length", "Petal.Length"))
  expect_identical(
    poorman::ungroup(unstandardize(stand, select = c("Sepal.Length", "Petal.Length"))),
    test
  )

  expect_s3_class(unstand, "grouped_df")

  # can't recover attributes
  stand <- poorman::group_by(iris, Species)
  stand <- standardize(stand, "Sepal.Length")
  attr(stand, "groups") <- NULL

  expect_error(
    unstandardize(stand, "Sepal.Length"),
    regexp = "Couldn't retrieve the necessary information"
  )

  # normalize applied on grouped data but unstandardize applied on ungrouped data
  stand <- poorman::group_by(mtcars, cyl)
  stand <- standardize(stand, "mpg")
  stand <- poorman::ungroup(stand)

  expect_error(
    unstandardize(stand, "mpg"),
    regexp = "must provide the arguments"
  )

  # standardize applied on grouped data but unstandardize applied different grouped
  # data
  stand <- poorman::group_by(stand, am)
  expect_error(
    unstandardize(stand, "mpg"),
    regexp = "Couldn't retrieve the necessary"
  )
})
