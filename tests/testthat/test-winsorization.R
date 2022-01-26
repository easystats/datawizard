
test_that("testing Winsorization of factors", {
  expect_equal(winsorize(as.factor(mtcars$am)), as.factor(mtcars$am))
})

test_that("with missing values", {
  expect_snapshot(suppressWarnings(head(winsorize(na.omit(ggplot2::msleep$brainwt)))))
  expect_equal(length(winsorize(as.factor(ggplot2::msleep$vore))), 83)
})
