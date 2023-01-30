if (require("testthat") && require("sjstats") && require("dplyr")) {
  data(efc)
  set.seed(123)
  efc$weight <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))
  efc_grouped <- group_by(efc, c172code)

  test_that("means_by_group", {
    expect_equal(means_by_group(efc, c12hour, e42dep)$mean, c(9.90909, 17.53778, 34.51961, 75.90132, 42.4384), tolerance = 1e-3)
  })

  test_that("means_by_group, weighting", {
    w <- "weight"
    expect_equal(means_by_group(efc, c12hour, e42dep, weights = weight)$mean, c(9.43932, 17.28629, 35.16486, 79.23457, 43.0544), tolerance = 1e-3)
    expect_equal(means_by_group(efc, c12hour, e42dep, weights = "weight")$mean, c(9.43932, 17.28629, 35.16486, 79.23457, 43.0544), tolerance = 1e-3)
    expect_equal(means_by_group(efc, c12hour, e42dep, weights = w)$mean, c(9.43932, 17.28629, 35.16486, 79.23457, 43.0544), tolerance = 1e-3)
  })

  test_that("means_by_group, grouping", {
    m <- means_by_group(efc_grouped, c12hour, e42dep)
    expect_equal(length(m), 3)
  })

  test_that("means_by_group, grouped weighting", {
    w <- "weight"
    means_by_group(efc_grouped, c12hour, e42dep, weights = weight)
    means_by_group(efc_grouped, c12hour, e42dep, weights = "weight")
    means_by_group(efc_grouped, c12hour, e42dep, weights = w)
  })

}
