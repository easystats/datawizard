# data(efc)
# set.seed(123)
# efc$weight <- abs(rnorm(nrow(efc), 1, .3))
#
# test_that("weighted centrality and dispersion measures work as expected", {
#   expect_equal(weighted_sd(efc$c12hour, weights = efc$weight), 51.18224, tolerance = 1e-5)
#   expect_equal(weighted_sd(efc$c12hour, weights = NULL), 50.80504, tolerance = 1e-5)
#
#   expect_equal(weighted_mean(efc$c12hour, weights = efc$weight), 42.80723, tolerance = 1e-5)
#   expect_equal(weighted_mean(efc$c12hour, weights = NULL), 42.39911, tolerance = 1e-5)
#
#   expect_equal(weighted_se(efc$c12hour, weights = efc$weight), 1.704182, tolerance = 1e-5)
#   expect_equal(weighted_se(efc$c12hour, weights = NULL), 1.691623, tolerance = 1e-5)
#
#   expect_equal(weighted_median(efc$c12hour, weights = efc$weight), 20, tolerance = 1e-5)
#   expect_equal(weighted_median(efc$c12hour, weights = NULL), 20, tolerance = 1e-5)
# })
