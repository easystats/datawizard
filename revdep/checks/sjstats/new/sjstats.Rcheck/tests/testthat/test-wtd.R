if (require("testthat") && require("sjstats")) {
  data(efc)
  set.seed(123)
  efc$weight <- abs(rnorm(nrow(efc), 1, .3))

  test_that("wtd", {
    expect_equal(weighted_sd(efc$c12hour, weights = efc$weight), 51.18224, tolerance = 1e-5)
    expect_equal(weighted_sd(efc$c12hour, weights = NULL), 50.80504, tolerance = 1e-5)

    expect_equal(weighted_mean(efc$c12hour, weights = efc$weight), 42.80723, tolerance = 1e-5)
    expect_equal(weighted_mean(efc$c12hour, weights = NULL), 42.39911, tolerance = 1e-5)

    expect_equal(weighted_se(efc$c12hour, weights = efc$weight), 1.704182, tolerance = 1e-5)
    expect_equal(weighted_se(efc$c12hour, weights = NULL), 1.691623, tolerance = 1e-5)

    expect_equal(weighted_median(efc$c12hour, weights = efc$weight), 20, tolerance = 1e-5)
    expect_equal(weighted_median(efc$c12hour, weights = NULL), 20, tolerance = 1e-5)
  })

  test_that("weighted_chisqtest", {
    w <- weighted_chisqtest(efc, c161sex, c172code, weights = weight)
    expect_equal(w$estimate, 0.06668895, tolerance = 1e-5)
    expect_equal(w$p.value, 0.1517221, tolerance = 1e-5)

    w <- weighted_chisqtest(c161sex ~ c172code + weight, efc)
    expect_equal(w$estimate, 0.06668895, tolerance = 1e-5)
    expect_equal(w$p.value, 0.1517221, tolerance = 1e-5)
  })

  test_that("weighted_mannwhitney", {
    weighted_mannwhitney(efc, c12hour, c161sex, weights = weight)
    weighted_mannwhitney(c12hour ~ c161sex + weight, efc)
  })

  test_that("weighted_ttest", {
    weighted_ttest(efc, e17age, weights = weight)
    weighted_ttest(efc, e17age, c160age, weights = weight)
    weighted_ttest(e17age ~ e16sex + weight, efc)
    weighted_ttest(efc, e17age, c160age, weights = weight, ci.lvl = .8)
  })
}
