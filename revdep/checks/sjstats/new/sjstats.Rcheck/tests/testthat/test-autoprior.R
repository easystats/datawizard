.runThisTest <- Sys.getenv("RunAllsjstatsTests") == "yes"

if (.runThisTest) {

  if (suppressWarnings(
    require("testthat") &&
    require("sjstats") &&
    require("sjmisc") &&
    require("brms")
  )) {
    context("sjstats, autoprior")

    data(efc)
    efc$c172code <- as.factor(efc$c172code)
    efc$c161sex <- to_label(efc$c161sex)
    efc$neg_c_7d <- ifelse(efc$neg_c_7 < median(efc$neg_c_7, na.rm = TRUE), 0, 1)

    test_that("auto_prior", {
      mf <- formula(neg_c_7 ~ c161sex + c160age + c172code)
      expect_s3_class(auto_prior(mf, efc, TRUE), "brmsprior")

      mf <- formula(neg_c_7 ~ c161sex + c160age + c172code + c12hour + e17age)
      expect_s3_class(auto_prior(mf, efc, TRUE), "brmsprior")
      expect_error(auto_prior(mf, efc))

      mf <- formula(neg_c_7d ~ c161sex + c160age + c172code + e17age)
      expect_s3_class(auto_prior(mf, efc, FALSE), "brmsprior")
      expect_s3_class(auto_prior(mf, efc), "brmsprior")
      expect_warning(auto_prior(mf, efc, TRUE))
    })
  }
}
