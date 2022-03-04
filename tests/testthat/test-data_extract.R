if (require("testthat")) {
  data(efc)

  test_that("data_extract works with select-length > 1", {
    expect_s3_class(data_extract(efc, select = c("e42dep", "c172code")), "data.frame")
  })
}
