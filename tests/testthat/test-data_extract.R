if (require("testthat")) {
  data(efc)

  test_that("data_extract works with select-length > 1", {
    # works with multiple selects
    expect_s3_class(data_extract(efc, select = c("e42dep", "c172code")), "data.frame")

    # colnames properly set
    expect_equal(
      colnames(data_extract(efc, select = c("e42dep", "c172code"))),
      c("e42dep", "c172code")
    )

    # properly extract vector, w/o naming
    expect_equal(data_extract(efc, select = "e42dep"), efc$e42dep)

    # properly extract vector, with naming
    x <- data_extract(efc, select = "e42dep", name = "c172code")
    expect_equal(names(x), as.character(efc$c172code))
  })
}
