test_that("data_remove works as expected", {
  expect_equal(
    data_remove(BOD, "Time"),
    structure(list(demand = c(8.3, 10.3, 19, 16, 15.6, 19.8)),
      class = "data.frame",
      row.names = c(NA, 6L),
      reference = "A1.4, p. 270"
    )
  )
})
