test_that("contr.deviation", {
  c.treatment <- solve(cbind(Intercept = 1, contr.treatment(3)))
  c.sum <- solve(cbind(Intercept = 1, contr.sum(3)))
  c.deviation <- solve(cbind(Intercept = 1, contr.deviation(3)))

  expect_equal(c.deviation[1,], c.sum[1,])
  expect_equal(c.deviation[-1,], c.treatment[-1,])
})
