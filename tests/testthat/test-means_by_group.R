test_that("meany_by_group", {
  data(efc)
  expect_snapshot(means_by_group(efc, "c12hour", "e42dep"))
  expect_snapshot(means_by_group(efc, "c12hour", "e42dep", ci = 0.99))
  expect_snapshot(means_by_group(efc, "c12hour", "e42dep", ci = NA))
  expect_snapshot(means_by_group(efc$c12hour, efc$e42dep))
  expect_snapshot(means_by_group(efc$c12hour, efc$e42dep, ci = NA))
})
