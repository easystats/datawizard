test_that(".coerce_to_dataframe works for matrices", {
  mat <- matrix(c(1, 2, 3, 11, 12, 13), nrow = 2, ncol = 3, byrow = TRUE)

  expect_equal(
    .coerce_to_dataframe(mat),
    data.frame(
      V1 = c(1, 11),
      V2 = c(2, 12),
      V3 = c(3, 13)
    )
  )
})

test_that(".coerce_to_dataframe works for vectors and list", {
  expect_equal(
    .coerce_to_dataframe(1:3),
    data.frame(data = 1:3)
  )

  expect_equal(
    .coerce_to_dataframe(c("a", "b", "c")),
    data.frame(data = c("a", "b", "c"), stringsAsFactors = FALSE)
  )

  expect_equal(
    .coerce_to_dataframe(list(var1 = 1:3, var2 = 4:6)),
    data.frame(var1 = 1:3, var2 = 4:6)
  )
})

test_that(".coerce_to_dataframe errors correctly if can't coerce", {
  expect_error(
    .coerce_to_dataframe(list(var1 = 1:3, var2 = 4:5)),
    regexp = "object that can be coerced"
  )
})

test_that(".is_sorted works", {
  expect_true(.is_sorted(1:3))
  expect_true(.is_sorted(c("a", "b", "c")))
  expect_true(.is_sorted(factor(c("a", "b", "c"))))

  expect_false(.is_sorted(c(1, 3, 2)))
  expect_false(.is_sorted(c("b", "a", "c")))
  expect_false(.is_sorted(factor(c("b", "a", "c"))))
})
