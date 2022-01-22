test_that("compact character works as expected", {
  # atomic vectors
  expect_equal(compact_character(c("x", "y", NA)), c("x", "y", NA))
  expect_equal(compact_character(c("x", NULL, "", "y")), c("x", "y"))

  # lists
  expect_equal(compact_character(list("x", "y", NA)), list("x", "y", NULL))
  expect_equal(compact_character(list("x", NULL, "", "y")), list("x", "y"))

  # scalar
  expect_equal(compact_character(""), character(0))
})
