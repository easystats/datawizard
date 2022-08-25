data(efc)

test_that("data_group attributes", {
  x <- data_group(efc, "c172code")
  expect_equal(
    attributes(x)$groups,
    structure(
      list(
        c172code = c(1, 2, 3, NA),
        .rows = list(
          c(3L, 14L, 30L, 32L, 36L, 77L, 91L, 99L),
          c(
            1L, 2L, 4L, 5L, 6L, 7L, 8L, 10L, 11L, 12L, 16L, 17L, 18L,
            21L, 22L, 23L, 24L, 25L, 26L, 28L, 29L, 31L, 33L, 34L, 35L,
            37L, 38L, 39L, 40L, 42L, 44L, 45L, 46L, 47L, 50L, 51L, 52L,
            53L, 54L, 56L, 57L, 59L, 60L, 62L, 65L, 68L, 69L, 71L, 72L,
            73L, 76L, 78L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L,
            90L, 92L, 93L, 96L, 100L
          ),
          c(
            13L, 15L, 19L, 20L, 27L, 41L, 43L, 55L, 58L, 64L, 66L, 67L,
            74L, 75L, 79L, 89L
          ),
          c(9L, 48L, 49L, 61L, 63L, 70L, 94L, 95L, 97L, 98L)
        )
      ),
      row.names = c(2L, 1L, 4L, 3L), class = "data.frame", .drop = TRUE
    )
  )
  expect_s3_class(x, "grouped_df")
})


test_that("data_group attributes", {
  x <- data_group(efc, "c172code")
  if (requireNamespace("poorman", quietly = TRUE)) {
    out <- poorman::summarise(x, mw = mean(c12hour, na.rm = TRUE))
    expect_equal(out$mw, c(87.125, 94.046875, 75), tolerance = 1e-3)
  }
})

# select helpers ------------------------------
test_that("data_group regex", {
  expect_equal(
    attributes(data_group(mtcars, select = "yl", regex = TRUE))$groups[[1]],
    sort(unique(mtcars$cyl))
  )
})
