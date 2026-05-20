skip_on_cran()
skip_if_not_installed("discovr")

test_that("discovr-book-examples, chap 8.8", {
  data(album_sales, package = "discovr")
  set.seed(123)
  out <- describe_distribution(
    album_sales,
    range = FALSE,
    ci = 0.95,
    iterations = 500
  )
  expect_snapshot(print(out, table_width = Inf))
})
