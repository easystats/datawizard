test_that("text formatting helpers work as expected", {
  expect_equal(
    text_fullstop(c("something", "something else.")),
    c("something.", "something else.")
  )

  expect_equal(
    text_lastchar(c("ABC", "DEF"), n = 2),
    c("BC", "EF"),
    ignore_attr = TRUE
  )

  expect_equal(
    text_concatenate(c("First", "Second", "Last")),
    "First, Second and Last"
  )

  expect_equal(
    text_remove(c("one!", "two", "three!"), "!"),
    c("one", "two", "three")
  )

  long_text <- paste(rep("abc ", 100), collapse = "")
  expect_snapshot(cat(text_wrap(long_text, width = 50)))

  expect_equal(
    text_paste(c("A", "", "B"), c("42", "42", "42")),
    c("A, 42", "42", "B, 42")
  )
})
