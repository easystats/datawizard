test_that("data_separate: simple use case", {
  # simple case
  d_sep <- data.frame(
    x = c("1.a.6", "2.b.7", "3.c.8"),
    stringsAsFactors = FALSE
  )

  expect_error(data_separate(d_sep), regex = "Either")

  # basic
  expect_silent(data_separate(d_sep, guess_columns = "mode", verbose = FALSE))
  expect_silent({
    out <- data_separate(d_sep, guess_columns = "mode")
  })
  expect_identical(colnames(out), c("x_1", "x_2", "x_3"))
  expect_identical(out$x_1, c("1", "2", "3"))
  expect_identical(out$x_2, c("a", "b", "c"))

  # manual separator char
  out2 <- data_separate(d_sep, separator = "\\.", guess_columns = "mode", verbose = FALSE)
  expect_identical(out, out2)

  # non-existing separator char
  expect_message(
    data_separate(d_sep, separator = "_", guess_columns = "mode"),
    regex = "Separator probably not found"
  )

  # column names
  out <- data_separate(d_sep, new_columns = c("A1", "B2", "C3"), verbose = FALSE)
  expect_identical(colnames(out), c("A1", "B2", "C3"))
  expect_identical(out$A1, c("1", "2", "3"))
  expect_identical(out$B2, c("a", "b", "c"))

  out <- data_separate(d_sep, new_columns = letters[1:3], append = TRUE)
  expect_equal(
    out,
    data.frame(
      x = c("1.a.6", "2.b.7", "3.c.8"),
      a = c("1", "2", "3"),
      b = c("a", "b", "c"),
      c = c("6", "7", "8"),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )
})


test_that("data_separate: convert between data_unite and data_separate", {
  d_unite <- data.frame(
    x = as.character(c(NA, 1:3)),
    y = c(letters[1:3], NA_character_),
    z = as.character(6:9),
    m = c("X", NA_character_, "Y", "Z"),
    n = c("NATION", "COUNTRY", "NATION", NA_character_),
    stringsAsFactors = FALSE
  )

  out1 <- data_unite(d_unite, new_column = "test")
  d_sep <- data_separate(out1, new_columns = c("x", "y", "z", "m", "n"), separator = "_")

  expect_identical(d_unite, d_sep)
})


test_that("data_separate: different number of values", {
  d_sep <- data.frame(
    x = c("1.a.6", "2.b.7.d", "3.c.8", "5.j"),
    stringsAsFactors = FALSE
  )

  # basic use-case
  expect_silent(data_separate(d_sep, guess_columns = "mode", verbose = FALSE))
  expect_message(
    expect_message(
      expect_message(
        data_separate(d_sep, guess_columns = "mode"),
        regex = "3 columns"
      ),
      regex = "have been dropped"
    ),
    regex = "filled with `NA`"
  )
  out <- data_separate(d_sep, guess_columns = "mode", verbose = FALSE)
  expect_identical(colnames(out), c("x_1", "x_2", "x_3"))
  expect_identical(out$x_1, c("1", "2", "3", "5"))
  expect_identical(out$x_2, c("a", "b", "c", "j"))
  expect_identical(out$x_3, c("6", "7", "8", NA))

  # fill missings left
  out <- data_separate(d_sep, guess_columns = "mode", fill = "left", verbose = FALSE)
  expect_identical(colnames(out), c("x_1", "x_2", "x_3"))
  expect_identical(out$x_1, c("1", "2", "3", NA))
  expect_identical(out$x_2, c("a", "b", "c", "5"))
  expect_identical(out$x_3, c("6", "7", "8", "j"))

  # merge extra right
  out <- data_separate(d_sep, guess_columns = "mode", extra = "merge_right", verbose = FALSE)
  expect_identical(colnames(out), c("x_1", "x_2", "x_3"))
  expect_identical(out$x_1, c("1", "2", "3", "5"))
  expect_identical(out$x_2, c("a", "b", "c", "j"))
  expect_identical(out$x_3, c("6", "7 d", "8", NA))

  # max columns
  out <- data_separate(d_sep, guess_columns = "max", verbose = FALSE)
  expect_equal(
    out,
    data.frame(
      x_1 = c("1", "2", "3", "5"),
      x_2 = c("a", "b", "c", "j"),
      x_3 = c("6", "7", "8", NA),
      x_4 = c(NA, "d", NA, NA),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  # min columns
  out <- data_separate(d_sep, guess_columns = "min", verbose = FALSE)
  expect_equal(
    out,
    data.frame(
      x_1 = c("1", "2", "3", "5"),
      x_2 = c("a", "b", "c", "j"),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  out <- data_separate(d_sep, guess_columns = "min", extra = "merge_left", verbose = FALSE)
  expect_equal(
    out,
    data.frame(
      x_1 = c("1 a", "2 b 7", "3 c", "5"),
      x_2 = c("6", "d", "8", "j"),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  out <- data_separate(d_sep, guess_columns = "max", fill = "left", verbose = FALSE)
  expect_equal(
    out,
    data.frame(
      x_1 = c(NA, "2", NA, NA),
      x_2 = c("1", "b", "3", NA),
      x_3 = c("a", "7", "c", "5"),
      x_4 = c("6", "d", "8", "j"),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )
})


test_that("data_separate: multiple columns", {
  d_sep <- data.frame(
    x = c("1.a.6", "2.b.7.d", "3.c.8", "5.j"),
    y = c("m.n.99", "77.f.g", "44.9", NA),
    stringsAsFactors = FALSE
  )

  # select works
  out <- data_separate(d_sep, select = "x", guess_columns = "mode", verbose = FALSE)
  expect_identical(colnames(out), c("y", "x_1", "x_2", "x_3"))
  expect_identical(out$x_1, c("1", "2", "3", "5"))
  expect_identical(out$x_2, c("a", "b", "c", "j"))
  expect_identical(out$x_3, c("6", "7", "8", NA))

  out <- data_separate(d_sep, guess_columns = "mode", verbose = FALSE)
  expect_snapshot(out)

  out <- data_separate(d_sep, guess_columns = "mode", extra = "merge_right", verbose = FALSE)
  expect_snapshot(out)

  out <- data_separate(d_sep, new_columns = c("A", "B", "C"), extra = "merge_right", verbose = FALSE)
  expect_snapshot(out)

  out <- data_separate(d_sep, new_columns = c("A", "B", "C"), extra = "merge_right", append = TRUE, verbose = FALSE)
  expect_snapshot(out)

  out <- data_separate(d_sep, guess_columns = "mode", extra = "drop_left", verbose = FALSE)
  expect_snapshot(out)

  out <- data_separate(
    d_sep,
    new_columns = c("A", "B", "C"),
    fill = "value_right",
    extra = "merge_right",
    append = TRUE,
    verbose = FALSE
  )
  expect_snapshot(out)

  out <- data_separate(
    d_sep,
    new_columns = c("A", "B", "C"),
    fill = "value_right",
    extra = "merge_right",
    merge_multiple = TRUE,
    append = TRUE,
    verbose = FALSE
  )
  expect_snapshot(out)

  out <- data_separate(
    d_sep,
    new_columns = c("A", "B", "C"),
    merge_multiple = TRUE,
    append = TRUE,
    verbose = FALSE
  )
  expect_snapshot(out)

  out <- data_separate(d_sep, guess_columns = "mode", fill = "value_left", verbose = FALSE)
  expect_snapshot(out)
})


test_that("data_separate: multiple columns, different lengths", {
  d_sep <- data.frame(
    x = c("1.a.6", "2.b.7.d", "3.c.8", "5.j"),
    y = c("m.n.99.22", "77.f.g.34", "44.9", NA),
    stringsAsFactors = FALSE
  )

  # separate column names
  out <- data_separate(
    d_sep,
    select = c("x", "y"),
    new_columns = list(x = c("A", "B", "C"), y = c("EE", "FF", "GG")),
    verbose = FALSE
  )
  expect_named(out, c("A", "B", "C", "EE", "FF", "GG"))
  expect_snapshot(out)

  out <- data_separate(
    d_sep,
    select = c("x", "y"),
    new_columns = list(x = c("A", "B", "C"), y = c("EE", "FF", "GG", "HH")),
    verbose = FALSE
  )
  expect_named(out, c("A", "B", "C", "EE", "FF", "GG", "HH"))
  expect_snapshot(out)
})


test_that("data_separate: numeric separator", {
  d_sep <- data.frame(
    x = c("Thisisalongstring", "Doeshe1losteverything", "Wereme2longornot"),
    stringsAsFactors = FALSE
  )

  expect_silent({
    out <- data_separate(d_sep, guess_columns = "mode", separator = c(5, 7, 8, 12), verbose = TRUE)
  })
  expect_equal(
    out,
    data.frame(
      x_1 = c("This", "Does", "Were"),
      x_2 = c("is", "he", "me"),
      x_3 = c("a", "1", "2"),
      x_4 = c("long", "lost", "long"),
      x_5 = c("string", "everything", "ornot"),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  d_sep <- data.frame(
    x = c("Thisisalongstring", "Doeshe1losteverything"),
    y = c("Wereme2longornot", NA),
    stringsAsFactors = FALSE
  )
  expect_silent({
    out <- data_separate(d_sep, separator = c(5, 7, 8, 12), new_columns = LETTERS[1:5])
  })
  expect_equal(
    out,
    data.frame(
      A = c("This", "Does"),
      B = c("is", "he"),
      C = c("a", "1"),
      D = c("long", "lost"),
      E = c("string", "everything"),
      A.1 = c("Were", NA),
      B.1 = c("me", NA),
      C.1 = c("2", NA),
      D.1 = c("long", NA),
      E.1 = c("ornot", NA),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  expect_error(
    data_separate(d_sep, separator = c(5, 7, 8, 12), new_columns = LETTERS[1:6]),
    regex = "went wrong"
  )
})


test_that("data_separate: fail if invalid column selected", {
  d_sep <- data.frame(
    x = c("1.a.6", "2.b.7.d", "3.c.8", "5.j"),
    y = c("m.n.99", "77.f.g", "44.9", NA),
    stringsAsFactors = FALSE
  )
  expect_warning(
    expect_message(
      data_separate(d_sep, guess_columns = "mode", select = "z"),
      reg = "not found"
    ),
    regex = "misspelled?"
  )
  expect_identical(
    data_separate(d_sep, guess_columns = "mode", select = "z", verbose = FALSE),
    d_sep
  )
  expect_snapshot(data_separate(d_sep, guess_columns = "mode", select = NULL))
})


test_that("data_separate: numeric column", {
  d_sep <- data.frame(
    x = c(154353523, 535543532, 12342422, 15454334535),
    y = c("m.n.99", "77.f.g", "44.9", NA),
    stringsAsFactors = FALSE
  )
  expect_message(data_separate(d_sep, guess_columns = "mode", select = "x"), regex = "Separator probably")
  out <- data_separate(d_sep, guess_columns = "mode", select = "x", separator = c(3, 6, 9))
  expect_snapshot(out)
})
