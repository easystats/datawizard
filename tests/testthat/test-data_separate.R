test_that("data_separate: simple use case", {
  # simple case
  d_sep <- data.frame(
    x = c("1.a.6", "2.b.7", "3.c.8"),
    stringsAsFactors = FALSE
  )

  # basic
  expect_silent(data_separate(d_sep, verbose = FALSE))
  expect_message(
    {
      out <- data_separate(d_sep)
    },
    regex = "3 columns"
  )
  expect_identical(colnames(out), c("split_1", "split_2", "split_3"))
  expect_identical(out$split_1, c("1", "2", "3"))
  expect_identical(out$split_2, c("a", "b", "c"))

  # manual separator char
  out2 <- data_separate(d_sep, separator = "\\.", verbose = FALSE)
  expect_identical(out, out2)

  # non-existing separator char
  expect_message(
    data_separate(d_sep, separator = "_"),
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
  expect_silent(data_separate(d_sep, verbose = FALSE))
  expect_message(
    expect_message(
      expect_message(
        data_separate(d_sep),
        regex = "3 columns"
      ),
      regex = "have been dropped"
    ),
    regex = "filled with `NA`"
  )
  out <- data_separate(d_sep, verbose = FALSE)
  expect_identical(colnames(out), c("split_1", "split_2", "split_3"))
  expect_identical(out$split_1, c("1", "2", "3", "5"))
  expect_identical(out$split_2, c("a", "b", "c", "j"))
  expect_identical(out$split_3, c("6", "7", "8", NA))

  # fill missings left
  out <- data_separate(d_sep, fill = "left", verbose = FALSE)
  expect_identical(colnames(out), c("split_1", "split_2", "split_3"))
  expect_identical(out$split_1, c("1", "2", "3", NA))
  expect_identical(out$split_2, c("a", "b", "c", "5"))
  expect_identical(out$split_3, c("6", "7", "8", "j"))

  # merge extra right
  out <- data_separate(d_sep, extra = "merge_right", verbose = FALSE)
  expect_identical(colnames(out), c("split_1", "split_2", "split_3"))
  expect_identical(out$split_1, c("1", "2", "3", "5"))
  expect_identical(out$split_2, c("a", "b", "c", "j"))
  expect_identical(out$split_3, c("6", "7 d", "8", NA))

  # max columns
  out <- data_separate(d_sep, guess_columns = "max", verbose = FALSE)
  expect_equal(
    out,
    data.frame(
      split_1 = c("1", "2", "3", "5"),
      split_2 = c("a", "b", "c", "j"),
      split_3 = c("6", "7", "8", NA),
      split_4 = c(NA, "d", NA, NA),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  # min columns
  out <- data_separate(d_sep, guess_columns = "min", verbose = FALSE)
  expect_equal(
    out,
    data.frame(
      split_1 = c("1", "2", "3", "5"),
      split_2 = c("a", "b", "c", "j"),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  out <- data_separate(d_sep, guess_columns = "min", extra = "merge_left", verbose = FALSE)
  expect_equal(
    out,
    data.frame(
      split_1 = c("1 a", "2 b 7", "3 c", "5"),
      split_2 = c("6", "d", "8", "j"),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )

  out <- data_separate(d_sep, guess_columns = "max", fill = "left", verbose = FALSE)
  expect_equal(
    out,
    data.frame(
      split_1 = c(NA, "2", NA, NA),
      split_2 = c("1", "b", "3", NA),
      split_3 = c("a", "7", "c", "5"),
      split_4 = c("6", "d", "8", "j"),
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
  out <- data_separate(d_sep, select = "x", verbose = FALSE)
  expect_identical(colnames(out), c("split_1", "split_2", "split_3"))
  expect_identical(out$split_1, c("1", "2", "3", "5"))
  expect_identical(out$split_2, c("a", "b", "c", "j"))
  expect_identical(out$split_3, c("6", "7", "8", NA))

  out <- data_separate(d_sep, verbose = FALSE)
  expect_snapshot(print(out))

  out <- data_separate(d_sep, extra = "merge_right", verbose = FALSE)
  expect_snapshot(print(out))

  out <- data_separate(d_sep, new_columns = c("A", "B", "C"), extra = "merge_right", verbose = FALSE)
  expect_snapshot(print(out))

  out <- data_separate(d_sep, new_columns = c("A", "B", "C"), extra = "merge_right", append = TRUE, verbose = FALSE)
  expect_snapshot(print(out))

  out <- data_separate(d_sep, extra = "drop_left", verbose = FALSE)
  expect_snapshot(print(out))

  out <- data_separate(
    d_sep,
    new_columns = c("A", "B", "C"),
    fill = "value_right",
    extra = "merge_right",
    append = TRUE,
    verbose = FALSE
  )
  expect_snapshot(print(out))

  out <- data_separate(
    d_sep,
    new_columns = c("A", "B", "C"),
    fill = "value_right",
    extra = "merge_right",
    merge_multiple = TRUE,
    append = TRUE,
    verbose = FALSE
  )
  expect_snapshot(print(out))

  out <- data_separate(
    d_sep,
    new_columns = c("A", "B", "C"),
    merge_multiple = TRUE,
    append = TRUE,
    verbose = FALSE
  )
  expect_snapshot(print(out))
})


test_that("data_separate: numeric separator", {
  d_sep <- data.frame(
    x = c("Thisisalongstring", "Doeshe1losteverything", "Wereme2longornot"),
    stringsAsFactors = FALSE
  )

  expect_silent({
    out <- data_separate(d_sep, separator = c(5, 7, 8, 12), verbose = TRUE)
  })
  expect_equal(
    out,
    data.frame(
      split_1 = c("This", "Does", "Were"),
      split_2 = c("is", "he", "me"),
      split_3 = c("a", "1", "2"),
      split_4 = c("long", "lost", "long"),
      split_5 = c("string", "everything", "ornot"),
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
