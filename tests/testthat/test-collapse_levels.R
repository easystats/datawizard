# Oracle for this file: hand counts. With the default `min_prop = 0.05` and
# n = 100, a level is sparse below 5 values.
counts <- function(x) {
  tab <- table(x)
  stats::setNames(as.vector(tab), names(tab))
}
lump <- function(...) {
  k <- c(...)
  factor(rep(names(k), k), levels = names(k))
}
ord <- function(...) {
  k <- c(...)
  factor(rep(names(k), k), levels = names(k), ordered = TRUE)
}

test_that("collapse_levels merges sparse nominal levels", {
  # c and d are sparse, and c/d = 6 is not
  x <- lump(a = 80, b = 14, c = 3, d = 3)
  expect_identical(
    counts(collapse_levels(x)),
    c(a = 80L, b = 14L, "c/d" = 6L)
  )
  expect_identical(
    counts(collapse_levels(x, other = "Other")),
    c(a = 80L, b = 14L, Other = 6L)
  )
  expect_identical(
    counts(collapse_levels(x, sep = "+")),
    c(a = 80L, b = 14L, "c+d" = 6L)
  )

  # c alone is still sparse, so the next-smallest level joins it
  expect_identical(
    counts(collapse_levels(lump(a = 50, b = 47, c = 3))),
    c(a = 50L, "b/c" = 50L)
  )
  # c pulls in a, and the label follows the level order
  expect_identical(
    counts(collapse_levels(lump(a = 6, b = 90, c = 4))),
    c(b = 90L, "a/c" = 10L)
  )
  # a tie for the pulled level goes to the earlier one
  expect_identical(
    counts(collapse_levels(lump(a = 48, b = 48, c = 4))),
    c(b = 48L, "a/c" = 52L)
  )
  # c/d = 4 is still sparse, so b joins
  expect_identical(
    counts(collapse_levels(lump(a = 88, b = 8, c = 2, d = 2))),
    c(a = 88L, "b/c/d" = 12L)
  )
})

test_that("collapse_levels uses min_n or min_prop, whichever flags a level", {
  x <- lump(a = 50, b = 30, c = 12, d = 8)
  merged <- c(a = 50L, b = 30L, "c/d" = 20L)
  # d = 8 is below min_n = 10 only, and d's 8% is below min_prop = 0.1 only
  expect_identical(
    counts(collapse_levels(x, min_prop = 0.01, min_n = 10)),
    merged
  )
  expect_identical(
    counts(collapse_levels(x, min_prop = 0.1, min_n = 5)),
    merged
  )
  # a share equal to min_prop is not sparse: 5 / 100 = 0.05
  y <- lump(a = 90, b = 5, c = 5)
  expect_identical(collapse_levels(y), y)
})

test_that("collapse_levels merges sparse ordered levels into a neighbor", {
  # an end level has one neighbor
  out <- collapse_levels(ord(a = 2, b = 30, c = 30, d = 38))
  expect_s3_class(out, "ordered")
  expect_identical(counts(out), c("a/b" = 32L, c = 30L, d = 38L))
  # an interior level goes to its smaller neighbor, up or down
  expect_identical(
    counts(collapse_levels(ord(a = 40, b = 3, c = 10, d = 47))),
    c(a = 40L, "b/c" = 13L, d = 47L)
  )
  expect_identical(
    counts(collapse_levels(ord(a = 10, b = 3, c = 40, d = 47))),
    c("a/b" = 13L, c = 40L, d = 47L)
  )
  # a tie goes toward the middle position, 2.5 of 4 levels
  expect_identical(
    counts(collapse_levels(ord(a = 25, b = 4, c = 25, d = 46))),
    c(a = 25L, "b/c" = 29L, d = 46L)
  )
  expect_identical(
    counts(collapse_levels(ord(a = 46, b = 25, c = 4, d = 25))),
    c(a = 46L, "b/c" = 29L, d = 25L)
  )
  # a tie at the middle position, 3 of 5 levels, goes to the lower level
  expect_identical(
    counts(collapse_levels(ord(a = 40, b = 8, c = 4, d = 8, e = 40))),
    c(a = 40L, "b/c" = 12L, d = 8L, e = 40L)
  )
  # at min_prop = 0.1: a + b = 5, then c = 4 + 5 = 9, then 9 + d = 50
  expect_identical(
    counts(collapse_levels(
      ord(a = 2, b = 3, c = 4, d = 41, e = 50),
      min_prop = 0.1
    )),
    c("a/b/c/d" = 50L, e = 50L)
  )
  # an unused level is no neighbor: b = 3 goes to c = 47, not to a = 50
  x <- ord(a = 50, b = 3, u = 0, c = 47)
  expect_identical(counts(collapse_levels(x)), c(a = 50L, "b/c" = 50L))
  # other has no effect, and a merged name may equal it
  x <- ord(a = 2, b = 30, c = 30, d = 38)
  expect_identical(collapse_levels(x, other = "c"), collapse_levels(x))
})

test_that("collapse_levels stops with two ordered levels", {
  # b = 3 is sparse, but only two levels are left
  x <- ord(a = 97, b = 3)
  expect_warning(collapse_levels(x), "`b`", fixed = TRUE)
  expect_identical(suppressWarnings(collapse_levels(x)), x)
  # a + b = 2 and c = 1 are both below min_n = 10
  x <- ord(a = 1, b = 1, c = 1)
  expect_warning(collapse_levels(x, min_n = 10), "`a/b`, `c`", fixed = TRUE)
  out <- suppressWarnings(collapse_levels(x, min_n = 10))
  expect_identical(counts(out), c("a/b" = 2L, c = 1L))
})


test_that("collapse_levels leaves x unchanged when it can't merge", {
  # no sparse level, unused level kept
  x <- lump(a = 50, b = 50, u = 0)
  expect_identical(collapse_levels(x, min_prop = 0, min_n = 0), x)
  # only missing values
  x <- factor(c(NA, NA), levels = c("a", "b"))
  expect_identical(collapse_levels(x), x)

  # every level would merge: b = 3 is sparse and pulls in a; u stays
  x <- lump(a = 97, b = 3, u = 0)
  expect_warning(collapse_levels(x), "only one level", fixed = TRUE)
  expect_silent(collapse_levels(x, verbose = FALSE))
  expect_identical(collapse_levels(x, verbose = FALSE), x)
  # one used level below min_n = 10
  x <- ord(a = 5, u = 0)
  expect_warning(collapse_levels(x, min_n = 10), "only one level", fixed = TRUE)
  expect_identical(collapse_levels(x, min_n = 10, verbose = FALSE), x)

  # a sparse unused level alone changes the result
  x <- lump(a = 50, b = 50, u = 0)
  expect_identical(levels(collapse_levels(x)), c("a", "b"))
})

test_that("collapse_levels keeps missing values", {
  # NA is not in n: 3 / 100 is below 0.05
  x <- factor(c(rep(c("a", "b", "c", "d"), c(80, 14, 3, 3)), NA))
  out <- collapse_levels(x)
  expect_identical(which(is.na(out)), 101L)
  expect_identical(levels(out), c("a", "b", "c/d"))

  # an NA level with values is left out of n, and a value at an NA code stays NA
  x <- addNA(x)
  y <- as.integer(x)
  y[1] <- NA_integer_
  attributes(y) <- attributes(x)
  out <- collapse_levels(y)
  expect_identical(levels(out), c("a", "b", "c/d", NA))
  expect_identical(as.integer(out)[c(1, 2, 100, 101)], c(NA, 1L, 3L, 4L))

  # an NA level is never unused
  x <- addNA(factor(c("a", "b")))
  expect_identical(collapse_levels(x), x)

  # NA values and an NA level are not in n: 5 / 100 is not below 0.05, though
  # 5 / 101 would be
  x <- factor(c(rep(c("a", "b", "c"), c(90, 5, 5)), NA))
  expect_identical(collapse_levels(x), x)
  expect_identical(collapse_levels(addNA(x)), addNA(x))
})

test_that("collapse_levels keeps class and attributes", {
  x <- lump(a = 80, b = 14, c = 3, d = 3)
  names(x) <- seq_along(x)
  attr(x, "label") <- "A label"
  attr(x, "labels") <- c(one = 1, two = 2, three = 3, four = 4)
  stats::contrasts(x) <- stats::contr.sum(4)
  class(x) <- c("my_factor", "factor")
  out <- collapse_levels(x)
  expect_s3_class(out, "my_factor")
  expect_named(out, names(x))
  expect_identical(attr(out, "label"), "A label")
  # both were made for the old levels
  expect_null(attr(out, "labels"))
  expect_null(attr(out, "contrasts"))

  # a contrasts name does not depend on the levels
  x <- lump(a = 80, b = 14, c = 3, d = 3)
  stats::contrasts(x) <- "contr.sum"
  expect_identical(attr(collapse_levels(x), "contrasts"), "contr.sum")
})

test_that("collapse_levels works on character vectors", {
  x <- c(as.character(lump(a = 80, b = 14, c = 3, d = 3)), NA)
  names(x) <- seq_along(x)
  out <- collapse_levels(x)
  expect_type(out, "character")
  expect_named(out, names(x))
  expect_identical(unname(out), as.character(collapse_levels(factor(x))))
  expect_true(is.na(out[[101]]))

  # "" = 3 and c = 2 are sparse; plain indexing by name can't find ""
  x <- rep(c("a", "b", "c", ""), c(90, 5, 2, 3))
  expect_identical(levels(collapse_levels(factor(x))), c("a", "b", "/c"))
  expect_identical(table(collapse_levels(x))[["/c"]], 5L)
})

test_that("collapse_levels handles other classes", {
  expect_message(collapse_levels(1:3), "Only factors", fixed = TRUE)
  expect_identical(collapse_levels(1:3, verbose = FALSE), 1:3)
})

test_that("collapse_levels works on data frames", {
  f <- lump(a = 80, b = 14, c = 3, d = 3)
  d <- data.frame(
    id = 1:100,
    nom = f,
    ordi = ord(a = 40, b = 3, c = 10, d = 47),
    txt = as.character(f),
    keep = rev(f),
    stringsAsFactors = FALSE
  )
  expect_silent(collapse_levels(d, exclude = "keep"))
  out <- collapse_levels(d, exclude = "keep")
  expect_identical(out[c("id", "keep")], d[c("id", "keep")])
  expect_identical(out$nom, collapse_levels(d$nom))
  expect_identical(out$ordi, collapse_levels(d$ordi))
  expect_identical(out$txt, collapse_levels(d$txt))

  # a copy for each factor or character column, none for id
  out <- collapse_levels(d, select = c("id", "nom", "txt"), append = TRUE)
  expect_named(out, c(names(d), "nom_r", "txt_r"))
  expect_identical(out[names(d)], d)
  expect_identical(levels(out$nom_r), c("a", "b", "c/d"))
  # no column to copy
  expect_identical(collapse_levels(d["id"], append = TRUE), d["id"])

  # a grouped data frame keeps its grouping column
  f <- lump(a = 50, b = 47, c = 3)
  g <- data_group(data.frame(g = f, x = f), "g")
  out <- collapse_levels(g)
  expect_identical(levels(out$g), c("a", "b", "c"))
  expect_identical(levels(out$x), c("a", "b/c"))

  # warnings name the column
  d <- data.frame(nom = lump(a = 97, b = 3))
  expect_warning(collapse_levels(d), "column `nom`", fixed = TRUE)
})

test_that("collapse_levels pools counts for ordered items", {
  # own merges would differ: i1 "a/b", i2 "a/b/c", i3 none
  # pooled: a = 10, b = 56, c = 79, d = 155; a's 10 / 300 is sparse, so "a/b"
  d <- data.frame(
    i1 = ord(a = 2, b = 48, c = 25, d = 25),
    i2 = ord(a = 2, b = 2, c = 48, d = 48),
    i3 = ord(a = 6, b = 6, c = 6, d = 82)
  )
  expect_identical(levels(collapse_levels(d)$i2), c("a/b/c", "d"))
  out <- collapse_levels(d, pool = TRUE)
  expect_identical(counts(out$i1), c("a/b" = 50L, c = 25L, d = 25L))
  expect_identical(counts(out$i2), c("a/b" = 4L, c = 48L, d = 48L))
  expect_identical(counts(out$i3), c("a/b" = 12L, c = 6L, d = 82L))
  expect_s3_class(out$i1, "ordered")

  # each column keeps its own NA level, which is not a difference in levels
  d$i1 <- addNA(d$i1)
  out <- collapse_levels(d, pool = TRUE)
  expect_identical(levels(out$i1), c("a/b", "c", "d", NA))
  expect_identical(levels(out$i2), c("a/b", "c", "d"))

  # a's share is 8 / 200 with i1 and i2. It is 18 / 300 with i3, which is not
  # sparse
  d <- data.frame(
    i1 = ord(a = 2, b = 8, c = 90),
    i2 = ord(a = 6, b = 88, c = 6),
    i3 = ord(a = 10, b = 82, c = 8)
  )
  expect_identical(collapse_levels(d, pool = TRUE), d)
  out <- collapse_levels(d, select = c("i1", "i2"), pool = TRUE)
  expect_identical(counts(out$i1), c("a/b" = 10L, c = 90L))
  expect_identical(out$i3, d$i3)
})

test_that("collapse_levels pools counts for nominal items", {
  # pooled: a = 189, b = 5, c = 5, d = 101; b and c are below min_n = 8 and
  # b/c = 10 is not; alone, z would have every level merged. d is unused in x
  # only, so it stays
  d <- data.frame(id = 1:100)
  d$x <- factor(
    rep(c("a", "b", "c"), c(94, 3, 3)),
    levels = c("a", "b", "c", "d")
  )
  d$y <- factor(rep(c("b", "c", "d"), c(2, 2, 96)))
  d$z <- factor(rep(c("a", "d"), c(95, 5)))
  out <- collapse_levels(d, min_prop = 0, min_n = 8, pool = TRUE)
  expect_identical(counts(out$x), c(a = 94L, d = 0L, "b/c" = 6L))
  expect_identical(counts(out$y), c(a = 0L, d = 96L, "b/c" = 4L))
  expect_identical(counts(out$z), c(a = 95L, d = 5L, "b/c" = 0L))

  # alone, s has no sparse level. pooled: a = 130, b = 45, c = 18, d = 7;
  # d = 7 of 200 is sparse and pulls in c
  d <- data.frame(
    f = lump(a = 90, b = 5, c = 3, d = 2),
    s = as.character(lump(a = 40, b = 40, c = 15, d = 5)),
    stringsAsFactors = FALSE
  )
  out <- collapse_levels(d, pool = TRUE)
  expect_identical(levels(out$f), c("a", "b", "c/d"))
  expect_identical(out$s[100], "c/d")
  expect_identical(collapse_levels(d$s), d$s)

  # nothing is sparse, so the columns keep their own levels
  d <- data.frame(x = factor(c("a", "b")), y = factor(c("c", "d")))
  expect_identical(collapse_levels(d, pool = TRUE), d)
})

test_that("collapse_levels gives errors", {
  x <- lump(a = 80, b = 14, c = 3, d = 3)
  # other equals a kept level or a merged level
  expect_error(collapse_levels(x, other = "a"), "already", fixed = TRUE)
  expect_error(collapse_levels(x, other = "c"), "already", fixed = TRUE)
  # the joined label equals a kept level
  y <- lump(a = 88, b = 2, c = 3, "b/c" = 7)
  expect_error(collapse_levels(y), "`b/c`", fixed = TRUE)
  # two merged levels get one name: a + bc and ab + c are both "abc"
  y <- ord(a = 2, bc = 48, ab = 2, c = 48)
  expect_error(collapse_levels(y, sep = ""), "`abc`", fixed = TRUE)
  # no name error if nothing merges, or if the name is only an unused level
  y <- lump(a = 50, b = 50)
  expect_identical(collapse_levels(y, other = "a"), y)
  y <- lump(a = 80, b = 14, c = 3, d = 3, u = 0)
  expect_identical(levels(collapse_levels(y, other = "u")), c("a", "b", "u"))

  expect_error(collapse_levels(x, min_prop = 1.1), "`min_prop`", fixed = TRUE)
  expect_error(collapse_levels(x, min_n = -1), "`min_n`", fixed = TRUE)
  expect_error(collapse_levels(x, other = NA), "`other`", fixed = TRUE)
  expect_error(collapse_levels(x, sep = c("a", "b")), "`sep`", fixed = TRUE)

  d <- data.frame(x = x, y = as.ordered(x))
  expect_error(collapse_levels(d, pool = NA), "`pool`", fixed = TRUE)
  # ordered with other columns, or ordered with different levels
  expect_error(collapse_levels(d, pool = TRUE), "`pool = TRUE`", fixed = TRUE)
  d <- data.frame(
    x = ord(a = 5, b = 5),
    y = ordered(rep(c("a", "b"), c(5, 5)), levels = c("b", "a"))
  )
  expect_error(collapse_levels(d, pool = TRUE), "`pool = TRUE`", fixed = TRUE)
})
