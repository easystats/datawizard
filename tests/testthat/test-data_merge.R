
data(mtcars)
x <- mtcars[3:5, 1:3]
y <- mtcars[30:32, c(1, 4:5)]
z <- mtcars[11:13, 6:8]

x$id <- 1:3
y$id <- 2:4
z$id <- 3:5

# left -----------------------

test_that("left-join", {
  skip_if_not_installed("poorman")

  out <- data_merge(x, y, join = "left")
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "id", "hp", "drat"))
  expect_equal(dim(out), c(3, 6))
  expect_equal(out, suppressMessages(poorman::left_join(x, y)))

  out <- data_merge(x, y, join = "left", by = "id")
  expect_equal(colnames(out), c("cyl", "disp", "id", "hp", "drat", "mpg.x", "mpg.y"))
  expect_equal(out$disp, poorman::left_join(x, y, by = "id")$disp)
  expect_equal(dim(out), c(3, 7))

  out <- data_merge(x, y, join = "left", by = "mpg")
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "hp", "drat", "id.x", "id.y"))
  expect_equal(out$disp, poorman::left_join(x, y, by = "mpg")$disp)
  expect_equal(out$mpg, poorman::left_join(x, y, by = "mpg")$mpg)
  expect_equal(dim(out), c(3, 7))
})


# semi/anti -----------------------

# errors
test_that("semi-anti-join", {
  expect_error(data_merge(x, y, join = "semi"))
  expect_error(data_merge(x, y, join = "anti"))
})


# right -----------------------

test_that("right-join", {
  skip_if_not_installed("poorman")

  out <- data_merge(x, y, join = "right")
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "id", "hp", "drat"))
  expect_equal(dim(out), c(3, 6))
  # in data_merge(), we keep sorting from x, so do some preparation here
  poor_out <- suppressMessages(poorman::right_join(x, y))
  poor_out <- poor_out[order(poor_out$id), ]
  row.names(poor_out) <- 1:nrow(poor_out)
  expect_equal(out, poor_out)

  out <- data_merge(x, y, join = "right", by = "id")
  expect_equal(colnames(out), c("cyl", "disp", "id", "hp", "drat", "mpg.x", "mpg.y"))
  # in data_merge(), we keep sorting from x, so do some preparation here
  poor_out <- suppressMessages(poorman::right_join(x, y, by = "id"))
  poor_out <- poor_out[order(poor_out$id), ]
  expect_equal(out$disp, poor_out$disp)
  expect_equal(dim(out), c(3, 7))

  out <- data_merge(x, y, join = "right", by = "mpg")
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "hp", "drat", "id.x", "id.y"))
  # in data_merge(), we keep sorting from x, so do some preparation here
  poor_out <- suppressMessages(poorman::right_join(x, y, by = "mpg"))
  poor_out <- poor_out[order(poor_out$id.y, decreasing = TRUE), ]
  out <- out[order(out$id.y, decreasing = TRUE), ]
  expect_equal(out$disp, poor_out$disp)
  expect_equal(out$mpg, poor_out$mpg)
  expect_equal(dim(out), c(3, 7))
})


# inner -----------------------

test_that("inner-join", {
  skip_if_not_installed("poorman")

  out <- data_merge(x, y, join = "inner")
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "id", "hp", "drat"))
  expect_equal(dim(out), c(0, 6))

  out <- data_merge(x, y, join = "inner", by = "id")
  expect_equal(colnames(out), c("cyl", "disp", "id", "hp", "drat", "mpg.x", "mpg.y"))
  expect_equal(out$disp, poorman::inner_join(x, y, by = "id")$disp)
  expect_equal(dim(out), c(2, 7))

  out <- data_merge(x, y, join = "inner", by = "mpg")
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "hp", "drat", "id.x", "id.y"))
  expect_equal(out$disp, poorman::inner_join(x, y, by = "mpg")$disp)
  expect_equal(dim(out), c(1, 7))
})


# full -----------------------

test_that("full-join", {
  out <- data_merge(x, y, join = "full")
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "id", "hp", "drat"))
  expect_equal(dim(out), c(6, 6))
  expect_equal(out$mpg, c(22.8, 21.4, 18.7, 19.7, 15, 21.4), tolerance = 1e-2)
  expect_equal(out$id, c(1, 2, 3, 2, 3, 4), tolerance = 1e-2)

  out <- data_merge(x, y, join = "full", by = "id")
  expect_equal(colnames(out), c("cyl", "disp", "id", "hp", "drat", "mpg.x", "mpg.y"))
  expect_equal(dim(out), c(4, 7))
  expect_equal(out$mpg.x, c(22.8, 21.4, 18.7, NA), tolerance = 1e-2)
  expect_equal(out$id, 1:4, tolerance = 1e-2)

  out <- data_merge(x, y, join = "full", by = "mpg")
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "hp", "drat", "id.x", "id.y"))
  expect_equal(dim(out), c(5, 7))
  expect_equal(out$mpg, c(22.8, 21.4, 18.7, 19.7, 15), tolerance = 1e-2)
  expect_equal(out$id.x, c(1, 2, 3, NA, NA), tolerance = 1e-2)

  out <- data_merge(x, y, join = "full", by = c("id", "mpg"))
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "id", "hp", "drat"))
  expect_equal(dim(out), c(6, 6))
  expect_equal(out$mpg, c(22.8, 21.4, 18.7, 19.7, 15, 21.4), tolerance = 1e-2)
  expect_equal(out$id, c(1, 2, 3, 2, 3, 4), tolerance = 1e-2)
})


# bind -----------------------

test_that("bind-join", {
  skip_if_not_installed("poorman")

  out <- data_merge(x, y, join = "bind")
  poor_out <- poorman::bind_rows(x, y)
  row.names(poor_out) <- 1:nrow(poor_out)
  expect_equal(colnames(out), c("mpg", "cyl", "disp", "id", "hp", "drat"))
  expect_equal(dim(out), c(6, 6))
  expect_equal(out, poor_out)

  # by will be ignored
  out <- data_merge(x, y, join = "bind", by = "id")
  expect_equal(out, poor_out)

  # by will be ignored
  out <- data_merge(x, y, join = "bind", by = "mpg")
  expect_equal(out, poor_out)

  # by will be ignored
  out <- data_merge(x, y, join = "bind", by = c("id", "mpg"))
  expect_equal(out, poor_out)
})

# joins without common columns -----------------------

test_that("bind-join", {
  skip_if_not_installed("poorman")

  x2 <- mtcars[3:5, 1:3]
  y2 <- mtcars[30:32, 4:6]

  expect_warning(
    data_merge(x2, y2, join = "full"),
    "Found no matching columns in the data frames."
  )

  expect_equal(
    suppressWarnings(data_merge(x2, y2, join = "full")),
    poorman::full_join(x2, y2),
    ignore_attr = TRUE
  )

  expect_equal(
    data_merge(x2, y2, join = "bind"),
    poorman::bind_rows(x2, y2),
    ignore_attr = TRUE
  )
})

# joins without common columns -----------------------

test_that("compare bind and full joins", {
  x2 <- mtcars[3:5, 1:3]
  y2 <- mtcars[30:32, 3:6]
  expect_equal(
    data_merge(x2, y2, join = "full"),
    data_merge(x2, y2, join = "bind"),
    ignore_attr = TRUE
  )
})

# join data frames in a list -----------------------

test_that("join data frames in a list", {
  skip_if_not_installed("poorman")

  x <- mtcars[1:5, 1:3]
  y <- mtcars[28:31, 3:5]
  z <- mtcars[11:18, c(1, 3:4, 6:8)]
  x$id <- 1:5
  y$id <- 4:7
  z$id <- 3:10

  dat <- data_merge(list(x, y, z), by = "id", id = "df", join = "bind")

  expect_equal(
    remove_empty(subset(poorman::filter(dat, df == 1), select = -df)),
    x,
    ignore_attr = TRUE
  )

  expect_equal(
    remove_empty(subset(poorman::filter(dat, df == 2), select = -c(df, id))),
    subset(y, select = -id),
    ignore_attr = TRUE
  )

  expect_equal(
    remove_empty(subset(poorman::filter(dat, df == 3), select = -c(df, id))),
    subset(z, select = -id),
    ignore_attr = TRUE
  )
})


# join empty data frames -----------------------

x <- data.frame("x" = character())
y <- data.frame("x" = character())
z <- data.frame("y" = character())

test_that("join empty data frames", {
  expect_equal(dim(data_merge(x, y, join = "left")), c(0, 1))
  expect_equal(dim(data_merge(x, y, join = "full")), c(0, 1))
  expect_equal(dim(data_merge(x, y, join = "right")), c(0, 1))
  expect_equal(dim(data_merge(x, y, join = "bind")), c(0, 1))
  expect_equal(dim(data_merge(x, z, join = "bind")), c(0, 2))
})
