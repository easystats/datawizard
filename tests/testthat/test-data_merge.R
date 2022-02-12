if (require("testthat") && require("poorman")) {
  data(mtcars)
  x <- mtcars[3:5, 1:3]
  y <- mtcars[30:32, c(1, 4:5)]
  z <- mtcars[11:13, 6:8]

  x$id <- 1:3
  y$id <- 2:4
  z$id <- 3:5

  test_that("left-join", {
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

  # errors
  test_that("semi-anti-join", {
    expect_error(data_merge(x, y, join = "semi"))
    expect_error(data_merge(x, y, join = "anti"))
  })

  data_merge(x, y, join = "right")
  data_merge(x, y, join = "right", by = "id")
  data_merge(x, y, join = "right", by = "mpg")

  data_merge(x, y, join = "inner")
  data_merge(x, y, join = "inner", by = "id")
  data_merge(x, y, join = "inner", by = "mpg")

  data_merge(x, y, join = "full")
  data_merge(x, y, join = "full", by = "id")
  data_merge(x, y, join = "full", by = "mpg")
  data_merge(x, y, join = "full", by = c("id", "mpg"))

  data_merge(x, y, join = "bind")
  # by will be ignored
  data_merge(x, y, join = "bind", by = "id")
  data_merge(x, y, join = "bind", by = "mpg")
  data_merge(x, y, join = "bind", by = c("id", "mpg"))

  # x2 <- mtcars[3:5, 1:3]
  # y2 <- mtcars[30:32, 4:6]
  # data_merge(x2, y2, join = "full")
  # data_merge(x2, y2, join = "bind")
  #
  #
  # x2 <- mtcars[3:5, 1:3]
  # y2 <- mtcars[30:32, 3:6]
  # data_merge(x2, y2, join = "full")
  # data_merge(x2, y2, join = "bind")
}
