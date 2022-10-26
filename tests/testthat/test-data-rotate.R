test_that("rotate data works as expected", {
  df <- mtcars[1:3, 1:4]

  expect_equal(
    data_rotate(df),
    structure(
      list(
        `Mazda RX4` = c(21, 6, 160, 110),
        `Mazda RX4 Wag` = c(21, 6, 160, 110),
        `Datsun 710` = c(22.8, 4, 108, 93)
      ),
      class = "data.frame",
      row.names = c("mpg", "cyl", "disp", "hp")
    )
  )

  expect_equal(
    data_rotate(df, rownames = "property"),
    structure(
      list(
        property = c("mpg", "cyl", "disp", "hp"),
        `Mazda RX4` = c(21, 6, 160, 110),
        `Mazda RX4 Wag` = c(21, 6, 160, 110),
        `Datsun 710` = c(22.8, 4, 108, 93)
      ),
      class = "data.frame",
      row.names = c(NA, 4L)
    )
  )

  expect_equal(
    data_rotate(df, colnames = TRUE),
    structure(
      list(
        `21` = c(6, 160, 110),
        `21` = c(6, 160, 110),
        `22.8` = c(4, 108, 93)
      ),
      class = "data.frame",
      row.names = c("cyl", "disp", "hp")
    )
  )

  expect_equal(
    data_rotate(df, rownames = "property", colnames = TRUE),
    structure(
      list(
        property = c("cyl", "disp", "hp"),
        `21` = c(6, 160, 110),
        `21` = c(6, 160, 110),
        `22.8` = c(4, 108, 93)
      ),
      class = "data.frame",
      row.names = c(NA, 3L)
    )
  )
})

test_that("data_rotate, arg 'colnames' works", {
  df <- mtcars[1:3, 1:4]
  df <- rownames_as_column(df)

  expected <- data.frame(
    `Mazda RX4` = c(21, 6, 160, 110),
    `Mazda RX4 Wag` = c(21, 6, 160, 110),
    `Datsun 710` = c(22.8, 4, 108, 93),
    check.names = FALSE
  )
  row.names(expected) <- c("mpg", "cyl", "disp", "hp")

  expect_identical(
    data_rotate(df, colnames = "rowname"),
    expected
  )
})

test_that("data_rotate warns if mixed types of data", {
  df <- mtcars[1:3, 1:4]
  df <- rownames_as_column(df)

  expect_warning(
    data_rotate(df),
    "mixed types of data"
  )
})
