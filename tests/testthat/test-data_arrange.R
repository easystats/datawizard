library(poorman)
df <- head(mtcars)
df$character <- c("a", "b", "b", "c", "c", "a")

test_that("data_arrange works with one numeric column", {
  expect_equal(
    arrange(df, carb),
    data_arrange(df, "carb")
  )
  expect_equal(
    arrange(df, -carb),
    data_arrange(df, "-carb")
  )
})

test_that("data_arrange works with one character column", {
  expect_equal(
    arrange(df, character),
    data_arrange(df, "character")
  )
  expect_equal(
    arrange(df, desc(character)),
    data_arrange(df, "-character")
  )
})

test_that("data_arrange works with several columns", {
  expect_equal(
    arrange(df, carb, gear),
    data_arrange(df, c("carb", "gear"))
  )
  expect_equal(
    arrange(df, -carb, gear),
    data_arrange(df, c("-carb", "gear"))
  )
  expect_equal(
    arrange(df, -carb, desc(character)),
    data_arrange(df, c("-carb", "-character"))
  )
})

test_that("data_arrange works without columns", {
  expect_equal(data_arrange(df), df)
})

test_that("data_arrange ignores wrong names if safe = TRUE", {
  expect_warning(
    expect_equal(data_arrange(df, "foo"), df),
    regexp = "don't exist"
  )

  expect_warning(
    expect_equal(
      data_arrange(df, c("gear", "foo")),
      data_arrange(df, "gear")
    ),
    regexp = "don't exist"
  )
})

test_that("data_arrange errors if safe = FALSE", {
  expect_error(data_arrange(df, "foo", safe = FALSE))
})

test_that("data_arrange errors if not coercable to data frame", {
  expect_error(data_arrange(list(a = 1:5, b = letters[1:3]), select = "b"))
  expect_equal(
    data_arrange(list(a = 1:5, b = letters[5:1]), select = "b"),
    structure(list(a = 5:1, b = c("a", "b", "c", "d", "e")), row.names = 5:1, class = "data.frame"),
    ignore_attr = TRUE
  )
})

test_that("data_arrange works with grouped df", {
  set.seed(123)
  x <- mtcars[sample(1:nrow(mtcars), 10, replace = TRUE), c("cyl", "mpg")]
  g <- data_group(x, cyl)

  expected <- data.frame(
    cyl = c(4, 4, 4, 6, 6, 8, 8, 8, 8, 8),
    mpg = c(22.8, 30.4, 32.4, 17.8, 19.2, 10.4, 15, 15.2, 15.5, 18.7)
  )
  class(expected) <- c("grouped_df", "data.frame")
  rownames(expected) <- c(
    "Datsun 710", "Honda Civic", "Fiat 128", "Merc 280C", "Merc 280",
    "Cadillac Fleetwood", "Maserati Bora", "Merc 450SLC", "Dodge Challenger",
    "Hornet Sportabout"
  )
  attributes(expected)$groups <- attributes(g)$groups

  expect_equal(
    data_arrange(g, "mpg"),
    expected
  )
})

test_that("data_arrange works with NA", {
  # without groups

  tmp <- data.frame(
    a = c(1, 2, 2, 8, 1, 3),
    b = c(1, NA, 3, 3, NA, 5)
  )

  expect_equal(
    data_arrange(tmp, "a"),
    data.frame(
      a = c(1, 1, 2, 2, 3, 8),
      b = c(1, NA, NA, 3, 5, 3)
    )
  )

  # with groups

  g <- data_group(tmp, "b")

  expected <- data.frame(
    a = c(1, 2, 8, 3, 1, 2),
    b = c(1, 3, 3, 5, NA, NA)
  )
  class(expected) <- c("grouped_df", "data.frame")
  attributes(expected)$groups <- attributes(g)$groups

  expect_equal(
    data_arrange(g, "a"),
    expected
  )
})
