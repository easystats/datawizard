d_unite <- data.frame(
  x = c(NA, 1:3),
  y = c(letters[1:3], NA_character_),
  z = 6:9,
  m = c("X", NA_character_, "Y", "Z"),
  n = c("NATION", "COUNTRY", "NATION", NA_character_),
  stringsAsFactors = FALSE
)


# for following tests, we need to check for correct column names,
# and correct values in new variable

test_that("data_unite: simple use case", {
  # basic
  out <- data_unite(d_unite, new_column = "xyz")
  expect_identical(colnames(out), "xyz")
  expect_identical(
    out$xyz,
    c("NA_a_6_X_NATION", "1_b_7_NA_COUNTRY", "2_c_8_Y_NATION", "3_NA_9_Z_NA")
  )
  # use existing column name
  out <- data_unite(d_unite, new_column = "x")
  expect_identical(colnames(out), "x")
  expect_identical(
    out$x,
    c("NA_a_6_X_NATION", "1_b_7_NA_COUNTRY", "2_c_8_Y_NATION", "3_NA_9_Z_NA")
  )
  # select
  out <- data_unite(d_unite, new_column = "xyz", select = c("x", "n"))
  expect_identical(colnames(out), c(setdiff(colnames(d_unite), c("x", "n")), "xyz"))
  expect_identical(
    out$xyz,
    c("NA_NATION", "1_COUNTRY", "2_NATION", "3_NA")
  )
  # select, use existing column name
  out <- data_unite(d_unite, new_column = "x", select = c("x", "n"))
  expect_identical(colnames(out), c(setdiff(colnames(d_unite), c("x", "n")), "x"))
  expect_identical(
    out$x,
    c("NA_NATION", "1_COUNTRY", "2_NATION", "3_NA")
  )
})


test_that("data_unite: remove_na", {
  # basic
  out <- data_unite(d_unite, new_column = "xyz", remove_na = TRUE)
  expect_identical(colnames(out), "xyz")
  expect_identical(
    out$xyz,
    c("a_6_X_NATION", "1_b_7_COUNTRY", "2_c_8_Y_NATION", "3_9_Z")
  )
  # use existing column name
  out <- data_unite(d_unite, new_column = "x", remove_na = TRUE)
  expect_identical(colnames(out), "x")
  expect_identical(
    out$x,
    c("a_6_X_NATION", "1_b_7_COUNTRY", "2_c_8_Y_NATION", "3_9_Z")
  )
  # select
  out <- data_unite(d_unite, new_column = "xyz", remove_na = TRUE, select = c("x", "n"))
  expect_identical(colnames(out), c(setdiff(colnames(d_unite), c("x", "n")), "xyz"))
  expect_identical(
    out$xyz,
    c("NATION", "1_COUNTRY", "2_NATION", "3")
  )
  # select, use existing column name
  out <- data_unite(d_unite, new_column = "x", remove_na = TRUE, select = c("x", "n"))
  expect_identical(colnames(out), c(setdiff(colnames(d_unite), c("x", "n")), "x"))
  expect_identical(
    out$x,
    c("NATION", "1_COUNTRY", "2_NATION", "3")
  )
})


test_that("data_unite: append", {
  # basic
  out <- data_unite(d_unite, new_column = "xyz", append = TRUE)
  expect_identical(colnames(out), c("x", "y", "z", "m", "n", "xyz"))
  expect_identical(
    out$xyz,
    c("NA_a_6_X_NATION", "1_b_7_NA_COUNTRY", "2_c_8_Y_NATION", "3_NA_9_Z_NA")
  )
  # remove NA
  out <- data_unite(d_unite, new_column = "xyz", remove_na = TRUE, append = TRUE)
  expect_identical(colnames(out), c("x", "y", "z", "m", "n", "xyz"))
  expect_identical(
    out$xyz,
    c("a_6_X_NATION", "1_b_7_COUNTRY", "2_c_8_Y_NATION", "3_9_Z")
  )
  # append, using existing column name
  expect_message({
    out <- data_unite(d_unite, new_column = "x", append = TRUE)
  })
  expect_identical(colnames(out), c("x", "y", "z", "m", "n"))
  expect_identical(
    out$x,
    c("NA_a_6_X_NATION", "1_b_7_NA_COUNTRY", "2_c_8_Y_NATION", "3_NA_9_Z_NA")
  )
  # append, using existing column name, and remove NA
  expect_message({
    out <- data_unite(d_unite, new_column = "x", remove_na = TRUE, append = TRUE)
  })
  expect_identical(colnames(out), c("x", "y", "z", "m", "n"))
  expect_identical(
    out$x,
    c("a_6_X_NATION", "1_b_7_COUNTRY", "2_c_8_Y_NATION", "3_9_Z")
  )
})


test_that("data_unite: combine select and append", {
  # basic
  out <- data_unite(d_unite, new_column = "xyz", append = TRUE, select = c("x", "n"))
  expect_identical(colnames(out), c("x", "y", "z", "m", "n", "xyz"))
  expect_identical(
    out$xyz,
    c("NA_NATION", "1_COUNTRY", "2_NATION", "3_NA")
  )
  # remove NA
  out <- data_unite(d_unite, new_column = "xyz", remove_na = TRUE, append = TRUE, select = c("x", "n"))
  expect_identical(colnames(out), c("x", "y", "z", "m", "n", "xyz"))
  expect_identical(
    out$xyz,
    c("NATION", "1_COUNTRY", "2_NATION", "3")
  )
  # append, using existing column name
  expect_message({
    out <- data_unite(d_unite, new_column = "x", append = TRUE, select = c("x", "n"))
  })
  expect_identical(colnames(out), c("x", "y", "z", "m", "n"))
  expect_identical(
    out$x,
    c("NA_NATION", "1_COUNTRY", "2_NATION", "3_NA")
  )
  # append, using existing column name, and remove NA
  expect_message({
    out <- data_unite(d_unite, new_column = "x", remove_na = TRUE, append = TRUE, select = c("x", "n"))
  })
  expect_identical(colnames(out), c("x", "y", "z", "m", "n"))
  expect_identical(
    out$x,
    c("NATION", "1_COUNTRY", "2_NATION", "3")
  )
})


test_that("data_unite: errors", {
  expect_error(data_unite(d_unite), regex = "No name")
  expect_error(data_unite(d_unite, new_column = c("a", "b")), regex = "a single string")
  expect_error(expect_warning(data_unite(d_unite, new_column = "a", select = "huhu")), regex = "At least")
})
