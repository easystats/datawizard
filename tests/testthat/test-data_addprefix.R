test_that("data_addprefix works as expected", {
  expect_equal(
    names(head(data_addprefix(iris, "NEW_"))),
    c(
      "NEW_Sepal.Length", "NEW_Sepal.Width", "NEW_Petal.Length",
      "NEW_Petal.Width", "NEW_Species"
    )
  )

  expect_equal(
    names(head(data_addsuffix(iris, "_OLD"))),
    c(
      "Sepal.Length_OLD", "Sepal.Width_OLD", "Petal.Length_OLD",
      "Petal.Width_OLD", "Species_OLD"
    )
  )

  expect_equal(
    names(head(data_addprefix(iris, "NEW_", select = starts_with("Sepal")))),
    c("NEW_Sepal.Length", "NEW_Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  expect_equal(
    names(head(data_addsuffix(iris, "_OLD", select = starts_with("Petal")))),
    c("Sepal.Length", "Sepal.Width", "Petal.Length_OLD", "Petal.Width_OLD", "Species")
  )
})

# select helpers ------------------------------
test_that("data_addprefix regex", {
  expect_equal(
    data_addsuffix(mtcars, "_regex", select = "pg", regex = TRUE),
    data_addsuffix(mtcars, "_regex", select = "mpg")
  )
  expect_equal(
    data_addsuffix(mtcars, select = "pg$", "_regex", regex = TRUE),
    data_addsuffix(mtcars, select = "mpg", "_regex")
  )
})
