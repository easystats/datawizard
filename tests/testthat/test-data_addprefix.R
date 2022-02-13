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
})
