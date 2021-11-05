test_that("extract from data frame", {
  expect_equal(
    extract(mtcars, cyl, name = gear),
    setNames(mtcars$cyl, mtcars$gear)
  )
  expect_equal(
    extract(mtcars, -1, name = gear),
    setNames(mtcars[[11]], mtcars$gear)
  )
  expect_equal(
    extract(mtcars, cyl, name = 0),
    setNames(mtcars$cyl, rownames(mtcars))
  )
  expect_equal(
    extract(mtcars, cyl, name = "row.names"),
    setNames(mtcars$cyl, rownames(mtcars))
  )
})
