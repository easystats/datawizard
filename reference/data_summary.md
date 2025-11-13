# Summarize data

This function can be used to compute summary statistics for a data frame
or a matrix.

## Usage

``` r
data_summary(x, ...)

# S3 method for class 'data.frame'
data_summary(x, ..., by = NULL, remove_na = FALSE)
```

## Arguments

- x:

  A (grouped) data frame.

- ...:

  One or more named expressions that define the new variable name and
  the function to compute the summary statistic. Example:
  `mean_sepal_width = mean(Sepal.Width)`. The expression can also be
  provided as a character string, e.g.
  `"mean_sepal_width = mean(Sepal.Width)"`. The summary function
  [`n()`](https://nathaneastwood.github.io/poorman/reference/context.html)
  can be used to count the number of observations.

- by:

  Optional character string, indicating the names of one or more
  variables in the data frame. If supplied, the data will be split by
  these variables and summary statistics will be computed for each
  group.

- remove_na:

  Logical. If `TRUE`, missing values are omitted from the grouping
  variable. If `FALSE` (default), missing values are included as a level
  in the grouping variable.

## Value

A data frame with the requested summary statistics.

## Examples

``` r
data(iris)
data_summary(iris, MW = mean(Sepal.Width), SD = sd(Sepal.Width))
#>   MW |   SD
#> -----------
#> 3.06 | 0.44
data_summary(
  iris,
  MW = mean(Sepal.Width),
  SD = sd(Sepal.Width),
  by = "Species"
)
#> Species    |   MW |   SD
#> ------------------------
#> setosa     | 3.43 | 0.38
#> versicolor | 2.77 | 0.31
#> virginica  | 2.97 | 0.32

# same as
d <- data_group(iris, "Species")
data_summary(d, MW = mean(Sepal.Width), SD = sd(Sepal.Width))
#> Species    |   MW |   SD
#> ------------------------
#> setosa     | 3.43 | 0.38
#> versicolor | 2.77 | 0.31
#> virginica  | 2.97 | 0.32

# multiple groups
data(mtcars)
data_summary(mtcars, MW = mean(mpg), SD = sd(mpg), by = c("am", "gear"))
#> am | gear |    MW |   SD
#> ------------------------
#>  0 |    3 | 16.11 | 3.37
#>  0 |    4 | 21.05 | 3.07
#>  1 |    4 | 26.27 | 5.41
#>  1 |    5 | 21.38 | 6.66

# expressions can also be supplied as character strings
data_summary(mtcars, "MW = mean(mpg)", "SD = sd(mpg)", by = c("am", "gear"))
#> am | gear |    MW |   SD
#> ------------------------
#>  0 |    3 | 16.11 | 3.37
#>  0 |    4 | 21.05 | 3.07
#>  1 |    4 | 26.27 | 5.41
#>  1 |    5 | 21.38 | 6.66

# count observations within groups
data_summary(mtcars, observations = n(), by = c("am", "gear"))
#> am | gear | observations
#> ------------------------
#>  0 |    3 |           15
#>  0 |    4 |            4
#>  1 |    4 |            8
#>  1 |    5 |            5

# first and last observations of "mpg" within groups
data_summary(
  mtcars,
  first = mpg[1],
  last = mpg[length(mpg)],
  by = c("am", "gear")
)
#> am | gear | first |  last
#> -------------------------
#>  0 |    3 | 21.40 | 19.20
#>  0 |    4 | 24.40 | 17.80
#>  1 |    4 | 21.00 | 21.40
#>  1 |    5 | 26.00 | 15.00
```
