# Summarize data

This function can be used to compute summary statistics for a data frame
or a matrix.

## Usage

``` r
data_summary(x, ...)

# S3 method for class 'data.frame'
data_summary(x, ..., by = NULL, remove_na = FALSE, suffix = NULL)
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

- suffix:

  Optional, suffixes to be added to the new variable names, especially
  useful when a function returns several values (e.g.
  [`quantile()`](https://rdrr.io/r/stats/quantile.html)). Can be:

  - a character vector: all expressions in `...` must return the same
    number of values as elements in `suffix`.

  - a list of named character vectors: the names of elements in `suffix`
    must match the names of the expressions. It is also allowed to
    specify suffixes for selected expressions only.

  The new column names are a combination of the left-hand side (i.e.,
  the name) of the expression and the related suffixes. If
  `suffix = NULL` (the default), and a summary expression returns
  multiple values, either the names of the returned values (if any) or
  automatically numbered suffixes such as `_1`, `_2`, etc. are used. See
  'Examples'.

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

# allow more than one-column-summaries for expressions
d <- data.frame(
  x = rnorm(100, 1, 1),
  y = rnorm(100, 2, 2),
  groups = rep(1:4, each = 25)
)

# since we have multiple columns for one expression, the names of the
# returned summary results are used as suffix by default
data_summary(
  d,
  quant_x = quantile(x, c(0.25, 0.75)),
  mean_x = mean(x),
  quant_y = quantile(y, c(0.25, 0.5, 0.75))
)
#> quant_x25% | quant_x75% | mean_x | quant_y25% | quant_y50% | quant_y75%
#> -----------------------------------------------------------------------
#>       0.50 |       1.74 |   1.07 |       0.76 |       1.87 |       3.17

# if a summary function, like `fivenum()`, returns no named vector, suffixes
# are automatically numbered
data_summary(
  d,
  quant_x = quantile(x, c(0.25, 0.75)),
  mean_x = mean(x),
  fivenum_y = fivenum(y)
)
#> quant_x25% | quant_x75% | mean_x | fivenum_y_1 | fivenum_y_2 | fivenum_y_3
#> --------------------------------------------------------------------------
#>       0.50 |       1.74 |   1.07 |       -2.14 |        0.75 |        1.87
#> 
#> quant_x25% | fivenum_y_4 | fivenum_y_5
#> --------------------------------------
#>       0.50 |        3.17 |        6.86

# specify column suffix for expressions, matching by names
data_summary(
  d,
  quant_x = quantile(x, c(0.25, 0.75)),
  mean_x = mean(x),
  quant_y = quantile(y, c(0.25, 0.5, 0.75)),
  suffix = list(quant_y = c("_Q1", "_Q2", "_Q3"))
)
#> quant_x25% | quant_x75% | mean_x | quant_y_Q1 | quant_y_Q2 | quant_y_Q3
#> -----------------------------------------------------------------------
#>       0.50 |       1.74 |   1.07 |       0.76 |       1.87 |       3.17

# name multiple expression suffixes, grouped by variable
data_summary(
  d,
  quant_x = quantile(x, c(0.25, 0.75)),
  mean_x = mean(x),
  quant_y = quantile(y, c(0.25, 0.5, 0.75)),
  suffix = list(quant_x = c("Q1", "Q3"), quant_y = c("_Q1", "_Q2", "_Q3")),
  by = "groups"
)
#> groups | quant_xQ1 | quant_xQ3 | mean_x | quant_y_Q1 | quant_y_Q2 | quant_y_Q3
#> ------------------------------------------------------------------------------
#>      1 |      0.35 |      1.49 |   0.82 |       0.25 |       1.84 |       2.73
#>      2 |      0.44 |      1.54 |   1.01 |       0.88 |       2.26 |       3.21
#>      3 |      0.61 |      1.87 |   1.05 |       0.74 |       1.61 |       3.69
#>      4 |      0.77 |      2.13 |   1.42 |       0.44 |       2.05 |       3.00
```
