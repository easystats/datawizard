# Tools for working with column names

Tools for working with column names

## Usage

``` r
row_to_colnames(x, row = 1, na_prefix = "x", verbose = TRUE)

colnames_to_row(x, prefix = "x")
```

## Arguments

- x:

  A data frame.

- row:

  Row to use as column names.

- na_prefix:

  Prefix to give to the column name if the row has an `NA`. Default is
  'x', and it will be incremented at each `NA` (`x1`, `x2`, etc.).

- verbose:

  Toggle warnings.

- prefix:

  Prefix to give to the column name. Default is 'x', and it will be
  incremented at each column (`x1`, `x2`, etc.).

## Value

`row_to_colnames()` and `colnames_to_row()` both return a data frame.

## Examples

``` r
# Convert a row to column names --------------------------------
test <- data.frame(
  a = c("iso", 2, 5),
  b = c("year", 3, 6),
  c = c("value", 5, 7)
)
test
#>     a    b     c
#> 1 iso year value
#> 2   2    3     5
#> 3   5    6     7
row_to_colnames(test)
#>   iso year value
#> 2   2    3     5
#> 3   5    6     7

# Convert column names to row --------------------------------
test <- data.frame(
  ARG = c("BRA", "FRA"),
  `1960` = c(1960, 1960),
  `2000` = c(2000, 2000)
)
test
#>   ARG X1960 X2000
#> 1 BRA  1960  2000
#> 2 FRA  1960  2000
colnames_to_row(test)
#>    x1    x2    x3
#> 1 ARG X1960 X2000
#> 2 BRA  1960  2000
#> 3 FRA  1960  2000
```
