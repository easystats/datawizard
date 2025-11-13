# Convert infinite or `NaN` values into `NA`

Replaces all infinite (`Inf` and `-Inf`) or `NaN` values with `NA`.

## Usage

``` r
replace_nan_inf(x, ...)
```

## Arguments

- x:

  A vector or a dataframe

- ...:

  Currently not used.

## Value

Data with `Inf`, `-Inf`, and `NaN` converted to `NA`.

## Examples

``` r
# a vector
x <- c(1, 2, NA, 3, NaN, 4, NA, 5, Inf, -Inf, 6, 7)
replace_nan_inf(x)
#>  [1]  1  2 NA  3 NA  4 NA  5 NA NA  6  7

# a data frame
df <- data.frame(
  x = c(1, NA, 5, Inf, 2, NA),
  y = c(3, NaN, 4, -Inf, 6, 7),
  stringsAsFactors = FALSE
)
replace_nan_inf(df)
#>    x  y
#> 1  1  3
#> 2 NA NA
#> 3  5  4
#> 4 NA NA
#> 5  2  6
#> 6 NA  7
```
