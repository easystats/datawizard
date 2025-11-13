# Quantify the smoothness of a vector

Quantify the smoothness of a vector

## Usage

``` r
smoothness(x, method = "cor", lag = 1, iterations = NULL, ...)
```

## Arguments

- x:

  Numeric vector (similar to a time series).

- method:

  Can be `"diff"` (the standard deviation of the standardized
  differences) or `"cor"` (default, lag-one autocorrelation).

- lag:

  An integer indicating which lag to use. If less than `1`, will be
  interpreted as expressed in percentage of the length of the vector.

- iterations:

  The number of bootstrap replicates for computing standard errors. If
  `NULL` (default), parametric standard errors are computed.

- ...:

  Arguments passed to or from other methods.

## Value

Value of smoothness.

## References

https://stats.stackexchange.com/questions/24607/how-to-measure-smoothness-of-a-time-series-in-r

## Examples

``` r
x <- (-10:10)^3 + rnorm(21, 0, 100)
plot(x)

smoothness(x, method = "cor")
#> [1] 0.9198875
#> attr(,"class")
#> [1] "parameters_smoothness" "numeric"              
smoothness(x, method = "diff")
#> [1] 1.563718
#> attr(,"class")
#> [1] "parameters_smoothness" "numeric"              
```
