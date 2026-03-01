# Compute mode for a statistical distribution

Compute mode for a statistical distribution

## Usage

``` r
distribution_mode(x)
```

## Arguments

- x:

  An atomic vector, a list, or a data frame.

## Value

The value that appears most frequently in the provided data. The
returned data structure will be the same as the entered one.

## See also

For continuous variables, the **Highest Maximum a Posteriori probability
estimate (MAP)** may be a more useful way to estimate the most
commonly-observed value than the mode. See
[`bayestestR::map_estimate()`](https://rdrr.io/pkg/bayestestR/man/map_estimate.html).

## Examples

``` r

distribution_mode(c(1, 2, 3, 3, 4, 5))
#> [1] 3
distribution_mode(c(1.5, 2.3, 3.7, 3.7, 4.0, 5))
#> [1] 3.7
```
