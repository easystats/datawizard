# Compute mode for a statistical distribution

Compute mode for a statistical distribution

## Usage

``` r
distribution_mode(x, verbose = TRUE)
```

## Arguments

- x:

  An atomic vector, a list, or a data frame.

- verbose:

  Logical. Whether to show a message if there is a tie for the mode
  value. Defaults to `TRUE`. Setting to `FALSE` skips the tie check. In
  both cases, only the first mode is returned. Possible multiple mode
  values are saved as attribute `tied_values`.

## Value

The value that appears most frequently in the provided data. The
returned data structure will be the same as the entered one.

## See also

For continuous variables, the **Highest Maximum a Posteriori probability
estimate (MAP)** may be a more useful way to estimate the most
commonly-observed value than the mode. See
[`bayestestR::map_estimate()`](https://easystats.github.io/bayestestR/reference/map_estimate.html).

## Examples

``` r
distribution_mode(c(1, 2, 3, 3, 4, 5))
#> [1] 3
distribution_mode(c(1.5, 2.3, 3.7, 3.7, 4.0, 5))
#> [1] 3.7

# message for tied frequencies
data(iris)
distribution_mode(iris$Species)
#> Multiple modes detected with equal frequency. Returning the smallest
#>   value.
#> [1] setosa
#> attr(,"tied_values")
#> [1] setosa     versicolor virginica 
#> Levels: setosa versicolor virginica
#> Levels: setosa versicolor virginica
```
