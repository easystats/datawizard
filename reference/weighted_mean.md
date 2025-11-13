# Weighted Mean, Median, SD, and MAD

Weighted Mean, Median, SD, and MAD

## Usage

``` r
weighted_mean(x, weights = NULL, remove_na = TRUE, verbose = TRUE, ...)

weighted_median(x, weights = NULL, remove_na = TRUE, verbose = TRUE, ...)

weighted_sd(x, weights = NULL, remove_na = TRUE, verbose = TRUE, ...)

weighted_mad(
  x,
  weights = NULL,
  constant = 1.4826,
  remove_na = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  an object containing the values whose weighted mean is to be computed.

- weights:

  A numerical vector of weights the same length as `x` giving the
  weights to use for elements of `x`. If `weights = NULL`, `x` is passed
  to the non-weighted function.

- remove_na:

  Logical, if `TRUE` (default), removes missing (`NA`) and infinite
  values from `x` and `weights`.

- verbose:

  Show warning when `weights` are negative?

- ...:

  arguments to be passed to or from methods.

- constant:

  scale factor.

## Examples

``` r
## GPA from Siegel 1994
x <- c(3.7, 3.3, 3.5, 2.8)
wt <- c(5, 5, 4, 1) / 15

weighted_mean(x, wt)
#> [1] 3.453333
weighted_median(x, wt)
#> [1] 3.5

weighted_sd(x, wt)
#> [1] 0.2852935
weighted_mad(x, wt)
#> [1] 0.29652
```
