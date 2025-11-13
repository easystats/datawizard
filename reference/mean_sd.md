# Summary Helpers

Summary Helpers

## Usage

``` r
mean_sd(x, times = 1L, remove_na = TRUE, named = TRUE, ...)

median_mad(
  x,
  times = 1L,
  remove_na = TRUE,
  constant = 1.4826,
  named = TRUE,
  ...
)
```

## Arguments

- x:

  A numeric vector (or one that can be coerced to one via
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html)) to be
  summarized.

- times:

  How many SDs above and below the Mean (or MADs around the Median)

- remove_na:

  Logical. Should `NA` values be removed before computing (`TRUE`) or
  not (`FALSE`, default)?

- named:

  Should the vector be named? (E.g.,
  `c("-SD" = -1, Mean = 1, "+SD" = 2)`.)

- ...:

  Not used.

- constant:

  scale factor.

## Value

A (possibly named) numeric vector of length `2*times + 1` of SDs below
the mean, the mean, and SDs above the mean (or median and MAD).

## Examples

``` r
mean_sd(mtcars$mpg)
#>      -SD     Mean      +SD 
#> 14.06368 20.09062 26.11757 

mean_sd(mtcars$mpg, times = 2L)
#>     -2 SD     -1 SD      Mean     +1 SD     +2 SD 
#>  8.036729 14.063677 20.090625 26.117573 32.144521 

median_mad(mtcars$mpg)
#>     -MAD   Median     +MAD 
#> 13.78851 19.20000 24.61149 
```
