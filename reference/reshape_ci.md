# Reshape CI between wide/long formats

Reshape CI between wide/long formats.

## Usage

``` r
reshape_ci(x, ci_type = "CI")
```

## Arguments

- x:

  A data frame containing columns named `CI_low` and `CI_high` (or
  similar, see `ci_type`).

- ci_type:

  String indicating the "type" (i.e. prefix) of the interval columns.
  Per *easystats* convention, confidence or credible intervals are named
  `CI_low` and `CI_high`, and the related `ci_type` would be `"CI"`. If
  column names for other intervals differ, `ci_type` can be used to
  indicate the name, e.g. `ci_type = "SI"` can be used for support
  intervals, where the column names in the data frame would be `SI_low`
  and `SI_high`.

## Value

A data frame with columns corresponding to confidence intervals reshaped
either to wide or long format.

## Examples

``` r
x <- data.frame(
  Parameter = c("Term 1", "Term 2", "Term 1", "Term 2"),
  CI = c(.8, .8, .9, .9),
  CI_low = c(.2, .3, .1, .15),
  CI_high = c(.5, .6, .8, .85),
  stringsAsFactors = FALSE
)

reshape_ci(x)
#>   Parameter CI_low_0.8 CI_high_0.8 CI_low_0.9 CI_high_0.9
#> 1    Term 1        0.2         0.5       0.10        0.80
#> 2    Term 2        0.3         0.6       0.15        0.85
reshape_ci(reshape_ci(x))
#>   Parameter  CI CI_low CI_high
#> 1    Term 1 0.8   0.20    0.50
#> 2    Term 1 0.9   0.10    0.80
#> 3    Term 2 0.8   0.30    0.60
#> 4    Term 2 0.9   0.15    0.85
```
