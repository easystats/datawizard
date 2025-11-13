# Compute the coefficient of variation

Compute the coefficient of variation (CV, ratio of the standard
deviation to the mean, \\\sigma/\mu\\) for a set of numeric values.

## Usage

``` r
coef_var(x, ...)

distribution_coef_var(x, ...)

# S3 method for class 'numeric'
coef_var(
  x,
  mu = NULL,
  sigma = NULL,
  method = c("standard", "unbiased", "median_mad", "qcd"),
  trim = 0,
  remove_na = FALSE,
  n = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector of ratio scale (see details), or vector of values
  than can be coerced to one.

- ...:

  Further arguments passed to computation functions.

- mu:

  A numeric vector of mean values to use to compute the coefficient of
  variation. If supplied, `x` is not used to compute the mean.

- sigma:

  A numeric vector of standard deviation values to use to compute the
  coefficient of variation. If supplied, `x` is not used to compute the
  SD.

- method:

  Method to use to compute the CV. Can be `"standard"` to compute by
  dividing the standard deviation by the mean, `"unbiased"` for the
  unbiased estimator for normally distributed data, or one of two robust
  alternatives: `"median_mad"` to divide the median by the
  [`stats::mad()`](https://rdrr.io/r/stats/mad.html), or `"qcd"`
  (quartile coefficient of dispersion, interquartile range divided by
  the sum of the quartiles \[twice the midhinge\]: \\(Q_3 - Q_1)/(Q_3 +
  Q_1)\\.

- trim:

  the fraction (0 to 0.5) of values to be trimmed from each end of `x`
  before the mean and standard deviation (or other measures) are
  computed. Values of `trim` outside the range of (0 to 0.5) are taken
  as the nearest endpoint.

- remove_na:

  Logical. Should `NA` values be removed before computing (`TRUE`) or
  not (`FALSE`, default)?

- n:

  If `method = "unbiased"` and both `mu` and `sigma` are provided (not
  computed from `x`), what sample size to use to adjust the computed CV
  for small-sample bias?

## Value

The computed coefficient of variation for `x`.

## Details

CV is only applicable of values taken on a ratio scale: values that have
a *fixed* meaningfully defined 0 (which is either the lowest or highest
possible value), and that ratios between them are interpretable For
example, how many sandwiches have I eaten this week? 0 means "none" and
20 sandwiches is 4 times more than 5 sandwiches. If I were to center the
number of sandwiches, it will no longer be on a ratio scale (0 is no
"none" it is the mean, and the ratio between 4 and -2 is not
meaningful). Scaling a ratio scale still results in a ratio scale. So I
can re define "how many half sandwiches did I eat this week ( =
sandwiches \* 0.5) and 0 would still mean "none", and 20 half-sandwiches
is still 4 times more than 5 half-sandwiches.

This means that CV is **NOT** invariant to shifting, but it is to
scaling:

    sandwiches <- c(0, 4, 15, 0, 0, 5, 2, 7)
    coef_var(sandwiches)
    #> [1] 1.239094

    coef_var(sandwiches / 2) # same
    #> [1] 1.239094

    coef_var(sandwiches + 4) # different! 0 is no longer meaningful!
    #> [1] 0.6290784

## Examples

``` r
coef_var(1:10)
#> [1] 0.5504819
coef_var(c(1:10, 100), method = "median_mad")
#> [1] 0.7413
coef_var(c(1:10, 100), method = "qcd")
#> [1] 0.4166667
coef_var(mu = 10, sigma = 20)
#> [1] 2
coef_var(mu = 10, sigma = 20, method = "unbiased", n = 30)
#> [1] 2.250614
```
