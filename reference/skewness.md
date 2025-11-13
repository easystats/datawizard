# Compute Skewness and (Excess) Kurtosis

Compute Skewness and (Excess) Kurtosis

## Usage

``` r
skewness(x, ...)

# S3 method for class 'numeric'
skewness(
  x,
  remove_na = TRUE,
  type = "2",
  iterations = NULL,
  verbose = TRUE,
  ...
)

kurtosis(x, ...)

# S3 method for class 'numeric'
kurtosis(
  x,
  remove_na = TRUE,
  type = "2",
  iterations = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'parameters_kurtosis'
print(x, digits = 3, test = FALSE, ...)

# S3 method for class 'parameters_skewness'
print(x, digits = 3, test = FALSE, ...)

# S3 method for class 'parameters_skewness'
summary(object, test = FALSE, ...)

# S3 method for class 'parameters_kurtosis'
summary(object, test = FALSE, ...)
```

## Arguments

- x:

  A numeric vector or data.frame.

- ...:

  Arguments passed to or from other methods.

- remove_na:

  Logical. Should `NA` values be removed before computing (`TRUE`) or
  not (`FALSE`, default)?

- type:

  Type of algorithm for computing skewness. May be one of `1` (or `"1"`,
  `"I"` or `"classic"`), `2` (or `"2"`, `"II"` or `"SPSS"` or `"SAS"`)
  or `3` (or `"3"`, `"III"` or `"Minitab"`). See 'Details'.

- iterations:

  The number of bootstrap replicates for computing standard errors. If
  `NULL` (default), parametric standard errors are computed.

- verbose:

  Toggle warnings and messages.

- digits:

  Number of decimal places.

- test:

  Logical, if `TRUE`, tests if skewness or kurtosis is significantly
  different from zero.

- object:

  An object returned by `skewness()` or `kurtosis()`.

## Value

Values of skewness or kurtosis.

## Details

### Skewness

Symmetric distributions have a `skewness` around zero, while a negative
skewness values indicates a "left-skewed" distribution, and a positive
skewness values indicates a "right-skewed" distribution. Examples for
the relationship of skewness and distributions are:

- Normal distribution (and other symmetric distribution) has a skewness
  of 0

- Half-normal distribution has a skewness just below 1

- Exponential distribution has a skewness of 2

- Lognormal distribution can have a skewness of any positive value,
  depending on its parameters

(https://en.wikipedia.org/wiki/Skewness)

### Types of Skewness

`skewness()` supports three different methods for estimating skewness,
as discussed in Joanes and Gill (1988):

- Type "1" is the "classical" method, which is
  `g1 = (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5`

- Type "2" first calculates the type-1 skewness, then adjusts the
  result: `G1 = g1 * sqrt(n * (n - 1)) / (n - 2)`. This is what SAS and
  SPSS usually return.

- Type "3" first calculates the type-1 skewness, then adjusts the
  result: `b1 = g1 * ((1 - 1 / n))^1.5`. This is what Minitab usually
  returns.

### Kurtosis

The `kurtosis` is a measure of "tailedness" of a distribution. A
distribution with a kurtosis values of about zero is called
"mesokurtic". A kurtosis value larger than zero indicates a
"leptokurtic" distribution with *fatter* tails. A kurtosis value below
zero indicates a "platykurtic" distribution with *thinner* tails
(https://en.wikipedia.org/wiki/Kurtosis).

### Types of Kurtosis

`kurtosis()` supports three different methods for estimating kurtosis,
as discussed in Joanes and Gill (1988):

- Type "1" is the "classical" method, which is
  `g2 = n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2) - 3`.

- Type "2" first calculates the type-1 kurtosis, then adjusts the
  result: `G2 = ((n + 1) * g2 + 6) * (n - 1)/((n - 2) * (n - 3))`. This
  is what SAS and SPSS usually return

- Type "3" first calculates the type-1 kurtosis, then adjusts the
  result: `b2 = (g2 + 3) * (1 - 1 / n)^2 - 3`. This is what Minitab
  usually returns.

### Standard Errors

It is recommended to compute empirical (bootstrapped) standard errors
(via the `iterations` argument) than relying on analytic standard errors
(Wright & Herrington, 2011).

## References

- D. N. Joanes and C. A. Gill (1998). Comparing measures of sample
  skewness and kurtosis. The Statistician, 47, 183–189.

- Wright, D. B., & Herrington, J. A. (2011). Problematic standard errors
  and confidence intervals for skewness and kurtosis. Behavior research
  methods, 43(1), 8-17.

## Examples

``` r
skewness(rnorm(1000))
#> Skewness |    SE
#> ----------------
#>    0.093 | 0.077
kurtosis(rnorm(1000))
#> Kurtosis |    SE
#> ----------------
#>   -0.112 | 0.154
```
