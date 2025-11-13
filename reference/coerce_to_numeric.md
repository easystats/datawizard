# Convert to Numeric (if possible)

Tries to convert vector to numeric if possible (if no warnings or
errors). Otherwise, leaves it as is.

## Usage

``` r
coerce_to_numeric(x)
```

## Arguments

- x:

  A vector to be converted.

## Value

Numeric vector (if possible)

## Examples

``` r
coerce_to_numeric(c("1", "2"))
#> [1] 1 2
coerce_to_numeric(c("1", "2", "A"))
#> [1] "1" "2" "A"
```
