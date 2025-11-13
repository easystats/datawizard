# Rescale Variables to a New Range

Rescale variables to a new range. Can also be used to reverse-score
variables (change the keying/scoring direction), or to expand a range.

## Usage

``` r
rescale(x, ...)

change_scale(x, ...)

# S3 method for class 'numeric'
rescale(
  x,
  to = c(0, 100),
  multiply = NULL,
  add = NULL,
  range = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
rescale(
  x,
  select = NULL,
  exclude = NULL,
  to = c(0, 100),
  multiply = NULL,
  add = NULL,
  range = NULL,
  append = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = FALSE,
  ...
)
```

## Arguments

- x:

  A (grouped) data frame, numeric vector or factor.

- ...:

  Arguments passed to or from other methods.

- to:

  Numeric vector of length 2 giving the new range that the variable will
  have after rescaling. To reverse-score a variable, the range should be
  given with the maximum value first. See examples.

- multiply:

  If not `NULL`, `to` is ignored and `multiply` will be used, giving the
  factor by which the actual range of `x` should be expanded. For
  example, if a vector ranges from 5 to 15 and `multiply = 1.1`, the
  current range of 10 will be expanded by the factor of 1.1, giving a
  new range of 11. Thus, the rescaled vector would range from 4.5 to
  15.5.

- add:

  A vector of length 1 or 2. If not `NULL`, `to` is ignored and `add`
  will be used, giving the amount by which the minimum and maximum of
  the actual range of `x` should be expanded. For example, if a vector
  ranges from 5 to 15 and `add = 1`, the range will be expanded from 4
  to 16. If `add` is of length 2, then the first value is used for the
  lower bound and the second value for the upper bound.

- range:

  Initial (old) range of values. If `NULL`, will take the range of the
  input vector (`range(x)`).

- verbose:

  Toggle warnings.

- select:

  Variables that will be included when performing the required tasks.
  Can be either

  - a variable specified as a literal variable name (e.g.,
    `column_name`),

  - a string with the variable name (e.g., `"column_name"`), a character
    vector of variable names (e.g., `c("col1", "col2", "col3")`), or a
    character vector of variable names including ranges specified via
    `:` (e.g., `c("col1:col3", "col5")`),

  - for some functions, like
    [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
    or
    [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md),
    `select` can be a named character vector. In this case, the names
    are used to rename the columns in the output data frame. See
    'Details' in the related functions to see where this option applies.

  - a formula with variable names (e.g., `~column_1 + column_2`),

  - a vector of positive integers, giving the positions counting from
    the left (e.g. `1` or `c(1, 3, 5)`),

  - a vector of negative integers, giving the positions counting from
    the right (e.g., `-1` or `-1:-3`),

  - one of the following select-helpers:
    [`starts_with()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
    [`ends_with()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
    [`contains()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
    a range using `:`, or `regex()`.
    [`starts_with()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
    [`ends_with()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
    and
    [`contains()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html)
    accept several patterns, e.g `starts_with("Sep", "Petal")`.
    `regex()` can be used to define regular expression patterns.

  - a function testing for logical conditions, e.g.
    [`is.numeric()`](https://rdrr.io/r/base/numeric.html) (or
    `is.numeric`), or any user-defined function that selects the
    variables for which the function returns `TRUE` (like:
    `foo <- function(x) mean(x) > 3`),

  - ranges specified via literal variable names, select-helpers (except
    `regex()`) and (user-defined) functions can be negated, i.e. return
    non-matching elements, when prefixed with a `-`, e.g.
    `-ends_with()`, `-is.numeric` or `-(Sepal.Width:Petal.Length)`.
    **Note:** Negation means that matches are *excluded*, and thus, the
    `exclude` argument can be used alternatively. For instance,
    `select=-ends_with("Length")` (with `-`) is equivalent to
    `exclude=ends_with("Length")` (no `-`). In case negation should not
    work as expected, use the `exclude` argument instead.

  If `NULL`, selects all columns. Patterns that found no matches are
  silently ignored, e.g.
  `extract_column_names(iris, select = c("Species", "Test"))` will just
  return `"Species"`.

- exclude:

  See `select`, however, column names matched by the pattern from
  `exclude` will be excluded instead of selected. If `NULL` (the
  default), excludes no columns.

- append:

  Logical or string. If `TRUE`, recoded or converted variables get new
  column names and are appended (column bind) to `x`, thus returning
  both the original and the recoded variables. The new columns get a
  suffix, based on the calling function: `"_r"` for recode functions,
  `"_n"` for
  [`to_numeric()`](https://easystats.github.io/datawizard/reference/to_numeric.md),
  `"_f"` for
  [`to_factor()`](https://easystats.github.io/datawizard/reference/to_factor.md),
  or `"_s"` for
  [`slide()`](https://easystats.github.io/datawizard/reference/slide.md).
  If `append=FALSE`, original variables in `x` will be overwritten by
  their recoded versions. If a character value, recoded variables are
  appended with new column names (using the defined suffix) to the
  original data frame.

- ignore_case:

  Logical, if `TRUE` and when one of the select-helpers or a regular
  expression is used in `select`, ignores lower/upper case in the search
  pattern when matching against variable names.

- regex:

  Logical, if `TRUE`, the search pattern from `select` will be treated
  as regular expression. When `regex = TRUE`, select *must* be a
  character string (or a variable containing a character string) and is
  not allowed to be one of the supported select-helpers or a character
  vector of length \> 1. `regex = TRUE` is comparable to using one of
  the two select-helpers, `select = contains()` or `select = regex()`,
  however, since the select-helpers may not work when called from inside
  other functions (see 'Details'), this argument may be used as
  workaround.

## Value

A rescaled object.

## Selection of variables - the `select` argument

For most functions that have a `select` argument (including this
function), the complete input data frame is returned, even when `select`
only selects a range of variables. That is, the function is only applied
to those variables that have a match in `select`, while all other
variables remain unchanged. In other words: for this function, `select`
will not omit any non-included variables, so that the returned data
frame will include all variables from the input data frame.

## See also

See
[`makepredictcall.dw_transformer()`](https://easystats.github.io/datawizard/reference/makepredictcall.dw_transformer.md)
for use in model formulas.

Other transform utilities:
[`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md),
[`ranktransform()`](https://easystats.github.io/datawizard/reference/ranktransform.md),
[`reverse()`](https://easystats.github.io/datawizard/reference/reverse.md),
[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)

## Examples

``` r
rescale(c(0, 1, 5, -5, -2))
#> [1]  50  60 100   0  30
#> (original range = -5 to 5)
#> 
rescale(c(0, 1, 5, -5, -2), to = c(-5, 5))
#> [1]  0  1  5 -5 -2
#> (original range = -5 to 5)
#> 
rescale(c(1, 2, 3, 4, 5), to = c(-2, 2))
#> [1] -2 -1  0  1  2
#> (original range = 1 to 5)
#> 

# Specify the "theoretical" range of the input vector
rescale(c(1, 3, 4), to = c(0, 40), range = c(0, 4))
#> [1] 10 30 40
#> (original range = 0 to 4)
#> 

# Reverse-score a variable
rescale(c(1, 2, 3, 4, 5), to = c(5, 1))
#> [1] 5 4 3 2 1
#> (original range = 1 to 5)
#> 
rescale(c(1, 2, 3, 4, 5), to = c(2, -2))
#> [1]  2  1  0 -1 -2
#> (original range = 1 to 5)
#> 

# Data frames
head(rescale(iris, to = c(0, 1)))
#> Variables of class `factor` can't be rescaled and remain unchanged.
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1   0.22222222   0.6250000   0.06779661  0.04166667  setosa
#> 2   0.16666667   0.4166667   0.06779661  0.04166667  setosa
#> 3   0.11111111   0.5000000   0.05084746  0.04166667  setosa
#> 4   0.08333333   0.4583333   0.08474576  0.04166667  setosa
#> 5   0.19444444   0.6666667   0.06779661  0.04166667  setosa
#> 6   0.30555556   0.7916667   0.11864407  0.12500000  setosa
head(rescale(iris, to = c(0, 1), select = "Sepal.Length"))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1   0.22222222         3.5          1.4         0.2  setosa
#> 2   0.16666667         3.0          1.4         0.2  setosa
#> 3   0.11111111         3.2          1.3         0.2  setosa
#> 4   0.08333333         3.1          1.5         0.2  setosa
#> 5   0.19444444         3.6          1.4         0.2  setosa
#> 6   0.30555556         3.9          1.7         0.4  setosa

# One can specify a list of ranges
head(rescale(iris, to = list(
  "Sepal.Length" = c(0, 1),
  "Petal.Length" = c(-1, 0)
)))
#> Variables of class `factor` can't be rescaled and remain unchanged.
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1   0.22222222         3.5   -0.9322034         0.2  setosa
#> 2   0.16666667         3.0   -0.9322034         0.2  setosa
#> 3   0.11111111         3.2   -0.9491525         0.2  setosa
#> 4   0.08333333         3.1   -0.9152542         0.2  setosa
#> 5   0.19444444         3.6   -0.9322034         0.2  setosa
#> 6   0.30555556         3.9   -0.8813559         0.4  setosa

# "expand" ranges by a factor or a given value
x <- 5:15
x
#>  [1]  5  6  7  8  9 10 11 12 13 14 15
# both will expand the range by 10%
rescale(x, multiply = 1.1)
#>  [1]  4.5  5.6  6.7  7.8  8.9 10.0 11.1 12.2 13.3 14.4 15.5
#> (original range = 5 to 15)
#> 
rescale(x, add = 0.5)
#>  [1]  4.5  5.6  6.7  7.8  8.9 10.0 11.1 12.2 13.3 14.4 15.5
#> (original range = 5 to 15)
#> 

# expand range by different values
rescale(x, add = c(1, 3))
#>  [1]  4.0  5.4  6.8  8.2  9.6 11.0 12.4 13.8 15.2 16.6 18.0
#> (original range = 5 to 15)
#> 

# Specify list of multipliers
d <- data.frame(x = 5:15, y = 5:15)
rescale(d, multiply = list(x = 1.1, y = 0.5))
#>       x    y
#> 1   4.5  7.5
#> 2   5.6  8.0
#> 3   6.7  8.5
#> 4   7.8  9.0
#> 5   8.9  9.5
#> 6  10.0 10.0
#> 7  11.1 10.5
#> 8  12.2 11.0
#> 9  13.3 11.5
#> 10 14.4 12.0
#> 11 15.5 12.5
```
