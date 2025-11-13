# Normalize numeric variable to 0-1 range

Performs a normalization of data, i.e., it scales variables in the range
0 - 1. This is a special case of
[`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md).
`unnormalize()` is the counterpart, but only works for variables that
have been normalized with `normalize()`.

## Usage

``` r
normalize(x, ...)

# S3 method for class 'numeric'
normalize(x, include_bounds = TRUE, verbose = TRUE, ...)

# S3 method for class 'data.frame'
normalize(
  x,
  select = NULL,
  exclude = NULL,
  include_bounds = TRUE,
  append = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)

unnormalize(x, ...)

# S3 method for class 'numeric'
unnormalize(x, verbose = TRUE, ...)

# S3 method for class 'data.frame'
unnormalize(
  x,
  select = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'grouped_df'
unnormalize(
  x,
  select = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A numeric vector, (grouped) data frame, or matrix. See 'Details'.

- ...:

  Arguments passed to or from other methods.

- include_bounds:

  Numeric or logical. Using this can be useful in case of
  beta-regression, where the response variable is not allowed to include
  zeros and ones. If `TRUE`, the input is normalized to a range that
  includes zero and one. If `FALSE`, the return value is compressed,
  using Smithson and Verkuilen's (2006) formula
  `(x * (n - 1) + 0.5) / n`, to avoid zeros and ones in the normalized
  variables. Else, if numeric (e.g., `0.001`), `include_bounds` defines
  the "distance" to the lower and upper bound, i.e. the normalized
  vectors are rescaled to a range from `0 + include_bounds` to
  `1 - include_bounds`.

- verbose:

  Toggle warnings and messages on or off.

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

  Logical or string. If `TRUE`, standardized variables get new column
  names (with the suffix `"_z"`) and are appended (column bind) to `x`,
  thus returning both the original and the standardized variables. If
  `FALSE`, original variables in `x` will be overwritten by their
  standardized versions. If a character value, standardized variables
  are appended with new column names (using the defined suffix) to the
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

A normalized object.

## Details

- If `x` is a matrix, normalization is performed across all values (not
  column- or row-wise). For column-wise normalization, convert the
  matrix to a data.frame.

- If `x` is a grouped data frame (`grouped_df`), normalization is
  performed separately for each group.

## Selection of variables - the `select` argument

For most functions that have a `select` argument (including this
function), the complete input data frame is returned, even when `select`
only selects a range of variables. That is, the function is only applied
to those variables that have a match in `select`, while all other
variables remain unchanged. In other words: for this function, `select`
will not omit any non-included variables, so that the returned data
frame will include all variables from the input data frame.

## References

Smithson M, Verkuilen J (2006). A Better Lemon Squeezer?
Maximum-Likelihood Regression with Beta-Distributed Dependent Variables.
Psychological Methods, 11(1), 54–71.

## See also

See
[`makepredictcall.dw_transformer()`](https://easystats.github.io/datawizard/reference/makepredictcall.dw_transformer.md)
for use in model formulas.

Other transform utilities:
[`ranktransform()`](https://easystats.github.io/datawizard/reference/ranktransform.md),
[`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md),
[`reverse()`](https://easystats.github.io/datawizard/reference/reverse.md),
[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)

## Examples

``` r

normalize(c(0, 1, 5, -5, -2))
#> [1] 0.5 0.6 1.0 0.0 0.3
#> (original range = -5 to 5)
#> 
normalize(c(0, 1, 5, -5, -2), include_bounds = FALSE)
#> [1] 0.50 0.58 0.90 0.10 0.34
#> (original range = -5 to 5)
#> 
# use a value defining the bounds
normalize(c(0, 1, 5, -5, -2), include_bounds = .001)
#> [1] 0.5000 0.5998 0.9990 0.0010 0.3004
#> (original range = -5 to 5)
#> 

head(normalize(trees))
#>        Girth     Height      Volume
#> 1 0.00000000 0.29166667 0.001497006
#> 2 0.02439024 0.08333333 0.001497006
#> 3 0.04065041 0.00000000 0.000000000
#> 4 0.17886179 0.37500000 0.092814371
#> 5 0.19512195 0.75000000 0.128742515
#> 6 0.20325203 0.83333333 0.142215569
```
