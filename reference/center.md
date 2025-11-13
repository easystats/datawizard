# Centering (Grand-Mean Centering)

Performs a grand-mean centering of data.

## Usage

``` r
center(x, ...)

centre(x, ...)

# S3 method for class 'numeric'
center(
  x,
  robust = FALSE,
  weights = NULL,
  reference = NULL,
  center = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
center(
  x,
  select = NULL,
  exclude = NULL,
  robust = FALSE,
  weights = NULL,
  reference = NULL,
  center = NULL,
  force = FALSE,
  remove_na = c("none", "selected", "all"),
  append = FALSE,
  ignore_case = FALSE,
  verbose = TRUE,
  regex = FALSE,
  ...
)
```

## Arguments

- x:

  A (grouped) data frame, a (numeric or character) vector or a factor.

- ...:

  Currently not used.

- robust:

  Logical, if `TRUE`, centering is done by subtracting the median from
  the variables. If `FALSE`, variables are centered by subtracting the
  mean.

- weights:

  Can be `NULL` (for no weighting), or:

  - For data frames: a numeric vector of weights, or a character of the
    name of a column in the `data.frame` that contains the weights.

  - For numeric vectors: a numeric vector of weights.

- reference:

  A data frame or variable from which the centrality and deviation will
  be computed instead of from the input variable. Useful for
  standardizing a subset or new data according to another data frame.

- center:

  Numeric value, which can be used as alternative to `reference` to
  define a reference centrality. If `center` is of length 1, it will be
  recycled to match the length of selected variables for centering.
  Else, `center` must be of same length as the number of selected
  variables. Values in `center` will be matched to selected variables in
  the provided order, unless a named vector is given. In this case,
  names are matched against the names of the selected variables.

- verbose:

  Toggle warnings and messages.

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

  - one of the following select-helpers: `starts_with()`, `ends_with()`,
    `contains()`, a range using `:`, or `regex()`. `starts_with()`,
    `ends_with()`, and `contains()` accept several patterns, e.g
    `starts_with("Sep", "Petal")`. `regex()` can be used to define
    regular expression patterns.

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

- force:

  Logical, if `TRUE`, forces centering of factors as well. Factors are
  converted to numerical values, with the lowest level being the value
  `1` (unless the factor has numeric levels, which are converted to the
  corresponding numeric value).

- remove_na:

  How should missing values (`NA`) be treated: if `"none"` (default):
  each column's standardization is done separately, ignoring `NA`s.
  Else, rows with `NA` in the columns selected with `select` / `exclude`
  (`"selected"`) or in all columns (`"all"`) are dropped before
  standardization, and the resulting data frame does not include these
  cases.

- append:

  Logical or string. If `TRUE`, centered variables get new column names
  (with the suffix `"_c"`) and are appended (column bind) to `x`, thus
  returning both the original and the centered variables. If `FALSE`,
  original variables in `x` will be overwritten by their centered
  versions. If a character value, centered variables are appended with
  new column names (using the defined suffix) to the original data
  frame.

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

The centered variables.

## Note

**Difference between centering and standardizing**: Standardized
variables are computed by subtracting the mean of the variable and then
dividing it by the standard deviation, while centering variables
involves only the subtraction.

## Selection of variables - the `select` argument

For most functions that have a `select` argument (including this
function), the complete input data frame is returned, even when `select`
only selects a range of variables. That is, the function is only applied
to those variables that have a match in `select`, while all other
variables remain unchanged. In other words: for this function, `select`
will not omit any non-included variables, so that the returned data
frame will include all variables from the input data frame.

## See also

If centering within-clusters (instead of grand-mean centering) is
required, see
[`demean()`](https://easystats.github.io/datawizard/reference/demean.md).
For standardizing, see
[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md),
and
[`makepredictcall.dw_transformer()`](https://easystats.github.io/datawizard/reference/makepredictcall.dw_transformer.md)
for use in model formulas.

## Examples

``` r
data(iris)

# entire data frame or a vector
head(iris$Sepal.Width)
#> [1] 3.5 3.0 3.2 3.1 3.6 3.9
head(center(iris$Sepal.Width))
#> [1]  0.44266667 -0.05733333  0.14266667  0.04266667  0.54266667  0.84266667
head(center(iris))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1   -0.7433333  0.44266667       -2.358  -0.9993333  setosa
#> 2   -0.9433333 -0.05733333       -2.358  -0.9993333  setosa
#> 3   -1.1433333  0.14266667       -2.458  -0.9993333  setosa
#> 4   -1.2433333  0.04266667       -2.258  -0.9993333  setosa
#> 5   -0.8433333  0.54266667       -2.358  -0.9993333  setosa
#> 6   -0.4433333  0.84266667       -2.058  -0.7993333  setosa
head(center(iris, force = TRUE))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1   -0.7433333  0.44266667       -2.358  -0.9993333      -1
#> 2   -0.9433333 -0.05733333       -2.358  -0.9993333      -1
#> 3   -1.1433333  0.14266667       -2.458  -0.9993333      -1
#> 4   -1.2433333  0.04266667       -2.258  -0.9993333      -1
#> 5   -0.8433333  0.54266667       -2.358  -0.9993333      -1
#> 6   -0.4433333  0.84266667       -2.058  -0.7993333      -1

# only the selected columns from a data frame
center(anscombe, select = c("x1", "x3"))
#>    x1 x2 x3 x4    y1   y2    y3    y4
#> 1   1 10  1  8  8.04 9.14  7.46  6.58
#> 2  -1  8 -1  8  6.95 8.14  6.77  5.76
#> 3   4 13  4  8  7.58 8.74 12.74  7.71
#> 4   0  9  0  8  8.81 8.77  7.11  8.84
#> 5   2 11  2  8  8.33 9.26  7.81  8.47
#> 6   5 14  5  8  9.96 8.10  8.84  7.04
#> 7  -3  6 -3  8  7.24 6.13  6.08  5.25
#> 8  -5  4 -5 19  4.26 3.10  5.39 12.50
#> 9   3 12  3  8 10.84 9.13  8.15  5.56
#> 10 -2  7 -2  8  4.82 7.26  6.42  7.91
#> 11 -4  5 -4  8  5.68 4.74  5.73  6.89
center(anscombe, exclude = c("x1", "x3"))
#>    x1 x2 x3 x4          y1         y2    y3         y4
#> 1  10  1 10 -1  0.53909091  1.6390909 -0.04 -0.9209091
#> 2   8 -1  8 -1 -0.55090909  0.6390909 -0.73 -1.7409091
#> 3  13  4 13 -1  0.07909091  1.2390909  5.24  0.2090909
#> 4   9  0  9 -1  1.30909091  1.2690909 -0.39  1.3390909
#> 5  11  2 11 -1  0.82909091  1.7590909  0.31  0.9690909
#> 6  14  5 14 -1  2.45909091  0.5990909  1.34 -0.4609091
#> 7   6 -3  6 -1 -0.26090909 -1.3709091 -1.42 -2.2509091
#> 8   4 -5  4 10 -3.24090909 -4.4009091 -2.11  4.9990909
#> 9  12  3 12 -1  3.33909091  1.6290909  0.65 -1.9409091
#> 10  7 -2  7 -1 -2.68090909 -0.2409091 -1.08  0.4090909
#> 11  5 -4  5 -1 -1.82090909 -2.7609091 -1.77 -0.6109091

# centering with reference center and scale
d <- data.frame(
  a = c(-2, -1, 0, 1, 2),
  b = c(3, 4, 5, 6, 7)
)

# default centering at mean
center(d)
#>    a  b
#> 1 -2 -2
#> 2 -1 -1
#> 3  0  0
#> 4  1  1
#> 5  2  2

# centering, using 0 as mean
center(d, center = 0)
#>    a b
#> 1 -2 3
#> 2 -1 4
#> 3  0 5
#> 4  1 6
#> 5  2 7

# centering, using -5 as mean
center(d, center = -5)
#>   a  b
#> 1 3  8
#> 2 4  9
#> 3 5 10
#> 4 6 11
#> 5 7 12
```
