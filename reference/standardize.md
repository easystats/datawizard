# Standardization (Z-scoring)

Performs a standardization of data (z-scoring), i.e., centering and
scaling, so that the data is expressed in terms of standard deviation
(i.e., mean = 0, SD = 1) or Median Absolute Deviance (median = 0, MAD =
1). When applied to a statistical model, this function extracts the
dataset, standardizes it, and refits the model with this standardized
version of the dataset. The
[`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
function can also be used to scale all numeric variables within the 0 -
1 range.\
\
For model standardization, see
[`standardize.default()`](https://easystats.github.io/datawizard/reference/standardize.default.md).

## Usage

``` r
standardize(x, ...)

standardise(x, ...)

# S3 method for class 'numeric'
standardize(
  x,
  robust = FALSE,
  two_sd = FALSE,
  weights = NULL,
  reference = NULL,
  center = NULL,
  scale = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'factor'
standardize(
  x,
  robust = FALSE,
  two_sd = FALSE,
  weights = NULL,
  force = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
standardize(
  x,
  select = NULL,
  exclude = NULL,
  robust = FALSE,
  two_sd = FALSE,
  weights = NULL,
  reference = NULL,
  center = NULL,
  scale = NULL,
  remove_na = c("none", "selected", "all"),
  force = FALSE,
  append = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)

unstandardize(x, ...)

unstandardise(x, ...)

# S3 method for class 'numeric'
unstandardize(
  x,
  center = NULL,
  scale = NULL,
  reference = NULL,
  robust = FALSE,
  two_sd = FALSE,
  ...
)

# S3 method for class 'data.frame'
unstandardize(
  x,
  center = NULL,
  scale = NULL,
  reference = NULL,
  robust = FALSE,
  two_sd = FALSE,
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

  A (grouped) data frame, a vector or a statistical model (for
  `unstandardize()` cannot be a model).

- ...:

  Arguments passed to or from other methods.

- robust:

  Logical, if `TRUE`, centering is done by subtracting the median from
  the variables and dividing it by the median absolute deviation (MAD).
  If `FALSE`, variables are standardized by subtracting the mean and
  dividing it by the standard deviation (SD).

- two_sd:

  If `TRUE`, the variables are scaled by two times the deviation (SD or
  MAD depending on `robust`). This method can be useful to obtain model
  coefficients of continuous parameters comparable to coefficients
  related to binary predictors, when applied to **the predictors** (not
  the outcome) (Gelman, 2008).

- weights:

  Can be `NULL` (for no weighting), or:

  - For model: if `TRUE` (default), a weighted-standardization is
    carried out.

  - For `data.frame`s: a numeric vector of weights, or a character of
    the name of a column in the `data.frame` that contains the weights.

  - For numeric vectors: a numeric vector of weights.

- reference:

  A data frame or variable from which the centrality and deviation will
  be computed instead of from the input variable. Useful for
  standardizing a subset or new data according to another data frame.

- center, scale:

  - For `standardize()`:\
    Numeric values, which can be used as alternative to `reference` to
    define a reference centrality and deviation. If `scale` and `center`
    are of length 1, they will be recycled to match the length of
    selected variables for standardization. Else, `center` and `scale`
    must be of same length as the number of selected variables. Values
    in `center` and `scale` will be matched to selected variables in the
    provided order, unless a named vector is given. In this case, names
    are matched against the names of the selected variables.

  - For `unstandardize()`:\
    `center` and `scale` correspond to the center (the mean / median)
    and the scale (SD / MAD) of the original non-standardized data (for
    data frames, should be named, or have column order correspond to the
    numeric column). However, one can also directly provide the original
    data through `reference`, from which the center and the scale will
    be computed (according to `robust` and `two_sd`). Alternatively, if
    the input contains the attributes `center` and `scale` (as does the
    output of `standardize()`), it will take it from there if the rest
    of the arguments are absent.

- verbose:

  Toggle warnings and messages on or off.

- force:

  Logical, if `TRUE`, forces recoding of factors and character vectors
  as well.

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

- remove_na:

  How should missing values (`NA`) be treated: if `"none"` (default):
  each column's standardization is done separately, ignoring `NA`s.
  Else, rows with `NA` in the columns selected with `select` / `exclude`
  (`"selected"`) or in all columns (`"all"`) are dropped before
  standardization, and the resulting data frame does not include these
  cases.

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

The standardized object (either a standardize data frame or a
statistical model fitted on standardized data).

## Note

When `x` is a vector or a data frame with `remove_na = "none")`, missing
values are preserved, so the return value has the same length / number
of rows as the original input.

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
[`center()`](https://easystats.github.io/datawizard/reference/center.md)
for grand-mean centering of variables, and
[`makepredictcall.dw_transformer()`](https://easystats.github.io/datawizard/reference/makepredictcall.dw_transformer.md)
for use in model formulas.

Other transform utilities:
[`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md),
[`ranktransform()`](https://easystats.github.io/datawizard/reference/ranktransform.md),
[`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md),
[`reverse()`](https://easystats.github.io/datawizard/reference/reverse.md)

Other standardize:
[`standardize.default()`](https://easystats.github.io/datawizard/reference/standardize.default.md)

## Examples

``` r
d <- iris[1:4, ]

# vectors
standardise(d$Petal.Length)
#> [1]  0.000000  0.000000 -1.224745  1.224745
#> (center: 1.4, scale = 0.082)
#> 

# Data frames
# overwrite
standardise(d, select = c("Sepal.Length", "Sepal.Width"))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1    1.2402159   1.3887301          1.4         0.2  setosa
#> 2    0.3382407  -0.9258201          1.4         0.2  setosa
#> 3   -0.5637345   0.0000000          1.3         0.2  setosa
#> 4   -1.0147221  -0.4629100          1.5         0.2  setosa

# append
standardise(d, select = c("Sepal.Length", "Sepal.Width"), append = TRUE)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Sepal.Length_z
#> 1          5.1         3.5          1.4         0.2  setosa      1.2402159
#> 2          4.9         3.0          1.4         0.2  setosa      0.3382407
#> 3          4.7         3.2          1.3         0.2  setosa     -0.5637345
#> 4          4.6         3.1          1.5         0.2  setosa     -1.0147221
#>   Sepal.Width_z
#> 1     1.3887301
#> 2    -0.9258201
#> 3     0.0000000
#> 4    -0.4629100

# append, suffix
standardise(d, select = c("Sepal.Length", "Sepal.Width"), append = "_std")
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Sepal.Length_std
#> 1          5.1         3.5          1.4         0.2  setosa        1.2402159
#> 2          4.9         3.0          1.4         0.2  setosa        0.3382407
#> 3          4.7         3.2          1.3         0.2  setosa       -0.5637345
#> 4          4.6         3.1          1.5         0.2  setosa       -1.0147221
#>   Sepal.Width_std
#> 1       1.3887301
#> 2      -0.9258201
#> 3       0.0000000
#> 4      -0.4629100

# standardizing with reference center and scale
d <- data.frame(
  a = c(-2, -1, 0, 1, 2),
  b = c(3, 4, 5, 6, 7)
)

# default standardization, based on mean and sd of each variable
standardize(d) # means are 0 and 5, sd ~ 1.581139
#>            a          b
#> 1 -1.2649111 -1.2649111
#> 2 -0.6324555 -0.6324555
#> 3  0.0000000  0.0000000
#> 4  0.6324555  0.6324555
#> 5  1.2649111  1.2649111

# standardization, based on mean and sd set to the same values
standardize(d, center = c(0, 5), scale = c(1.581, 1.581))
#>            a          b
#> 1 -1.2650221 -1.2650221
#> 2 -0.6325111 -0.6325111
#> 3  0.0000000  0.0000000
#> 4  0.6325111  0.6325111
#> 5  1.2650221  1.2650221

# standardization, mean and sd for each variable newly defined
standardize(d, center = c(3, 4), scale = c(2, 4))
#>      a     b
#> 1 -2.5 -0.25
#> 2 -2.0  0.00
#> 3 -1.5  0.25
#> 4 -1.0  0.50
#> 5 -0.5  0.75

# standardization, taking same mean and sd for each variable
standardize(d, center = 1, scale = 3)
#>            a         b
#> 1 -1.0000000 0.6666667
#> 2 -0.6666667 1.0000000
#> 3 -0.3333333 1.3333333
#> 4  0.0000000 1.6666667
#> 5  0.3333333 2.0000000
```
