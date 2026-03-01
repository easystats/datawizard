# Describe a distribution

This function describes a distribution by a set of indices (e.g.,
measures of centrality, dispersion, range, skewness, (excess) kurtosis).

## Usage

``` r
describe_distribution(x, ...)

# S3 method for class 'numeric'
describe_distribution(
  x,
  centrality = "mean",
  dispersion = TRUE,
  iqr = TRUE,
  range = TRUE,
  quartiles = FALSE,
  ci = NULL,
  iterations = 100,
  threshold = 0.1,
  verbose = TRUE,
  ...
)

# S3 method for class 'factor'
describe_distribution(x, dispersion = TRUE, range = TRUE, verbose = TRUE, ...)

# S3 method for class 'data.frame'
describe_distribution(
  x,
  select = NULL,
  exclude = NULL,
  centrality = "mean",
  dispersion = TRUE,
  iqr = TRUE,
  range = TRUE,
  quartiles = FALSE,
  include_factors = FALSE,
  ci = NULL,
  iterations = 100,
  threshold = 0.1,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  by = NULL,
  ...
)
```

## Arguments

- x:

  A numeric vector, a character vector, a data frame, or a list. See
  `Details`.

- ...:

  Additional arguments to be passed to or from methods.

- centrality:

  The point-estimates (centrality indices) to compute. Character
  (vector) or list with one or more of these options: `"median"`,
  `"mean"`, `"MAP"` (see
  [`map_estimate()`](https://rdrr.io/pkg/bayestestR/man/map_estimate.html)),
  `"trimmed"` (which is just `mean(x, trim = threshold)`), `"mode"` or
  `"all"`.

- dispersion:

  Logical, if `TRUE`, computes indices of dispersion related to the
  estimate(s) (`SD` and `MAD` for `mean` and `median`, respectively).
  Dispersion is not available for `"MAP"` or `"mode"` centrality
  indices.

- iqr:

  Logical, if `TRUE`, the interquartile range is calculated (based on
  [`stats::IQR()`](https://rdrr.io/r/stats/IQR.html), using `type = 6`).

- range:

  Return the range (min and max).

- quartiles:

  Return the first and third quartiles (25th and 75th percentiles).

- ci:

  Confidence Interval (CI) level. Default is `NULL`, i.e. no confidence
  intervals are computed. If not `NULL`, confidence intervals are based
  on bootstrap replicates (see `iterations`).

- iterations:

  The number of bootstrap replicates for computing confidence intervals.
  Only applies when `ci` is not `NULL`. Defaults to `100`. For more
  stable results, increase the number of `iterations`, but note that
  this can also increase the computation time significantly.

- threshold:

  For `centrality = "trimmed"` (i.e. trimmed mean), indicates the
  fraction (0 to 0.5) of observations to be trimmed from each end of the
  vector before the mean is computed.

- verbose:

  Show or silence warnings and messages.

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

- include_factors:

  Logical, if `TRUE`, factors are included in the output, however, only
  columns for range (first and last factor levels) as well as n and
  missing will contain information.

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

- by:

  Column names indicating how to split the data in various groups before
  describing the distribution. `by` groups will be added to potentially
  existing groups created by
  [`data_group()`](https://easystats.github.io/datawizard/reference/data_group.md).

## Value

A data frame with columns that describe the properties of the variables.

## Details

If `x` is a data frame, only numeric variables are kept and will be
displayed in the summary by default.

If `x` is a list, the behavior is different whether `x` is a stored
list. If `x` is stored (for example, `describe_distribution(mylist)`
where `mylist` was created before), artificial variable names are used
in the summary (`Var_1`, `Var_2`, etc.). If `x` is an unstored list (for
example, `describe_distribution(list(mtcars$mpg))`), then `"mtcars$mpg"`
is used as variable name.

## Note

There is also a
[`plot()`-method](https://easystats.github.io/see/articles/parameters.html)
implemented in the [**see**-package](https://easystats.github.io/see/).

## See also

[`kurtosis()`](https://easystats.github.io/datawizard/reference/skewness.md)
to compute kurtosis (recognized as excess kurtosis).

## Examples

``` r
describe_distribution(rnorm(100))
#>  Mean |   SD |  IQR |         Range | Skewness | Kurtosis |   n | n_Missing
#> ---------------------------------------------------------------------------
#> -0.05 | 1.07 | 1.69 | [-3.31, 2.46] |    -0.31 |    -0.07 | 100 |         0

data(iris)
describe_distribution(iris)
#> Variable     | Mean |   SD |  IQR |        Range | Skewness | Kurtosis |   n | n_Missing
#> ----------------------------------------------------------------------------------------
#> Sepal.Length | 5.84 | 0.83 | 1.30 | [4.30, 7.90] |     0.31 |    -0.55 | 150 |         0
#> Sepal.Width  | 3.06 | 0.44 | 0.52 | [2.00, 4.40] |     0.32 |     0.23 | 150 |         0
#> Petal.Length | 3.76 | 1.77 | 3.52 | [1.00, 6.90] |    -0.27 |    -1.40 | 150 |         0
#> Petal.Width  | 1.20 | 0.76 | 1.50 | [0.10, 2.50] |    -0.10 |    -1.34 | 150 |         0
describe_distribution(iris, include_factors = TRUE, quartiles = TRUE)
#> Variable     | Mean |   SD |  IQR |               Range |  Quartiles | Skewness
#> -------------------------------------------------------------------------------
#> Sepal.Length | 5.84 | 0.83 | 1.30 |          [4.3, 7.9] | 5.10, 6.40 |     0.31
#> Sepal.Width  | 3.06 | 0.44 | 0.52 |            [2, 4.4] | 2.80, 3.30 |     0.32
#> Petal.Length | 3.76 | 1.77 | 3.52 |            [1, 6.9] | 1.60, 5.10 |    -0.27
#> Petal.Width  | 1.20 | 0.76 | 1.50 |          [0.1, 2.5] | 0.30, 1.80 |    -0.10
#> Species      |      |      |      | [setosa, virginica] |            |     0.00
#> 
#> Variable     | Kurtosis |   n | n_Missing
#> -----------------------------------------
#> Sepal.Length |    -0.55 | 150 |         0
#> Sepal.Width  |     0.23 | 150 |         0
#> Petal.Length |    -1.40 | 150 |         0
#> Petal.Width  |    -1.34 | 150 |         0
#> Species      |    -1.51 | 150 |         0
describe_distribution(list(mtcars$mpg, mtcars$cyl))
#> Variable   |  Mean |   SD |  IQR |          Range | Skewness | Kurtosis |  n | n_Missing
#> ----------------------------------------------------------------------------------------
#> mtcars$mpg | 20.09 | 6.03 | 7.53 | [10.40, 33.90] |     0.67 |    -0.02 | 32 |         0
#> mtcars$cyl |  6.19 | 1.79 | 4.00 |   [4.00, 8.00] |    -0.19 |    -1.76 | 32 |         0
```
