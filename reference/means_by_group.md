# Summary of mean values by group

Computes summary table of means by groups.

## Usage

``` r
means_by_group(x, ...)

# S3 method for class 'numeric'
means_by_group(x, by = NULL, ci = 0.95, weights = NULL, digits = NULL, ...)

# S3 method for class 'data.frame'
means_by_group(
  x,
  select = NULL,
  by = NULL,
  ci = 0.95,
  weights = NULL,
  digits = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A vector or a data frame.

- ...:

  Currently not used

- by:

  If `x` is a numeric vector, `by` should be a factor that indicates the
  group-classifying categories. If `x` is a data frame, `by` should be a
  character string, naming the variable in `x` that is used for
  grouping. Numeric vectors are coerced to factors. Not that `by` should
  only refer to a single variable.

- ci:

  Level of confidence interval for mean estimates. Default is `0.95`.
  Use `ci = NA` to suppress confidence intervals.

- weights:

  If `x` is a numeric vector, `weights` should be a vector of weights
  that will be applied to weight all observations. If `x` is a data
  frame, `weights` can also be a character string indicating the name of
  the variable in `x` that should be used for weighting. Default is
  `NULL`, so no weights are used.

- digits:

  Optional scalar, indicating the amount of digits after decimal point
  when rounding estimates and values.

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

- verbose:

  Toggle warnings.

## Value

A data frame with information on mean and further summary statistics for
each sub-group.

## Details

This function is comparable to `aggregate(x, by, mean)`, but provides
some further information, including summary statistics from a
One-Way-ANOVA using `x` as dependent and `by` as independent variable.
[`emmeans::contrast()`](https://rvlenth.github.io/emmeans/reference/contrast.html)
is used to get p-values for each sub-group. P-values indicate whether
each group-mean is significantly different from the total mean.

## Examples

``` r
data(efc)
means_by_group(efc, "c12hour", "e42dep")
#> # Mean of average number of hours of care per week by elder's dependency
#> 
#> Category             |   Mean |  N |    SD |           95% CI |      p
#> ----------------------------------------------------------------------
#> independent          |  17.00 |  2 | 11.31 | [-68.46, 102.46] | 0.573 
#> slightly dependent   |  34.25 |  4 | 29.97 | [-26.18,  94.68] | 0.626 
#> moderately dependent |  52.75 | 28 | 51.83 | [ 29.91,  75.59] | > .999
#> severely dependent   | 106.97 | 63 | 65.88 | [ 91.74, 122.19] | 0.001 
#> Total                |  86.46 | 97 | 66.40 |                  |       
#> 
#> Anova: R2=0.186; adj.R2=0.160; F=7.098; p<.001

data(iris)
means_by_group(iris, "Sepal.Width", "Species")
#> # Mean of Sepal.Width by Species
#> 
#> Category   | Mean |   N |   SD |       95% CI |      p
#> ------------------------------------------------------
#> setosa     | 3.43 |  50 | 0.38 | [3.33, 3.52] | < .001
#> versicolor | 2.77 |  50 | 0.31 | [2.68, 2.86] | < .001
#> virginica  | 2.97 |  50 | 0.32 | [2.88, 3.07] | 0.035 
#> Total      | 3.06 | 150 | 0.44 |              |       
#> 
#> Anova: R2=0.401; adj.R2=0.393; F=49.160; p<.001

# weighting
efc$weight <- abs(rnorm(n = nrow(efc), mean = 1, sd = .5))
means_by_group(efc, "c12hour", "e42dep", weights = "weight")
#> # Mean of average number of hours of care per week by elder's dependency
#> 
#> Category             |   Mean |  N |    SD |           95% CI |      p
#> ----------------------------------------------------------------------
#> independent          |  17.86 |  3 | 11.31 | [-50.28,  86.00] | 0.417 
#> slightly dependent   |  34.93 |  5 | 30.14 | [-17.67,  87.53] | 0.591 
#> moderately dependent |  49.00 | 28 | 47.17 | [ 27.08,  70.93] | 0.841 
#> severely dependent   | 105.33 | 58 | 66.80 | [ 90.13, 120.54] | < .001
#> Total                |  82.20 | 97 | 65.86 |                  |       
#> 
#> Anova: R2=0.211; adj.R2=0.186; F=8.288; p<.001
```
