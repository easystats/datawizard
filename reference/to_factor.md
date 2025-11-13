# Convert data to factors

Convert data to factors

## Usage

``` r
to_factor(x, ...)

# S3 method for class 'numeric'
to_factor(x, labels_to_levels = TRUE, verbose = TRUE, ...)

# S3 method for class 'data.frame'
to_factor(
  x,
  select = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  append = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A data frame or vector.

- ...:

  Arguments passed to or from other methods.

- labels_to_levels:

  Logical, if `TRUE`, value labels are used as factor levels after `x`
  was converted to factor. Else, factor levels are based on the values
  of `x` (i.e. as if using
  [`as.factor()`](https://rdrr.io/r/base/factor.html)).

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

- ignore_case:

  Logical, if `TRUE` and when one of the select-helpers or a regular
  expression is used in `select`, ignores lower/upper case in the search
  pattern when matching against variable names.

- append:

  Logical or string. If `TRUE`, recoded or converted variables get new
  column names and are appended (column bind) to `x`, thus returning
  both the original and the recoded variables. The new columns get a
  suffix, based on the calling function: `"_r"` for recode functions,
  `"_n"` for
  [`to_numeric()`](https://easystats.github.io/datawizard/reference/to_numeric.md),
  `"_f"` for `to_factor()`, or `"_s"` for
  [`slide()`](https://easystats.github.io/datawizard/reference/slide.md).
  If `append=FALSE`, original variables in `x` will be overwritten by
  their recoded versions. If a character value, recoded variables are
  appended with new column names (using the defined suffix) to the
  original data frame.

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

A factor, or a data frame of factors.

## Details

Convert variables or data into factors. If the data is labelled, value
labels will be used as factor levels. The counterpart to convert
variables into numeric is
[`to_numeric()`](https://easystats.github.io/datawizard/reference/to_numeric.md).

## Note

Factors are ignored and returned as is. If you want to use value labels
as levels for factors, use
[`labels_to_levels()`](https://easystats.github.io/datawizard/reference/labels_to_levels.md)
instead.

## Selection of variables - the `select` argument

For most functions that have a `select` argument (including this
function), the complete input data frame is returned, even when `select`
only selects a range of variables. That is, the function is only applied
to those variables that have a match in `select`, while all other
variables remain unchanged. In other words: for this function, `select`
will not omit any non-included variables, so that the returned data
frame will include all variables from the input data frame.

## Examples

``` r
str(to_factor(iris))
#> 'data.frame':    150 obs. of  5 variables:
#>  $ Sepal.Length: Factor w/ 35 levels "4.3","4.4","4.5",..: 9 7 5 4 8 12 4 8 2 7 ...
#>  $ Sepal.Width : Factor w/ 23 levels "2","2.2","2.3",..: 15 10 12 11 16 19 14 14 9 11 ...
#>  $ Petal.Length: Factor w/ 43 levels "1","1.1","1.2",..: 5 5 4 6 5 8 5 6 5 6 ...
#>  $ Petal.Width : Factor w/ 22 levels "0.1","0.2","0.3",..: 2 2 2 2 2 4 3 2 2 1 ...
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

# use labels as levels
data(efc)
str(efc$c172code)
#>  num [1:100] 2 2 1 2 2 2 2 2 NA 2 ...
#>  - attr(*, "label")= chr "carer's level of education"
#>  - attr(*, "labels")= Named num [1:3] 1 2 3
#>   ..- attr(*, "names")= chr [1:3] "low level of education" "intermediate level of education" "high level of education"
head(to_factor(efc$c172code))
#> [1] intermediate level of education intermediate level of education
#> [3] low level of education          intermediate level of education
#> [5] intermediate level of education intermediate level of education
#> 3 Levels: low level of education ... high level of education
```
