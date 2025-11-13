# Reverse-Score Variables

Reverse-score variables (change the keying/scoring direction).

## Usage

``` r
reverse(x, ...)

reverse_scale(x, ...)

# S3 method for class 'numeric'
reverse(x, range = NULL, verbose = TRUE, ...)

# S3 method for class 'data.frame'
reverse(
  x,
  select = NULL,
  exclude = NULL,
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

- range:

  Range of values that is used as reference for reversing the scale. For
  numeric variables, can be `NULL` or a numeric vector of length two,
  indicating the lowest and highest value of the reference range. If
  `NULL`, will take the range of the input vector (`range(x)`). For
  factors, `range` can be `NULL`, a numeric vector of length two, or a
  (numeric) vector of at least the same length as factor levels (i.e.
  must be equal to or larger than `nlevels(x)`). Note that providing a
  `range` for factors usually only makes sense when factor levels are
  numeric, not characters.

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

A reverse-scored object.

## Selection of variables - the `select` argument

For most functions that have a `select` argument (including this
function), the complete input data frame is returned, even when `select`
only selects a range of variables. That is, the function is only applied
to those variables that have a match in `select`, while all other
variables remain unchanged. In other words: for this function, `select`
will not omit any non-included variables, so that the returned data
frame will include all variables from the input data frame.

## See also

Other transform utilities:
[`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md),
[`ranktransform()`](https://easystats.github.io/datawizard/reference/ranktransform.md),
[`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md),
[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)

## Examples

``` r
reverse(c(1, 2, 3, 4, 5))
#> [1] 5 4 3 2 1
reverse(c(-2, -1, 0, 2, 1))
#> [1]  2  1  0 -2 -1

# Specify the "theoretical" range of the input vector
reverse(c(1, 3, 4), range = c(0, 4))
#> [1] 3 1 0

# Factor variables
reverse(factor(c(1, 2, 3, 4, 5)))
#> [1] 5 4 3 2 1
#> Levels: 1 2 3 4 5
reverse(factor(c(1, 2, 3, 4, 5)), range = 0:10)
#> [1] 9 8 7 6 5
#> Levels: 0 1 2 3 4 5 6 7 8 9 10

# Data frames
head(reverse(iris))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
#> 1          7.1         2.9          6.5         2.4 virginica
#> 2          7.3         3.4          6.5         2.4 virginica
#> 3          7.5         3.2          6.6         2.4 virginica
#> 4          7.6         3.3          6.4         2.4 virginica
#> 5          7.2         2.8          6.5         2.4 virginica
#> 6          6.8         2.5          6.2         2.2 virginica
head(reverse(iris, select = "Sepal.Length"))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          7.1         3.5          1.4         0.2  setosa
#> 2          7.3         3.0          1.4         0.2  setosa
#> 3          7.5         3.2          1.3         0.2  setosa
#> 4          7.6         3.1          1.5         0.2  setosa
#> 5          7.2         3.6          1.4         0.2  setosa
#> 6          6.8         3.9          1.7         0.4  setosa
```
