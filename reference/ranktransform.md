# (Signed) rank transformation

Transform numeric values with the integers of their rank (i.e., 1st
smallest, 2nd smallest, 3rd smallest, etc.). Setting the `sign` argument
to `TRUE` will give you signed ranks, where the ranking is done
according to absolute size but where the sign is preserved (i.e., 2, 1,
-3, 4).

## Usage

``` r
ranktransform(x, ...)

# S3 method for class 'numeric'
ranktransform(
  x,
  sign = FALSE,
  method = "average",
  zeros = "na",
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
ranktransform(
  x,
  select = NULL,
  exclude = NULL,
  sign = FALSE,
  method = "average",
  ignore_case = FALSE,
  regex = FALSE,
  zeros = "na",
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  Object.

- ...:

  Arguments passed to or from other methods.

- sign:

  Logical, if `TRUE`, return signed ranks.

- method:

  Treatment of ties. Can be one of `"average"` (default), `"first"`,
  `"last"`, `"random"`, `"max"` or `"min"`. See
  [`rank()`](https://rdrr.io/r/base/rank.html) for details.

- zeros:

  How to handle zeros. If `"na"` (default), they are marked as `NA`. If
  `"signrank"`, they are kept during the ranking and marked as zeros.
  This is only used when `sign = TRUE`.

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

A rank-transformed object.

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
[`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md),
[`reverse()`](https://easystats.github.io/datawizard/reference/reverse.md),
[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)

## Examples

``` r
ranktransform(c(0, 1, 5, -5, -2))
#> [1] 3 4 5 1 2

# By default, zeros are converted to NA
suppressWarnings(
  ranktransform(c(0, 1, 5, -5, -2), sign = TRUE)
)
#> [1]   NA  1.0  3.5 -3.5 -2.0
ranktransform(c(0, 1, 5, -5, -2), sign = TRUE, zeros = "signrank")
#> [1]  0.0  2.0  4.5 -4.5 -3.0

head(ranktransform(trees))
#>   Girth Height Volume
#> 1     1    6.0    2.5
#> 2     2    3.0    2.5
#> 3     3    1.0    1.0
#> 4     4    8.5    5.0
#> 5     5   25.5    7.0
#> 6     6   28.0    9.0
```
