# Expand (i.e. replicate rows) a data frame

Expand a data frame by replicating rows based on another variable that
contains the counts of replications per row.

## Usage

``` r
data_replicate(
  data,
  expand = NULL,
  select = NULL,
  exclude = NULL,
  remove_na = FALSE,
  ignore_case = FALSE,
  verbose = TRUE,
  regex = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame.

- expand:

  The name of the column that contains the counts of replications for
  each row. Can also be a numeric value, indicating the position of that
  column. Note that the variable indicated by `expand` must be an
  integer vector.

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

  Logical. If `TRUE`, missing values in the column provided in `expand`
  are removed from the data frame. If `FALSE` and `expand` contains
  missing values, the function will throw an error.

- ignore_case:

  Logical, if `TRUE` and when one of the select-helpers or a regular
  expression is used in `select`, ignores lower/upper case in the search
  pattern when matching against variable names.

- verbose:

  Toggle warnings.

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

- ...:

  Currently not used.

## Value

A dataframe with each row replicated as many times as defined in
`expand`.

## Examples

``` r
data(mtcars)
data_replicate(head(mtcars), "carb")
#>     mpg cyl disp  hp drat    wt  qsec vs am gear
#> 1  21.0   6  160 110 3.90 2.620 16.46  0  1    4
#> 2  21.0   6  160 110 3.90 2.620 16.46  0  1    4
#> 3  21.0   6  160 110 3.90 2.620 16.46  0  1    4
#> 4  21.0   6  160 110 3.90 2.620 16.46  0  1    4
#> 5  21.0   6  160 110 3.90 2.875 17.02  0  1    4
#> 6  21.0   6  160 110 3.90 2.875 17.02  0  1    4
#> 7  21.0   6  160 110 3.90 2.875 17.02  0  1    4
#> 8  21.0   6  160 110 3.90 2.875 17.02  0  1    4
#> 9  22.8   4  108  93 3.85 2.320 18.61  1  1    4
#> 10 21.4   6  258 110 3.08 3.215 19.44  1  0    3
#> 11 18.7   8  360 175 3.15 3.440 17.02  0  0    3
#> 12 18.7   8  360 175 3.15 3.440 17.02  0  0    3
#> 13 18.1   6  225 105 2.76 3.460 20.22  1  0    3
```
