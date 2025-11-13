# Unite ("merge") multiple variables

Merge values of multiple variables per observation into one new
variable.

## Usage

``` r
data_unite(
  data,
  new_column = NULL,
  select = NULL,
  exclude = NULL,
  separator = "_",
  append = FALSE,
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

- new_column:

  The name of the new column, as a string.

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

- separator:

  A character to use between values.

- append:

  Logical, if `FALSE` (default), removes original columns that were
  united. If `TRUE`, all columns are preserved and the new column is
  appended to the data frame.

- remove_na:

  Logical, if `TRUE`, missing values (`NA`) are not included in the
  united values. If `FALSE`, missing values are represented as `"NA"` in
  the united values.

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

`data`, with a newly created variable.

## See also

[`data_separate()`](https://easystats.github.io/datawizard/reference/data_separate.md)

## Examples

``` r
d <- data.frame(
  x = 1:3,
  y = letters[1:3],
  z = 6:8
)
d
#>   x y z
#> 1 1 a 6
#> 2 2 b 7
#> 3 3 c 8
data_unite(d, new_column = "xyz")
#>     xyz
#> 1 1_a_6
#> 2 2_b_7
#> 3 3_c_8
data_unite(d, new_column = "xyz", remove = FALSE)
#>     xyz
#> 1 1_a_6
#> 2 2_b_7
#> 3 3_c_8
data_unite(d, new_column = "xyz", select = c("x", "z"))
#>   y xyz
#> 1 a 1_6
#> 2 b 2_7
#> 3 c 3_8
data_unite(d, new_column = "xyz", select = c("x", "z"), append = TRUE)
#>   x y z xyz
#> 1 1 a 6 1_6
#> 2 2 b 7 2_7
#> 3 3 c 8 3_8
```
