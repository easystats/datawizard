# Count specific values row-wise

`row_count()` mimics base R's
[`rowSums()`](https://rdrr.io/r/base/colSums.html), with sums for a
specific value indicated by `count`. Hence, it is similar to
`rowSums(x == count, na.rm = TRUE)`, but offers some more options,
including strict comparisons. Comparisons using `==` coerce values to
atomic vectors, thus both `2 == 2` and `"2" == 2` are `TRUE`. In
`row_count()`, it is also possible to make "type safe" comparisons using
the `allow_coercion` argument, where `"2" == 2` is not true.

## Usage

``` r
row_count(
  data,
  select = NULL,
  exclude = NULL,
  count = NULL,
  allow_coercion = TRUE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame with at least two columns, where number of specific
  values are counted row-wise.

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

- count:

  The value for which the row sum should be computed. May be a numeric
  value, a character string (for factors or character vectors), `NA` or
  `Inf`.

- allow_coercion:

  Logical. If `FALSE`, `count` matches only values of same class (i.e.
  when `count = 2`, the value `"2"` is not counted and vice versa). By
  default, when `allow_coercion = TRUE`, `count = 2` also matches `"2"`.
  In order to count factor levels in the data, use
  `count = factor("level")`. See 'Examples'.

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

A vector with row-wise counts of values specified in `count`.

## Examples

``` r
dat <- data.frame(
  c1 = c(1, 2, NA, 4),
  c2 = c(NA, 2, NA, 5),
  c3 = c(NA, 4, NA, NA),
  c4 = c(2, 3, 7, 8)
)

# count all 4s per row
row_count(dat, count = 4)
#> [1] 0 1 0 1
# count all missing values per row
row_count(dat, count = NA)
#> [1] 2 0 3 1

dat <- data.frame(
  c1 = c("1", "2", NA, "3"),
  c2 = c(NA, "2", NA, "3"),
  c3 = c(NA, 4, NA, NA),
  c4 = c(2, 3, 7, Inf)
)
# count all 2s and "2"s per row
row_count(dat, count = 2)
#> [1] 1 2 0 0
# only count 2s, but not "2"s
row_count(dat, count = 2, allow_coercion = FALSE)
#> [1] 1 0 0 0

dat <- data.frame(
  c1 = factor(c("1", "2", NA, "3")),
  c2 = c("2", "1", NA, "3"),
  c3 = c(NA, 4, NA, NA),
  c4 = c(2, 3, 7, Inf)
)
# find only character "2"s
row_count(dat, count = "2", allow_coercion = FALSE)
#> [1] 1 0 0 0
# find only factor level "2"s
row_count(dat, count = factor("2"), allow_coercion = FALSE)
#> [1] 0 1 0 0
```
