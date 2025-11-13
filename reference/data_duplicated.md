# Extract all duplicates

Extract all duplicates, for visual inspection. Note that it also
contains the first occurrence of future duplicates, unlike
[`duplicated()`](https://rdrr.io/r/base/duplicated.html) or
[`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)).
Also contains an additional column reporting the number of missing
values for that row, to help in the decision-making when selecting which
duplicates to keep.

## Usage

``` r
data_duplicated(
  data,
  select = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame.

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

A dataframe, containing all duplicates.

## See also

[`data_unique()`](https://easystats.github.io/datawizard/reference/data_unique.md)

## Examples

``` r
df1 <- data.frame(
  id = c(1, 2, 3, 1, 3),
  year = c(2022, 2022, 2022, 2022, 2000),
  item1 = c(NA, 1, 1, 2, 3),
  item2 = c(NA, 1, 1, 2, 3),
  item3 = c(NA, 1, 1, 2, 3)
)

data_duplicated(df1, select = "id")
#>   Row id year item1 item2 item3 count_na
#> 1   1  1 2022    NA    NA    NA        3
#> 4   4  1 2022     2     2     2        0
#> 3   3  3 2022     1     1     1        0
#> 5   5  3 2000     3     3     3        0

data_duplicated(df1, select = c("id", "year"))
#>   Row id year item1 item2 item3 count_na
#> 1   1  1 2022    NA    NA    NA        3
#> 4   4  1 2022     2     2     2        0

# Filter to exclude duplicates
df2 <- df1[-c(1, 5), ]
df2
#>   id year item1 item2 item3
#> 2  2 2022     1     1     1
#> 3  3 2022     1     1     1
#> 4  1 2022     2     2     2
```
