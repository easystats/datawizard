# Row means or sums (optionally with minimum amount of valid values)

This function is similar to the SPSS `MEAN.n` or `SUM.n` function and
computes row means or row sums from a data frame or matrix if at least
`min_valid` values of a row are valid (and not `NA`).

## Usage

``` r
row_means(
  data,
  select = NULL,
  exclude = NULL,
  min_valid = NULL,
  digits = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  remove_na = FALSE,
  verbose = TRUE
)

row_sums(
  data,
  select = NULL,
  exclude = NULL,
  min_valid = NULL,
  digits = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  remove_na = FALSE,
  verbose = TRUE
)
```

## Arguments

- data:

  A data frame with at least two columns, where row means or row sums
  are applied.

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

- min_valid:

  Optional, a numeric value of length 1. May either be

  - a numeric value that indicates the amount of valid values per row to
    calculate the row mean or row sum;

  - or a value between `0` and `1`, indicating a proportion of valid
    values per row to calculate the row mean or row sum (see 'Details').

  - `NULL` (default), in which all cases are considered.

  If a row's sum of valid values is less than `min_valid`, `NA` will be
  returned.

- digits:

  Numeric value indicating the number of decimal places to be used for
  rounding mean values. Negative values are allowed (see 'Details'). By
  default, `digits = NULL` and no rounding is used.

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

- remove_na:

  Logical, if `TRUE` (default), removes missing (`NA`) values before
  calculating row means or row sums. Only applies if `min_valid` is not
  specified.

- verbose:

  Toggle warnings.

## Value

A vector with row means (for `row_means()`) or row sums (for
`row_sums()`) for those rows with at least `n` valid values.

## Details

Rounding to a negative number of `digits` means rounding to a power of
ten, for example `row_means(df, 3, digits = -2)` rounds to the nearest
hundred. For `min_valid`, if not `NULL`, `min_valid` must be a numeric
value from `0` to `ncol(data)`. If a row in the data frame has at least
`min_valid` non-missing values, the row mean or row sum is returned. If
`min_valid` is a non-integer value from 0 to 1, `min_valid` is
considered to indicate the proportion of required non-missing values per
row. E.g., if `min_valid = 0.75`, a row must have at least
`ncol(data) * min_valid` non-missing values for the row mean or row sum
to be calculated. See 'Examples'.

## Examples

``` r
dat <- data.frame(
  c1 = c(1, 2, NA, 4),
  c2 = c(NA, 2, NA, 5),
  c3 = c(NA, 4, NA, NA),
  c4 = c(2, 3, 7, 8)
)

# default, all means are shown, if no NA values are present
row_means(dat)
#> [1]   NA 2.75   NA   NA

# remove all NA before computing row means
row_means(dat, remove_na = TRUE)
#> [1] 1.500000 2.750000 7.000000 5.666667

# needs at least 4 non-missing values per row
row_means(dat, min_valid = 4) # 1 valid return value
#> [1]   NA 2.75   NA   NA
row_sums(dat, min_valid = 4) # 1 valid return value
#> [1] NA 11 NA NA

# needs at least 3 non-missing values per row
row_means(dat, min_valid = 3) # 2 valid return values
#> [1]       NA 2.750000       NA 5.666667

# needs at least 2 non-missing values per row
row_means(dat, min_valid = 2)
#> [1] 1.500000 2.750000       NA 5.666667

# needs at least 1 non-missing value per row, for two selected variables
row_means(dat, select = c("c1", "c3"), min_valid = 1)
#> [1]  1  3 NA  4

# needs at least 50% of non-missing values per row
row_means(dat, min_valid = 0.5) # 3 valid return values
#> [1] 1.500000 2.750000       NA 5.666667
row_sums(dat, min_valid = 0.5)
#> [1]  3 11 NA 17

# needs at least 75% of non-missing values per row
row_means(dat, min_valid = 0.75) # 2 valid return values
#> [1]       NA 2.750000       NA 5.666667
```
