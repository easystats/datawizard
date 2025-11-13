# Replace missing values in a variable or a data frame.

Replace missing values in a variable or a data frame.

## Usage

``` r
convert_na_to(x, ...)

# S3 method for class 'numeric'
convert_na_to(x, replacement = NULL, verbose = TRUE, ...)

# S3 method for class 'character'
convert_na_to(x, replacement = NULL, verbose = TRUE, ...)

# S3 method for class 'data.frame'
convert_na_to(
  x,
  select = NULL,
  exclude = NULL,
  replacement = NULL,
  replace_num = replacement,
  replace_char = replacement,
  replace_fac = replacement,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A numeric, factor, or character vector, or a data frame.

- ...:

  Not used.

- replacement:

  Numeric or character value that will be used to replace `NA`.

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

- replace_num:

  Value to replace `NA` when variable is of type numeric.

- replace_char:

  Value to replace `NA` when variable is of type character.

- replace_fac:

  Value to replace `NA` when variable is of type factor.

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

`x`, where `NA` values are replaced by `replacement`.

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
# Convert NA to 0 in a numeric vector
convert_na_to(
  c(9, 3, NA, 2, 3, 1, NA, 8),
  replacement = 0
)
#> [1] 9 3 0 2 3 1 0 8

# Convert NA to "missing" in a character vector
convert_na_to(
  c("a", NA, "d", "z", NA, "t"),
  replacement = "missing"
)
#> [1] "a"       "missing" "d"       "z"       "missing" "t"      

### For data frames

test_df <- data.frame(
  x = c(1, 2, NA),
  x2 = c(4, 5, NA),
  y = c("a", "b", NA)
)

# Convert all NA to 0 in numeric variables, and all NA to "missing" in
# character variables
convert_na_to(
  test_df,
  replace_num = 0,
  replace_char = "missing"
)
#>   x x2       y
#> 1 1  4       a
#> 2 2  5       b
#> 3 0  0 missing

# Convert a specific variable in the data frame
convert_na_to(
  test_df,
  replace_num = 0,
  replace_char = "missing",
  select = "x"
)
#>   x x2    y
#> 1 1  4    a
#> 2 2  5    b
#> 3 0 NA <NA>

# Convert all variables starting with "x"
convert_na_to(
  test_df,
  replace_num = 0,
  replace_char = "missing",
  select = starts_with("x")
)
#>   x x2    y
#> 1 1  4    a
#> 2 2  5    b
#> 3 0  0 <NA>

# Convert NA to 1 in variable 'x2' and to 0 in all other numeric
# variables
convert_na_to(
  test_df,
  replace_num = 0,
  select = list(x2 = 1)
)
#>   x x2    y
#> 1 1  4    a
#> 2 2  5    b
#> 3 0  1 <NA>
```
