# Convert non-missing values in a variable into missing values.

Convert non-missing values in a variable into missing values.

## Usage

``` r
convert_to_na(x, ...)

# S3 method for class 'numeric'
convert_to_na(x, na = NULL, verbose = TRUE, ...)

# S3 method for class 'factor'
convert_to_na(x, na = NULL, drop_levels = FALSE, verbose = TRUE, ...)

# S3 method for class 'data.frame'
convert_to_na(
  x,
  select = NULL,
  exclude = NULL,
  na = NULL,
  drop_levels = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A vector, factor or a data frame.

- ...:

  Not used.

- na:

  Numeric, character vector or logical (or a list of numeric, character
  vectors or logicals) with values that should be converted to `NA`.
  Numeric values applied to numeric vectors, character values are used
  for factors, character vectors or date variables, and logical values
  for logical vectors.

- verbose:

  Toggle warnings.

- drop_levels:

  Logical, for factors, when specific levels are replaced by `NA`,
  should unused levels be dropped?

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

## Value

`x`, where all values in `na` are converted to `NA`.

## Examples

``` r
x <- sample(1:6, size = 30, replace = TRUE)
x
#>  [1] 6 4 1 6 6 3 6 5 3 6 2 5 5 3 2 2 2 4 2 2 6 4 4 6 1 6 6 6 3 6
# values 4 and 5 to NA
convert_to_na(x, na = 4:5)
#>  [1]  6 NA  1  6  6  3  6 NA  3  6  2 NA NA  3  2  2  2 NA  2  2  6 NA NA  6  1
#> [26]  6  6  6  3  6

# data frames
set.seed(123)
x <- data.frame(
  a = sample(1:6, size = 20, replace = TRUE),
  b = sample(letters[1:6], size = 20, replace = TRUE),
  c = sample(c(30:33, 99), size = 20, replace = TRUE)
)
# for all numerics, convert 5 to NA. Character/factor will be ignored.
convert_to_na(x, na = 5)
#> Could not convert values into `NA` for a factor or character variable.
#>   To do this, `na` needs to be a character vector, or a list that contains
#>   character vector elements.
#>     a b  c
#> 1   3 a 33
#> 2   6 e 99
#> 3   3 c 99
#> 4   2 b 32
#> 5   2 b 30
#> 6   6 a 31
#> 7   3 f 99
#> 8  NA c 99
#> 9   4 d 33
#> 10  6 f 99
#> 11  6 a 31
#> 12  1 c 30
#> 13  2 e 30
#> 14  3 d 32
#> 15 NA b 30
#> 16  3 e 99
#> 17  3 a 30
#> 18  1 a 31
#> 19  4 b 33
#> 20  1 c 33

# for numerics, 5 to NA, for character/factor, "f" to NA
convert_to_na(x, na = list(6, "f"))
#>     a    b  c
#> 1   3    a 33
#> 2  NA    e 99
#> 3   3    c 99
#> 4   2    b 32
#> 5   2    b 30
#> 6  NA    a 31
#> 7   3 <NA> 99
#> 8   5    c 99
#> 9   4    d 33
#> 10 NA <NA> 99
#> 11 NA    a 31
#> 12  1    c 30
#> 13  2    e 30
#> 14  3    d 32
#> 15  5    b 30
#> 16  3    e 99
#> 17  3    a 30
#> 18  1    a 31
#> 19  4    b 33
#> 20  1    c 33

# select specific variables
convert_to_na(x, select = c("a", "b"), na = list(6, "f"))
#>     a    b  c
#> 1   3    a 33
#> 2  NA    e 99
#> 3   3    c 99
#> 4   2    b 32
#> 5   2    b 30
#> 6  NA    a 31
#> 7   3 <NA> 99
#> 8   5    c 99
#> 9   4    d 33
#> 10 NA <NA> 99
#> 11 NA    a 31
#> 12  1    c 30
#> 13  2    e 30
#> 14  3    d 32
#> 15  5    b 30
#> 16  3    e 99
#> 17  3    a 30
#> 18  1    a 31
#> 19  4    b 33
#> 20  1    c 33
```
