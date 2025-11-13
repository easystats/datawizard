# Convert value labels into factor levels

Convert value labels into factor levels

## Usage

``` r
labels_to_levels(x, ...)

# S3 method for class 'factor'
labels_to_levels(x, verbose = TRUE, ...)

# S3 method for class 'data.frame'
labels_to_levels(
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

  A data frame or factor. Other variable types (e.g. numerics) are not
  allowed.

- ...:

  Currently not used.

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
  `"_f"` for
  [`to_factor()`](https://easystats.github.io/datawizard/reference/to_factor.md),
  or `"_s"` for
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

`x`, where for all factors former levels are replaced by their value
labels.

## Details

`labels_to_levels()` allows to use value labels of factors as their
levels.

## Examples

``` r
data(efc)
# create factor
x <- as.factor(efc$c172code)
# add value labels - these are not factor levels yet
x <- assign_labels(x, values = c(`1` = "low", `2` = "mid", `3` = "high"))
levels(x)
#> [1] "1" "2" "3"
data_tabulate(x)
#> x <categorical>
#> # total N=100 valid N=90
#> 
#> Value |  N | Raw % | Valid % | Cumulative %
#> ------+----+-------+---------+-------------
#> 1     |  8 |     8 |    8.89 |         8.89
#> 2     | 66 |    66 |   73.33 |        82.22
#> 3     | 16 |    16 |   17.78 |       100.00
#> <NA>  | 10 |    10 |    <NA> |         <NA>

x <- labels_to_levels(x)
levels(x)
#> [1] "low"  "mid"  "high"
data_tabulate(x)
#> x <categorical>
#> # total N=100 valid N=90
#> 
#> Value |  N | Raw % | Valid % | Cumulative %
#> ------+----+-------+---------+-------------
#> low   |  8 |     8 |    8.89 |         8.89
#> mid   | 66 |    66 |   73.33 |        82.22
#> high  | 16 |    16 |   17.78 |       100.00
#> <NA>  | 10 |    10 |    <NA> |         <NA>
```
