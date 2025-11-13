# Peek at values and type of variables in a data frame

This function creates a table a data frame, showing all column names,
variable types and the first values (as many as fit into the screen).

## Usage

``` r
data_peek(x, ...)

# S3 method for class 'data.frame'
data_peek(
  x,
  select = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  width = NULL,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A data frame.

- ...:

  not used.

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

- width:

  Maximum width of line length to display. If `NULL`, width will be
  determined using `options()$width`.

- verbose:

  Toggle warnings.

## Value

A data frame with three columns, containing information about the name,
type and first values of the input data frame.

## Note

To show only specific or a limited number of variables, use the `select`
argument, e.g. `select = 1:5` to show only the first five variables.

## Examples

``` r
data(efc)
data_peek(efc)
#> Data frame with 100 rows and 5 variables
#> 
#> Variable | Type    | Values                                           
#> ----------------------------------------------------------------------
#> c12hour  | numeric | 16, 148, 70, NA, 168, 16, 161, 110, 28, 40, ...  
#> e16sex   | numeric | 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 2, ... 
#> e42dep   | factor  | 3, 3, 3, NA, 4, 4, 4, 4, 4, 4, 4, 3, 4, 3, 3, ...
#> c172code | numeric | 2, 2, 1, 2, 2, 2, 2, 2, NA, 2, 2, 2, 3, 1, 3, ...
#> neg_c_7  | numeric | 12, 20, 11, 10, 12, 19, 15, 11, 15, 10, 28, ...  
# show variables two to four
data_peek(efc, select = 2:4)
#> Data frame with 100 rows and 5 variables
#> 
#> Variable | Type    | Values                                           
#> ----------------------------------------------------------------------
#> e16sex   | numeric | 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 1, 2, 2, 2, 2, ... 
#> e42dep   | factor  | 3, 3, 3, NA, 4, 4, 4, 4, 4, 4, 4, 3, 4, 3, 3, ...
#> c172code | numeric | 2, 2, 1, 2, 2, 2, 2, 2, NA, 2, 2, 2, 3, 1, 3, ...
```
