# Find or get columns in a data frame based on search patterns

`extract_column_names()` returns column names from a data set that match
a certain search pattern, while `data_select()` returns the found data.

## Usage

``` r
data_select(
  data,
  select = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)

extract_column_names(
  data,
  select = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)

find_columns(
  data,
  select = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
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

  - for some functions, like `data_select()` or
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

- verbose:

  Toggle warnings.

- ...:

  Arguments passed down to other functions. Mostly not used yet.

## Value

`extract_column_names()` returns a character vector with column names
that matched the pattern in `select` and `exclude`, or `NULL` if no
matching column name was found. `data_select()` returns a data frame
with matching columns.

## Details

Specifically for `data_select()`, `select` can also be a named character
vector. In this case, the names are used to rename the columns in the
output data frame. See 'Examples'.

Note that it is possible to either pass an entire select helper or only
the pattern inside a select helper as a function argument:

    foo <- function(data, pattern) {
      extract_column_names(data, select = starts_with(pattern))
    }
    foo(iris, pattern = "Sep")

    foo2 <- function(data, pattern) {
      extract_column_names(data, select = pattern)
    }
    foo2(iris, pattern = starts_with("Sep"))

This means that it is also possible to use loop values as arguments or
patterns:

    for (i in c("Sepal", "Sp")) {
      head(iris) |>
        extract_column_names(select = starts_with(i)) |>
        print()
    }

However, this behavior is limited to a "single-level function". It will
not work in nested functions, like below:

    inner <- function(data, arg) {
      extract_column_names(data, select = arg)
    }
    outer <- function(data, arg) {
      inner(data, starts_with(arg))
    }
    outer(iris, "Sep")

In this case, it is better to pass the whole select helper as the
argument of [`outer()`](https://rdrr.io/r/base/outer.html):

    outer <- function(data, arg) {
      inner(data, arg)
    }
    outer(iris, starts_with("Sep"))

## See also

- Add a prefix or suffix to column names:
  [`data_addprefix()`](https://easystats.github.io/datawizard/reference/data_prefix_suffix.md),
  [`data_addsuffix()`](https://easystats.github.io/datawizard/reference/data_prefix_suffix.md)

- Functions to reorder or remove columns:
  [`data_reorder()`](https://easystats.github.io/datawizard/reference/data_relocate.md),
  [`data_relocate()`](https://easystats.github.io/datawizard/reference/data_relocate.md),
  [`data_remove()`](https://easystats.github.io/datawizard/reference/data_relocate.md)

- Functions to reshape, pivot or rotate data frames:
  [`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md),
  [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md),
  [`data_rotate()`](https://easystats.github.io/datawizard/reference/data_rotate.md)

- Functions to recode data:
  [`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md),
  [`reverse()`](https://easystats.github.io/datawizard/reference/reverse.md),
  [`categorize()`](https://easystats.github.io/datawizard/reference/categorize.md),
  [`recode_values()`](https://easystats.github.io/datawizard/reference/recode_values.md),
  [`slide()`](https://easystats.github.io/datawizard/reference/slide.md)

- Functions to standardize, normalize, rank-transform:
  [`center()`](https://easystats.github.io/datawizard/reference/center.md),
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md),
  [`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md),
  [`ranktransform()`](https://easystats.github.io/datawizard/reference/ranktransform.md),
  [`winsorize()`](https://easystats.github.io/datawizard/reference/winsorize.md)

- Split and merge data frames:
  [`data_partition()`](https://easystats.github.io/datawizard/reference/data_partition.md),
  [`data_merge()`](https://easystats.github.io/datawizard/reference/data_merge.md)

- Functions to find or select columns: `data_select()`,
  `extract_column_names()`

- Functions to filter rows:
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md),
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)

## Examples

``` r
# Find column names by pattern
extract_column_names(iris, starts_with("Sepal"))
#> [1] "Sepal.Length" "Sepal.Width" 
extract_column_names(iris, ends_with("Width"))
#> [1] "Sepal.Width" "Petal.Width"
extract_column_names(iris, regex("\\."))
#> [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
extract_column_names(iris, c("Petal.Width", "Sepal.Length"))
#> [1] "Petal.Width"  "Sepal.Length"

# starts with "Sepal", but not allowed to end with "width"
extract_column_names(iris, starts_with("Sepal"), exclude = contains("Width"))
#> [1] "Sepal.Length"

# find numeric with mean > 3.5
numeric_mean_35 <- function(x) is.numeric(x) && mean(x, na.rm = TRUE) > 3.5
extract_column_names(iris, numeric_mean_35)
#> [1] "Sepal.Length" "Petal.Length"

# find column names, using range
extract_column_names(mtcars, c(cyl:hp, wt))
#> [1] "cyl"  "disp" "hp"   "wt"  

# find range of column names by range, using character vector
extract_column_names(mtcars, c("cyl:hp", "wt"))
#> [1] "cyl"  "disp" "hp"   "wt"  

# rename returned columns for "data_select()"
head(data_select(mtcars, c(`Miles per Gallon` = "mpg", Cylinders = "cyl")))
#>                   Miles per Gallon Cylinders
#> Mazda RX4                     21.0         6
#> Mazda RX4 Wag                 21.0         6
#> Datsun 710                    22.8         4
#> Hornet 4 Drive                21.4         6
#> Hornet Sportabout             18.7         8
#> Valiant                       18.1         6
```
