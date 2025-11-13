# Relocate (reorder) columns of a data frame

`data_relocate()` will reorder columns to specific positions, indicated
by `before` or `after`. `data_reorder()` will instead move selected
columns to the beginning of a data frame. Finally, `data_remove()`
removes columns from a data frame. All functions support select-helpers
that allow flexible specification of a search pattern to find matching
columns, which should be reordered or removed.

## Usage

``` r
data_relocate(
  data,
  select,
  before = NULL,
  after = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)

data_reorder(
  data,
  select,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)

data_remove(
  data,
  select = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = FALSE,
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

- before, after:

  Destination of columns. Supplying neither will move columns to the
  left-hand side; specifying both is an error. Can be a character
  vector, indicating the name of the destination column, or a numeric
  value, indicating the index number of the destination column. If `-1`,
  will be added before or after the last column.

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

- exclude:

  See `select`, however, column names matched by the pattern from
  `exclude` will be excluded instead of selected. If `NULL` (the
  default), excludes no columns.

## Value

A data frame with reordered columns.

## See also

- Add a prefix or suffix to column names:
  [`data_addprefix()`](https://easystats.github.io/datawizard/reference/data_prefix_suffix.md),
  [`data_addsuffix()`](https://easystats.github.io/datawizard/reference/data_prefix_suffix.md)

- Functions to reorder or remove columns: `data_reorder()`,
  `data_relocate()`, `data_remove()`

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

- Functions to find or select columns:
  [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md),
  [`extract_column_names()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)

- Functions to filter rows:
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md),
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)

## Examples

``` r
# Reorder columns
head(data_relocate(iris, select = "Species", before = "Sepal.Length"))
#>   Species Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1  setosa          5.1         3.5          1.4         0.2
#> 2  setosa          4.9         3.0          1.4         0.2
#> 3  setosa          4.7         3.2          1.3         0.2
#> 4  setosa          4.6         3.1          1.5         0.2
#> 5  setosa          5.0         3.6          1.4         0.2
#> 6  setosa          5.4         3.9          1.7         0.4
head(data_relocate(iris, select = "Species", before = "Sepal.Width"))
#>   Sepal.Length Species Sepal.Width Petal.Length Petal.Width
#> 1          5.1  setosa         3.5          1.4         0.2
#> 2          4.9  setosa         3.0          1.4         0.2
#> 3          4.7  setosa         3.2          1.3         0.2
#> 4          4.6  setosa         3.1          1.5         0.2
#> 5          5.0  setosa         3.6          1.4         0.2
#> 6          5.4  setosa         3.9          1.7         0.4
head(data_relocate(iris, select = "Sepal.Width", after = "Species"))
#>   Sepal.Length Petal.Length Petal.Width Species Sepal.Width
#> 1          5.1          1.4         0.2  setosa         3.5
#> 2          4.9          1.4         0.2  setosa         3.0
#> 3          4.7          1.3         0.2  setosa         3.2
#> 4          4.6          1.5         0.2  setosa         3.1
#> 5          5.0          1.4         0.2  setosa         3.6
#> 6          5.4          1.7         0.4  setosa         3.9
# which is same as
head(data_relocate(iris, select = "Sepal.Width", after = -1))
#>   Sepal.Length Petal.Length Petal.Width Species Sepal.Width
#> 1          5.1          1.4         0.2  setosa         3.5
#> 2          4.9          1.4         0.2  setosa         3.0
#> 3          4.7          1.3         0.2  setosa         3.2
#> 4          4.6          1.5         0.2  setosa         3.1
#> 5          5.0          1.4         0.2  setosa         3.6
#> 6          5.4          1.7         0.4  setosa         3.9

# Reorder multiple columns
head(data_relocate(iris, select = c("Species", "Petal.Length"), after = "Sepal.Width"))
#>   Sepal.Length Sepal.Width Species Petal.Length Petal.Width
#> 1          5.1         3.5  setosa          1.4         0.2
#> 2          4.9         3.0  setosa          1.4         0.2
#> 3          4.7         3.2  setosa          1.3         0.2
#> 4          4.6         3.1  setosa          1.5         0.2
#> 5          5.0         3.6  setosa          1.4         0.2
#> 6          5.4         3.9  setosa          1.7         0.4
# which is same as
head(data_relocate(iris, select = c("Species", "Petal.Length"), after = 2))
#>   Sepal.Length Sepal.Width Species Petal.Length Petal.Width
#> 1          5.1         3.5  setosa          1.4         0.2
#> 2          4.9         3.0  setosa          1.4         0.2
#> 3          4.7         3.2  setosa          1.3         0.2
#> 4          4.6         3.1  setosa          1.5         0.2
#> 5          5.0         3.6  setosa          1.4         0.2
#> 6          5.4         3.9  setosa          1.7         0.4

# Reorder columns
head(data_reorder(iris, c("Species", "Sepal.Length")))
#>   Species Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1  setosa          5.1         3.5          1.4         0.2
#> 2  setosa          4.9         3.0          1.4         0.2
#> 3  setosa          4.7         3.2          1.3         0.2
#> 4  setosa          4.6         3.1          1.5         0.2
#> 5  setosa          5.0         3.6          1.4         0.2
#> 6  setosa          5.4         3.9          1.7         0.4

# Remove columns
head(data_remove(iris, "Sepal.Length"))
#>   Sepal.Width Petal.Length Petal.Width Species
#> 1         3.5          1.4         0.2  setosa
#> 2         3.0          1.4         0.2  setosa
#> 3         3.2          1.3         0.2  setosa
#> 4         3.1          1.5         0.2  setosa
#> 5         3.6          1.4         0.2  setosa
#> 6         3.9          1.7         0.4  setosa
head(data_remove(iris, starts_with("Sepal")))
#>   Petal.Length Petal.Width Species
#> 1          1.4         0.2  setosa
#> 2          1.4         0.2  setosa
#> 3          1.3         0.2  setosa
#> 4          1.5         0.2  setosa
#> 5          1.4         0.2  setosa
#> 6          1.7         0.4  setosa
```
