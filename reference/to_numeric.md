# Convert data to numeric

Convert data to numeric by converting characters to factors and factors
to either numeric levels or dummy variables. The "counterpart" to
convert variables into factors is
[`to_factor()`](https://easystats.github.io/datawizard/reference/to_factor.md).

## Usage

``` r
to_numeric(x, ...)

# S3 method for class 'data.frame'
to_numeric(
  x,
  select = NULL,
  exclude = NULL,
  dummy_factors = FALSE,
  preserve_levels = FALSE,
  lowest = NULL,
  append = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A data frame, factor or vector.

- ...:

  Arguments passed to or from other methods.

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

- dummy_factors:

  Transform factors to dummy factors (all factor levels as different
  columns filled with a binary 0-1 value).

- preserve_levels:

  Logical, only applies if `x` is a factor. If `TRUE`, and `x` has
  numeric factor levels, these will be converted into the related
  numeric values. If this is not possible, the converted numeric values
  will start from 1 to number of levels.

- lowest:

  Numeric, indicating the lowest (minimum) value when converting factors
  or character vectors to numeric values.

- append:

  Logical or string. If `TRUE`, recoded or converted variables get new
  column names and are appended (column bind) to `x`, thus returning
  both the original and the recoded variables. The new columns get a
  suffix, based on the calling function: `"_r"` for recode functions,
  `"_n"` for `to_numeric()`, `"_f"` for
  [`to_factor()`](https://easystats.github.io/datawizard/reference/to_factor.md),
  or `"_s"` for
  [`slide()`](https://easystats.github.io/datawizard/reference/slide.md).
  If `append=FALSE`, original variables in `x` will be overwritten by
  their recoded versions. If a character value, recoded variables are
  appended with new column names (using the defined suffix) to the
  original data frame.

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

A data frame of numeric variables.

## Note

When factors should be converted into multiple "binary" dummies, i.e.
each factor level is converted into a separate column filled with a
binary 0-1 value, set `dummy_factors = TRUE`. If you want to preserve
the original factor levels (in case these represent numeric values), use
`preserve_levels = TRUE`.

## Selection of variables - `select` argument

For most functions that have a `select` argument the complete input data
frame is returned, even when `select` only selects a range of variables.
However, for `to_numeric()`, factors might be converted into dummies,
thus, the number of variables of the returned data frame no longer match
the input data frame. Hence, when `select` is used, *only* those
variables (or their dummies) specified in `select` will be returned. Use
`append=TRUE` to also include the original variables in the returned
data frame.

## Examples

``` r
to_numeric(head(ToothGrowth))
#>    len supp dose
#> 1  4.2    2  0.5
#> 2 11.5    2  0.5
#> 3  7.3    2  0.5
#> 4  5.8    2  0.5
#> 5  6.4    2  0.5
#> 6 10.0    2  0.5
to_numeric(head(ToothGrowth), dummy_factors = TRUE)
#>    len supp.OJ supp.VC dose
#> 1  4.2       0       1  0.5
#> 2 11.5       0       1  0.5
#> 3  7.3       0       1  0.5
#> 4  5.8       0       1  0.5
#> 5  6.4       0       1  0.5
#> 6 10.0       0       1  0.5

# factors
x <- as.factor(mtcars$gear)
to_numeric(x)
#>  [1] 2 2 2 1 1 1 1 2 2 2 2 1 1 1 1 1 1 2 2 2 1 1 1 1 1 2 3 3 3 3 3 2
to_numeric(x, preserve_levels = TRUE)
#>  [1] 4 4 4 3 3 3 3 4 4 4 4 3 3 3 3 3 3 4 4 4 3 3 3 3 3 4 5 5 5 5 5 4
# same as:
coerce_to_numeric(x)
#>  [1] 4 4 4 3 3 3 3 4 4 4 4 3 3 3 3 3 3 4 4 4 3 3 3 3 3 4 5 5 5 5 5 4
```
