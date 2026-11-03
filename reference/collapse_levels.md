# Collapse sparse levels of categorical variables

Merges sparse levels of factors and character vectors. A level is sparse
if it has fewer than `min_n` values or less than `min_prop` of the
non-missing values.

## Usage

``` r
collapse_levels(x, ...)

# S3 method for class 'factor'
collapse_levels(
  x,
  min_prop = 0.05,
  min_n = 0,
  other = NULL,
  sep = "/",
  verbose = TRUE,
  ...
)

# S3 method for class 'character'
collapse_levels(
  x,
  min_prop = 0.05,
  min_n = 0,
  other = NULL,
  sep = "/",
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
collapse_levels(
  x,
  select = NULL,
  exclude = NULL,
  min_prop = 0.05,
  min_n = 0,
  other = NULL,
  sep = "/",
  pool = FALSE,
  append = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A factor, a character vector, or a data frame.

- ...:

  Not used.

- min_prop:

  A level with a smaller share of the non-missing values is sparse. A
  number between 0 and 1.

- min_n:

  A level with fewer values is sparse. A number of 0 or more.

- other:

  Name of the merged level of a nominal variable. If `NULL`, the merged
  levels are joined with `sep`. Ignored for ordered factors. If levels
  merge, it is an error if the name is already a used level.

- sep:

  String that joins the labels of merged levels.

- verbose:

  Toggle warnings and messages.

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

- pool:

  Logical. If `TRUE`, the counts are summed over the selected columns,
  and all of them get the same merges and levels. Use this for the items
  of a scale. Ordered factors must have the same levels in the same
  order and cannot be mixed with other factors or character vectors.
  Only the summed counts decide which levels are sparse, so a level that
  is sparse in only one column can stay, and the selected columns change
  the result. If no level is sparse in the summed counts, nothing
  merges, and there is no warning. See the examples.

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

`x`, with sparse levels merged. In data frames, only factors and
character vectors change.

## Details

Nominal variables (unordered factors and character vectors): all sparse
levels merge into one last level. If that level is still sparse, the
smallest remaining level joins it until it is not, so a large level can
lose its own name. If all levels would merge into one, `x` is returned
unchanged with a warning.

Ordered factors: the smallest level merges with the adjacent level that
has fewer values, until no level is sparse or two levels are left, with
a warning if a level is still sparse. On equal counts, it merges toward
the middle of the scale, and from the exact middle to the lower level.

If `x` changes, levels unused in all selected columns are dropped, and a
factor loses the attribute `labels` and a contrast matrix. `NA` values
and an `NA` level (see [`addNA()`](https://rdrr.io/r/base/factor.html))
are not counted or merged. A grouped data frame is counted as a whole,
and its grouping columns stay unchanged.

## Selection of variables - the `select` argument

For most functions that have a `select` argument (including this
function), the complete input data frame is returned, even when `select`
only selects a range of variables. That is, the function is only applied
to those variables that have a match in `select`, while all other
variables remain unchanged. In other words: for this function, `select`
will not omit any non-included variables, so that the returned data
frame will include all variables from the input data frame.

## See also

[`recode_values()`](https://easystats.github.io/datawizard/reference/recode_values.md)
and
[`recode_into()`](https://easystats.github.io/datawizard/reference/recode_into.md)
to recode by hand, and
[`winsorize()`](https://easystats.github.io/datawizard/reference/winsorize.md)
to limit extreme values of numeric variables.

## Examples

``` r
x <- factor(rep(c("a", "b", "c", "d"), c(80, 14, 3, 3)))
table(collapse_levels(x))
#> 
#>   a   b c/d 
#>  80  14   6 
table(collapse_levels(x, other = "Other"))
#> 
#>     a     b Other 
#>    80    14     6 

# ordered factors merge with a neighbor
x <- factor(
  rep(c("low", "mid", "high"), c(3, 50, 47)),
  levels = c("low", "mid", "high"),
  ordered = TRUE
)
table(collapse_levels(x))
#> 
#> low/mid    high 
#>      53      47 

# the items of a scale get the same merges
d <- data.frame(
  item1 = ordered(rep(1:3, c(2, 8, 90))),
  item2 = ordered(rep(1:3, c(6, 88, 6)))
)
# level 1 has 8 of 200 values (4%), so it is sparse and merges
table(collapse_levels(d, pool = TRUE)$item2)
#> 
#> 1/2   3 
#>  94   6 

# the selected columns change the pooled counts: with a third column, level 1
# has 18 of 300 values (6%), so it is not sparse and nothing merges
d$no_item <- ordered(rep(1:3, c(10, 82, 8)))
table(collapse_levels(d, pool = TRUE)$item2)
#> 
#>  1  2  3 
#>  6 88  6 

# select only the items of the scale
table(collapse_levels(d, select = c("item1", "item2"), pool = TRUE)$item2)
#> 
#> 1/2   3 
#>  94   6 
```
