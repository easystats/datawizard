# Separate single variable into multiple variables

Separates a single variable into multiple new variables.

## Usage

``` r
data_separate(
  data,
  select = NULL,
  new_columns = NULL,
  separator = "[^[:alnum:]]+",
  guess_columns = NULL,
  merge_multiple = FALSE,
  merge_separator = "",
  fill = "right",
  extra = "drop_right",
  convert_na = TRUE,
  exclude = NULL,
  append = FALSE,
  ignore_case = FALSE,
  verbose = TRUE,
  regex = FALSE,
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

- new_columns:

  The names of the new columns, as character vector. If more than one
  variable was selected (in `select`), the new names are prefixed with
  the name of the original column. `new_columns` can also be a list of
  (named) character vectors when multiple variables should be separated.
  See 'Examples'.

- separator:

  Separator between columns. Can be a character vector, which is then
  treated as regular expression, or a numeric vector that indicates at
  which positions the string values will be split.

- guess_columns:

  If `new_columns` is not given, the required number of new columns is
  guessed based on the results of value splitting. For example, if a
  variable is split into three new columns, this will be considered as
  the required number of new columns, and columns are named `"split_1"`,
  `"split_2"` and `"split_3"`. When values from a variable are split
  into different amount of new columns, the `guess_column` can be either
  `"mode"` (number of new columns is based on the most common number of
  splits), `"min"` or `"max"` to use the minimum resp. maximum number of
  possible splits as required number of columns.

- merge_multiple:

  Logical, if `TRUE` and more than one variable is selected for
  separating, new columns can be merged. Value pairs of all split
  variables are merged.

- merge_separator:

  Separator string when `merge_multiple = TRUE`. Defines the string that
  is used to merge values together.

- fill:

  How to deal with values that return fewer new columns after splitting?
  Can be `"left"` (fill missing columns from the left with `NA`),
  `"right"` (fill missing columns from the right with `NA`) or
  `"value_left"` or `"value_right"` to fill missing columns from left or
  right with the left-most or right-most values.

- extra:

  How to deal with values that return too many new columns after
  splitting? Can be `"drop_left"` or `"drop_right"` to drop the
  left-most or right-most values, or `"merge_left"` or `"merge_right"`
  to merge the left- or right-most value together, and keeping all
  remaining values as is.

- convert_na:

  Logical, if `TRUE`, character `"NA"` values are converted into real
  `NA` values.

- exclude:

  See `select`, however, column names matched by the pattern from
  `exclude` will be excluded instead of selected. If `NULL` (the
  default), excludes no columns.

- append:

  Logical, if `FALSE` (default), removes original columns that were
  separated. If `TRUE`, all columns are preserved and the new columns
  are appended to the data frame.

- ignore_case:

  Logical, if `TRUE` and when one of the select-helpers or a regular
  expression is used in `select`, ignores lower/upper case in the search
  pattern when matching against variable names.

- verbose:

  Toggle warnings.

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

- ...:

  Currently not used.

## Value

A data frame with the newly created variable(s), or - when
`append = TRUE` - `data` including new variables.

## See also

[`data_unite()`](https://easystats.github.io/datawizard/reference/data_unite.md)

## Examples

``` r
# simple case
d <- data.frame(
  x = c("1.a.6", "2.b.7", "3.c.8"),
  stringsAsFactors = FALSE
)
d
#>       x
#> 1 1.a.6
#> 2 2.b.7
#> 3 3.c.8
data_separate(d, new_columns = c("a", "b", "c"))
#>   a b c
#> 1 1 a 6
#> 2 2 b 7
#> 3 3 c 8

# guess number of columns
d <- data.frame(
  x = c("1.a.6", NA, "2.b.6.7", "3.c", "x.y.z"),
  stringsAsFactors = FALSE
)
d
#>         x
#> 1   1.a.6
#> 2    <NA>
#> 3 2.b.6.7
#> 4     3.c
#> 5   x.y.z
data_separate(d, guess_columns = "mode")
#> Column `x` had different number of values after splitting. Variable was
#>   split into 3 columns.
#> `x` returned more columns than expected after splitting. Right-most
#>   columns have been dropped.
#> `x`returned fewer columns than expected after splitting. Right-most
#>   columns were filled with `NA`.
#>    x_1  x_2  x_3
#> 1    1    a    6
#> 2 <NA> <NA> <NA>
#> 3    2    b    6
#> 4    3    c <NA>
#> 5    x    y    z

data_separate(d, guess_columns = "max")
#> Column `x` had different number of values after splitting. Variable was
#>   split into 4 columns.
#> `x`returned fewer columns than expected after splitting. Right-most
#>   columns were filled with `NA`.
#>    x_1  x_2  x_3  x_4
#> 1    1    a    6 <NA>
#> 2 <NA> <NA> <NA> <NA>
#> 3    2    b    6    7
#> 4    3    c <NA> <NA>
#> 5    x    y    z <NA>

# drop left-most column
data_separate(d, guess_columns = "mode", extra = "drop_left")
#> Column `x` had different number of values after splitting. Variable was
#>   split into 3 columns.
#> `x` returned more columns than expected after splitting. Left-most
#>   columns have been dropped.
#> `x`returned fewer columns than expected after splitting. Right-most
#>   columns were filled with `NA`.
#>    x_1  x_2  x_3
#> 1    1    a    6
#> 2 <NA> <NA> <NA>
#> 3    b    6    7
#> 4    3    c <NA>
#> 5    x    y    z

# merge right-most column
data_separate(d, guess_columns = "mode", extra = "merge_right")
#> Column `x` had different number of values after splitting. Variable was
#>   split into 3 columns.
#> `x` returned more columns than expected after splitting. Right-most
#>   columns have been merged together.
#> `x`returned fewer columns than expected after splitting. Right-most
#>   columns were filled with `NA`.
#>    x_1  x_2  x_3
#> 1    1    a    6
#> 2 <NA> <NA> <NA>
#> 3    2    b  6 7
#> 4    3    c <NA>
#> 5    x    y    z

# fill columns with fewer values with left-most values
data_separate(d, guess_columns = "mode", fill = "value_left")
#> Column `x` had different number of values after splitting. Variable was
#>   split into 3 columns.
#> `x` returned more columns than expected after splitting. Right-most
#>   columns have been dropped.
#> `x`returned fewer columns than expected after splitting. Left-most
#>   columns were filled with first value.
#>    x_1  x_2  x_3
#> 1    1    a    6
#> 2 <NA> <NA> <NA>
#> 3    2    b    6
#> 4    3    3    c
#> 5    x    y    z

# fill and merge
data_separate(
  d,
  guess_columns = "mode",
  fill = "value_left",
  extra = "merge_right"
)
#> Column `x` had different number of values after splitting. Variable was
#>   split into 3 columns.
#> `x` returned more columns than expected after splitting. Right-most
#>   columns have been merged together.
#> `x`returned fewer columns than expected after splitting. Left-most
#>   columns were filled with first value.
#>    x_1  x_2  x_3
#> 1    1    a    6
#> 2 <NA> <NA> <NA>
#> 3    2    b  6 7
#> 4    3    3    c
#> 5    x    y    z

# multiple columns to split
d <- data.frame(
  x = c("1.a.6", "2.b.7", "3.c.8"),
  y = c("x.y.z", "10.11.12", "m.n.o"),
  stringsAsFactors = FALSE
)
d
#>       x        y
#> 1 1.a.6    x.y.z
#> 2 2.b.7 10.11.12
#> 3 3.c.8    m.n.o
# split two columns, default column names
data_separate(d, guess_columns = "mode")
#>   x_1 x_2 x_3 y_1 y_2 y_3
#> 1   1   a   6   x   y   z
#> 2   2   b   7  10  11  12
#> 3   3   c   8   m   n   o

# split into new named columns, repeating column names
data_separate(d, new_columns = c("a", "b", "c"))
#>   x_a x_b x_c y_a y_b y_c
#> 1   1   a   6   x   y   z
#> 2   2   b   7  10  11  12
#> 3   3   c   8   m   n   o

# split selected variable new columns
data_separate(d, select = "y", new_columns = c("a", "b", "c"))
#>       x  a  b  c
#> 1 1.a.6  x  y  z
#> 2 2.b.7 10 11 12
#> 3 3.c.8  m  n  o

# merge multiple split columns
data_separate(
  d,
  new_columns = c("a", "b", "c"),
  merge_multiple = TRUE
)
#>     a   b   c
#> 1  1x  ay  6z
#> 2 210 b11 712
#> 3  3m  cn  8o

# merge multiple split columns
data_separate(
  d,
  new_columns = c("a", "b", "c"),
  merge_multiple = TRUE,
  merge_separator = "-"
)
#>      a    b    c
#> 1  1-x  a-y  6-z
#> 2 2-10 b-11 7-12
#> 3  3-m  c-n  8-o

# separate multiple columns, give proper column names
d_sep <- data.frame(
  x = c("1.a.6", "2.b.7.d", "3.c.8", "5.j"),
  y = c("m.n.99.22", "77.f.g.34", "44.9", NA),
  stringsAsFactors = FALSE
)

data_separate(
  d_sep,
  select = c("x", "y"),
  new_columns = list(
    x = c("A", "B", "C"), # separate "x" into three columns
    y = c("EE", "FF", "GG", "HH") # separate "y" into four columns
  ),
  verbose = FALSE
)
#>   A B    C   EE   FF   GG   HH
#> 1 1 a    6    m    n   99   22
#> 2 2 b    7   77    f    g   34
#> 3 3 c    8   44    9 <NA> <NA>
#> 4 5 j <NA> <NA> <NA> <NA> <NA>
```
