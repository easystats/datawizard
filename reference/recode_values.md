# Recode old values of variables into new values

This functions recodes old values into new values and can be used to to
recode numeric or character vectors, or factors.

## Usage

``` r
recode_values(x, ...)

# S3 method for class 'numeric'
recode_values(
  x,
  recode = NULL,
  default = NULL,
  preserve_na = TRUE,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
recode_values(
  x,
  select = NULL,
  exclude = NULL,
  recode = NULL,
  default = NULL,
  preserve_na = TRUE,
  append = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A data frame, numeric or character vector, or factor.

- ...:

  not used.

- recode:

  A list of named vectors, which indicate the recode pairs. The *names*
  of the list-elements (i.e. the left-hand side) represent the *new*
  values, while the values of the list-elements indicate the original
  (old) values that should be replaced. When recoding numeric vectors,
  element names have to be surrounded in backticks. For example,
  `` recode=list(`0`=1) `` would recode all `1` into `0` in a numeric
  vector. See also 'Examples' and 'Details'.

- default:

  Defines the default value for all values that have no match in the
  recode-pairs. Note that, if `preserve_na=FALSE`, missing values (`NA`)
  are also captured by the `default` argument, and thus will also be
  recoded into the specified value. See 'Examples' and 'Details'.

- preserve_na:

  Logical, if `TRUE`, `NA` (missing values) are preserved. This
  overrides any other arguments, including `default`. Hence, if
  `preserve_na=TRUE`, `default` will no longer convert `NA` into the
  specified default value.

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

`x`, where old values are replaced by new values.

## Details

This section describes the pattern of the `recode` arguments, which also
provides some shortcuts, in particular when recoding numeric values.

- Single values

  Single values either need to be wrapped in backticks (in case of
  numeric values) or "as is" (for character or factor levels). Example:
  `` recode=list(`0`=1,`1`=2) `` would recode 1 into 0, and 2 into 1.
  For factors or character vectors, an example is:
  `recode=list(x="a",y="b")` (recode "a" into "x" and "b" into "y").

- Multiple values

  Multiple values that should be recoded into a new value can be
  separated with comma. Example:
  `` recode=list(`1`=c(1,4),`2`=c(2,3)) `` would recode the values 1 and
  4 into 1, and 2 and 3 into 2. It is also possible to define the old
  values as a character string, like:
  `` recode=list(`1`="1,4",`2`="2,3") `` For factors or character
  vectors, an example is: `recode=list(x=c("a","b"),y=c("c","d"))`.

- Value range

  Numeric value ranges can be defined using the `:`. Example:
  `` recode=list(`1`=1:3,`2`=4:6) `` would recode all values from 1 to 3
  into 1, and 4 to 6 into 2.

- `min` and `max`

  placeholder to use the minimum or maximum value of the (numeric)
  variable. Useful, e.g., when recoding ranges of values. Example:
  `` recode=list(`1`="min:10",`2`="11:max") ``.

- `default` values

  The `default` argument defines the default value for all values that
  have no match in the recode-pairs. For example,
  `` recode=list(`1`=c(1,2),`2`=c(3,4)), default=9 `` would recode
  values 1 and 2 into 1, 3 and 4 into 2, and all other values into 9. If
  `preserve_na` is set to `FALSE`, `NA` (missing values) will also be
  recoded into the specified default value.

- Reversing and rescaling

  See
  [`reverse()`](https://easystats.github.io/datawizard/reference/reverse.md)
  and
  [`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md).

## Note

You can use `options(data_recode_pattern = "old=new")` to switch the
behaviour of the `recode`-argument, i.e. recode-pairs are now following
the pattern `old values = new values`, e.g. if
`getOption("data_recode_pattern")` is set to `"old=new"`, then
`` recode(`1`=0) `` would recode all 1 into 0. The default for
`` recode(`1`=0) `` is to recode all 0 into 1.

## Selection of variables - the `select` argument

For most functions that have a `select` argument (including this
function), the complete input data frame is returned, even when `select`
only selects a range of variables. That is, the function is only applied
to those variables that have a match in `select`, while all other
variables remain unchanged. In other words: for this function, `select`
will not omit any non-included variables, so that the returned data
frame will include all variables from the input data frame.

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
  `recode_values()`,
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
# numeric ----------
set.seed(123)
x <- sample(c(1:4, NA), 15, TRUE)
table(x, useNA = "always")
#> x
#>    1    2    3    4 <NA> 
#>    2    3    6    2    2 

out <- recode_values(x, list(`0` = 1, `1` = 2:3, `2` = 4))
out
#>  [1]  1  1  1  1  1 NA  2  0  1  1 NA  1  1  0  2
table(out, useNA = "always")
#> out
#>    0    1    2 <NA> 
#>    2    9    2    2 

# to recode NA values, set preserve_na to FALSE
out <- recode_values(
  x,
  list(`0` = 1, `1` = 2:3, `2` = 4, `9` = NA),
  preserve_na = FALSE
)
out
#>  [1] 1 1 1 1 1 9 2 0 1 1 9 1 1 0 2
table(out, useNA = "always")
#> out
#>    0    1    2    9 <NA> 
#>    2    9    2    2    0 

# preserve na ----------
out <- recode_values(x, list(`0` = 1, `1` = 2:3), default = 77)
out
#>  [1]  1  1  1  1  1 NA 77  0  1  1 NA  1  1  0 77
table(out, useNA = "always")
#> out
#>    0    1   77 <NA> 
#>    2    9    2    2 

# recode na into default ----------
out <- recode_values(
  x,
  list(`0` = 1, `1` = 2:3),
  default = 77,
  preserve_na = FALSE
)
out
#>  [1]  1  1  1  1  1 77 77  0  1  1 77  1  1  0 77
table(out, useNA = "always")
#> out
#>    0    1   77 <NA> 
#>    2    9    4    0 


# factors (character vectors are similar) ----------
set.seed(123)
x <- as.factor(sample(c("a", "b", "c"), 15, TRUE))
table(x)
#> x
#> a b c 
#> 2 7 6 

out <- recode_values(x, list(x = "a", y = c("b", "c")))
out
#>  [1] y y y y y y y y y x y y x y y
#> Levels: x y
table(out)
#> out
#>  x  y 
#>  2 13 

out <- recode_values(x, list(x = "a", y = "b", z = "c"))
out
#>  [1] z z z y z y y y z x y y x y z
#> Levels: x y z
table(out)
#> out
#> x y z 
#> 2 7 6 

out <- recode_values(x, list(y = "b,c"), default = 77)
# same as
# recode_values(x, list(y = c("b", "c")), default = 77)
out
#>  [1] y  y  y  y  y  y  y  y  y  77 y  y  77 y  y 
#> Levels: 77 y
table(out)
#> out
#> 77  y 
#>  2 13 


# data frames ----------
set.seed(123)
d <- data.frame(
  x = sample(c(1:4, NA), 12, TRUE),
  y = as.factor(sample(c("a", "b", "c"), 12, TRUE)),
  stringsAsFactors = FALSE
)

recode_values(
  d,
  recode = list(`0` = 1, `1` = 2:3, `2` = 4, x = "a", y = c("b", "c")),
  append = TRUE
)
#>     x y x_r y_r
#> 1   3 c   1   y
#> 2   3 a   1   x
#> 3   2 a   1   x
#> 4   2 a   1   x
#> 5   3 a   1   x
#> 6  NA c  NA   y
#> 7   4 b   2   y
#> 8   1 c   0   y
#> 9   2 b   1   y
#> 10  3 a   1   x
#> 11 NA b  NA   y
#> 12  3 c   1   y


# switch recode pattern to "old=new" ----------
options(data_recode_pattern = "old=new")

# numeric
set.seed(123)
x <- sample(c(1:4, NA), 15, TRUE)
table(x, useNA = "always")
#> x
#>    1    2    3    4 <NA> 
#>    2    3    6    2    2 

out <- recode_values(x, list(`1` = 0, `2:3` = 1, `4` = 2))
table(out, useNA = "always")
#> out
#>    0    1    2 <NA> 
#>    2    9    2    2 

# factors (character vectors are similar)
set.seed(123)
x <- as.factor(sample(c("a", "b", "c"), 15, TRUE))
table(x)
#> x
#> a b c 
#> 2 7 6 

out <- recode_values(x, list(a = "x", `b, c` = "y"))
table(out)
#> out
#>  x  y 
#>  2 13 

# reset options
options(data_recode_pattern = NULL)
```
