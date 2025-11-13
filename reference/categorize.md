# Recode (or "cut" / "bin") data into groups of values.

This functions divides the range of variables into intervals and recodes
the values inside these intervals according to their related interval.
It is basically a wrapper around base R's
[`cut()`](https://rdrr.io/r/base/cut.html), providing a simplified and
more accessible way to define the interval breaks (cut-off values).

## Usage

``` r
categorize(x, ...)

# S3 method for class 'numeric'
categorize(
  x,
  split = "median",
  n_groups = NULL,
  range = NULL,
  lowest = 1,
  breaks = "exclusive",
  labels = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
categorize(
  x,
  select = NULL,
  exclude = NULL,
  split = "median",
  n_groups = NULL,
  range = NULL,
  lowest = 1,
  breaks = "exclusive",
  labels = NULL,
  append = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A (grouped) data frame, numeric vector or factor.

- ...:

  not used.

- split:

  Character vector, indicating at which breaks to split variables, or
  numeric values with values indicating breaks. If character, may be one
  of `"median"`, `"mean"`, `"quantile"`, `"equal_length"`, or
  `"equal_range"`. `"median"` or `"mean"` will return dichotomous
  variables, split at their mean or median, respectively. `"quantile"`
  and `"equal_length"` will split the variable into `n_groups` groups,
  where each group refers to an interval of a specific range of values.
  Thus, the length of each interval will be based on the number of
  groups. `"equal_range"` also splits the variable into multiple groups,
  however, the length of the interval is given, and the number of
  resulting groups (and hence, the number of breaks) will be determined
  by how many intervals can be generated, based on the full range of the
  variable.

- n_groups:

  If `split` is `"quantile"` or `"equal_length"`, this defines the
  number of requested groups (i.e. resulting number of levels or values)
  for the recoded variable(s). `"quantile"` will define intervals based
  on the distribution of the variable, while `"equal_length"` tries to
  divide the range of the variable into pieces of equal length.

- range:

  If `split = "equal_range"`, this defines the range of values that are
  recoded into a new value.

- lowest:

  Minimum value of the recoded variable(s). If `NULL` (the default), for
  numeric variables, the minimum of the original input is preserved. For
  factors, the default minimum is `1`. For `split = "equal_range"`, the
  default minimum is always `1`, unless specified otherwise in `lowest`.

- breaks:

  Character, indicating whether breaks for categorizing data are
  `"inclusive"` (values indicate the *upper* bound of the *previous*
  group or interval) or `"exclusive"` (values indicate the *lower* bound
  of the *next* group or interval to begin). Use `labels = "range"` to
  make this behaviour easier to see.

- labels:

  Character vector of value labels. If not `NULL`, `categorize()` will
  returns factors instead of numeric variables, with `labels` used for
  labelling the factor levels. Can also be `"mean"`, `"median"`,
  `"range"` or `"observed"` for a factor with labels as the mean/median,
  the requested range (even if not all values of that range are present
  in the data) or observed range (range of the actual recoded values) of
  each group. See 'Examples'.

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

`x`, recoded into groups. By default `x` is numeric, unless `labels` is
specified. In this case, a factor is returned, where the factor levels
(i.e. recoded groups are labelled accordingly.

## Splits and breaks (cut-off values)

Breaks are by default *exclusive*, this means that these values indicate
the lower bound of the next group or interval to begin. Take a simple
example, a numeric variable with values from 1 to 9. The median would be
5, thus the first interval ranges from 1-4 and is recoded into 1, while
5-9 would turn into 2 (compare `cbind(1:9, categorize(1:9))`). The same
variable, using `split = "quantile"` and `n_groups = 3` would define
breaks at 3.67 and 6.33 (see `quantile(1:9, probs = c(1/3, 2/3))`),
which means that values from 1 to 3 belong to the first interval and are
recoded into 1 (because the next interval starts at 3.67), 4 to 6 into 2
and 7 to 9 into 3.

The opposite behaviour can be achieved using `breaks = "inclusive"`, in
which case

## Recoding into groups with equal size or range

`split = "equal_length"` and `split = "equal_range"` try to divide the
range of `x` into intervals of similar (or same) length. The difference
is that `split = "equal_length"` will divide the range of `x` into
`n_groups` pieces and thereby defining the intervals used as breaks
(hence, it is equivalent to `cut(x, breaks = n_groups)`), while
`split = "equal_range"` will cut `x` into intervals that all have the
length of `range`, where the first interval by defaults starts at `1`.
The lowest (or starting) value of that interval can be defined using the
`lowest` argument.

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
  `categorize()`,
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
set.seed(123)
x <- sample(1:10, size = 50, replace = TRUE)

table(x)
#> x
#>  1  2  3  4  5  6  7  8  9 10 
#>  2  3  5  3  7  5  5  2 11  7 

# by default, at median
table(categorize(x))
#> 
#>  1  2 
#> 25 25 

# into 3 groups, based on distribution (quantiles)
table(categorize(x, split = "quantile", n_groups = 3))
#> 
#>  1  2  3 
#> 13 19 18 

# into 3 groups, user-defined break
table(categorize(x, split = c(3, 5)))
#> 
#>  1  2  3 
#>  5  8 37 

set.seed(123)
x <- sample(1:100, size = 500, replace = TRUE)

# into 5 groups, try to recode into intervals of similar length,
# i.e. the range within groups is the same for all groups
table(categorize(x, split = "equal_length", n_groups = 5))
#> 
#>   1   2   3   4   5 
#>  89 116  96  94 105 

# into 5 groups, try to return same range within groups
# i.e. 1-20, 21-40, 41-60, etc. Since the range of "x" is
# 1-100, and we have a range of 20, this results into 5
# groups, and thus is for this particular case identical
# to the previous result.
table(categorize(x, split = "equal_range", range = 20))
#> 
#>   1   2   3   4   5 
#>  89 116  96  94 105 

# return factor with value labels instead of numeric value
set.seed(123)
x <- sample(1:10, size = 30, replace = TRUE)
categorize(x, "equal_length", n_groups = 3)
#>  [1] 1 1 3 1 2 2 2 2 3 3 2 1 3 3 3 1 3 3 3 3 3 1 2 1 3 2 3 3 3 3
categorize(x, "equal_length", n_groups = 3, labels = c("low", "mid", "high"))
#>  [1] low  low  high low  mid  mid  mid  mid  high high mid  low  high high high
#> [16] low  high high high high high low  mid  low  high mid  high high high high
#> Levels: low mid high

# cut numeric into groups with the mean or median as a label name
x <- sample(1:10, size = 30, replace = TRUE)
categorize(x, "equal_length", n_groups = 3, labels = "mean")
#>  [1] 8.45 8.45 5.33 8.45 5.33 5.33 8.45 1.57 5.33 8.45 1.57 1.57 8.45 8.45 5.33
#> [16] 5.33 8.45 8.45 5.33 5.33 8.45 5.33 5.33 8.45 1.57 5.33 1.57 1.57 1.57 5.33
#> Levels: 1.57 5.33 8.45
categorize(x, "equal_length", n_groups = 3, labels = "median")
#>  [1] 9.00 9.00 5.50 9.00 5.50 5.50 9.00 2.00 5.50 9.00 2.00 2.00 9.00 9.00 5.50
#> [16] 5.50 9.00 9.00 5.50 5.50 9.00 5.50 5.50 9.00 2.00 5.50 2.00 2.00 2.00 5.50
#> Levels: 2.00 5.50 9.00

# cut numeric into groups with the requested range as a label name
# each category has the same range, and labels indicate this range
categorize(mtcars$mpg, "equal_length", n_groups = 5, labels = "range")
#>  [1] [19.8,24.5) [19.8,24.5) [19.8,24.5) [19.8,24.5) [15.1,19.8) [15.1,19.8)
#>  [7] [10.4,15.1) [19.8,24.5) [19.8,24.5) [15.1,19.8) [15.1,19.8) [15.1,19.8)
#> [13] [15.1,19.8) [15.1,19.8) [10.4,15.1) [10.4,15.1) [10.4,15.1) [29.2,33.9]
#> [19] [29.2,33.9] [29.2,33.9] [19.8,24.5) [15.1,19.8) [15.1,19.8) [10.4,15.1)
#> [25] [15.1,19.8) [24.5,29.2) [24.5,29.2) [29.2,33.9] [15.1,19.8) [15.1,19.8)
#> [31] [10.4,15.1) [19.8,24.5)
#> Levels: [10.4,15.1) [15.1,19.8) [19.8,24.5) [24.5,29.2) [29.2,33.9]
# in this example, each category has the same range, but labels only refer
# to the ranges of the actual values (present in the data) inside each group
categorize(mtcars$mpg, "equal_length", n_groups = 5, labels = "observed")
#>  [1] (21-24.4)   (21-24.4)   (21-24.4)   (21-24.4)   (15.2-19.7) (15.2-19.7)
#>  [7] (10.4-15)   (21-24.4)   (21-24.4)   (15.2-19.7) (15.2-19.7) (15.2-19.7)
#> [13] (15.2-19.7) (15.2-19.7) (10.4-15)   (10.4-15)   (10.4-15)   (30.4-33.9)
#> [19] (30.4-33.9) (30.4-33.9) (21-24.4)   (15.2-19.7) (15.2-19.7) (10.4-15)  
#> [25] (15.2-19.7) (26-27.3)   (26-27.3)   (30.4-33.9) (15.2-19.7) (15.2-19.7)
#> [31] (10.4-15)   (21-24.4)  
#> Levels: (10.4-15) (15.2-19.7) (21-24.4) (26-27.3) (30.4-33.9)
```
