# Create frequency and crosstables of variables

This function creates frequency or crosstables of variables, including
the number of levels/values as well as the distribution of raw, valid
and cumulative percentages. For crosstables, row, column and cell
percentages can be calculated.

## Usage

``` r
data_tabulate(x, ...)

# Default S3 method
data_tabulate(
  x,
  by = NULL,
  drop_levels = FALSE,
  weights = NULL,
  remove_na = FALSE,
  proportions = NULL,
  name = NULL,
  verbose = TRUE,
  ...
)

# S3 method for class 'data.frame'
data_tabulate(
  x,
  select = NULL,
  exclude = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  by = NULL,
  drop_levels = FALSE,
  weights = NULL,
  remove_na = FALSE,
  proportions = NULL,
  collapse = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'datawizard_table'
print(x, big_mark = NULL, ...)

# S3 method for class 'datawizard_table'
display(object, big_mark = NULL, format = "markdown", ...)
```

## Arguments

- x:

  A (grouped) data frame, a vector or factor.

- ...:

  not used.

- by:

  Optional vector or factor. If supplied, a crosstable is created. If
  `x` is a data frame, `by` can also be a character string indicating
  the name of a variable in `x`.

- drop_levels:

  Logical, if `FALSE`, factor levels that do not occur in the data are
  included in the table (with frequency of zero), else unused factor
  levels are dropped from the frequency table.

- weights:

  Optional numeric vector of weights. Must be of the same length as `x`.
  If `weights` is supplied, weighted frequencies are calculated.

- remove_na:

  Logical, if `FALSE`, missing values are included in the frequency or
  crosstable, else missing values are omitted. Note that the default for
  the [`as.table()`](https://rdrr.io/r/base/table.html) method is
  `remove_na = TRUE`, so that missing values are not included in the
  returned table, which makes more sense for post-processing of the
  table, e.g. using
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

- proportions:

  Optional character string, indicating the type of percentages to be
  calculated. Only applies to crosstables, i.e. when `by` is not `NULL`.
  Can be `"row"` (row percentages), `"column"` (column percentages) or
  `"full"` (to calculate relative frequencies for the full table).

- name:

  Optional character string, which includes the name that is used for
  printing.

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

- collapse:

  Logical, if `TRUE` collapses multiple tables into one larger table for
  printing. This affects only printing, not the returned object.

- big_mark:

  Optional character string, indicating the big mark that is used for
  large numbers. If `NULL` (default), a big mark is added automatically
  for large numbers (i.e. numbers with more than 5 digits). If you want
  to remove the big mark, set `big_mark = ""`.

- object:

  An object returned by `data_tabulate()`.

- format:

  String, indicating the output format. Can be `"markdown"` `"html"`, or
  `"tt"`. `format = "html"` create an HTML table using the *gt* package.
  `format = "tt"` creates a `tinytable` object, which is either printed
  as markdown or HTML table, depending on the environment. See
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)
  for details.

## Value

A data frame, or a list of data frames, with one frequency table as data
frame per variable.

## Details

There is an
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method,
to return the frequency tables as a data frame. The structure of the
returned object is a nested data frame, where the first column contains
name of the variable for which frequencies were calculated, and the
second column is a list column that contains the frequency tables as
data frame. See
[as.table.datawizard_table](https://easystats.github.io/datawizard/reference/as.prop.table.md).

There is also an [`as.table()`](https://rdrr.io/r/base/table.html)
method, which returns a table object with the frequencies of the
variable. This is useful for further statistical analysis, e.g. for
using [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html) on the
frequency table. See
[as.table.datawizard_table](https://easystats.github.io/datawizard/reference/as.prop.table.md).

## Note

There are
[`print_html()`](https://easystats.github.io/insight/reference/display.html)
and
[`print_md()`](https://easystats.github.io/insight/reference/display.html)
methods available for printing frequency or crosstables in HTML and
markdown format, e.g. `print_html(data_tabulate(x))`. The
[`print()`](https://rdrr.io/r/base/print.html) method for text outputs
passes arguments in `...` to
[`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html).

## Crosstables

If `by` is supplied, a crosstable is created. The crosstable includes
`<NA>` (missing) values by default. The first column indicates values of
`x`, the first row indicates values of `by` (including missing values).
The last row and column contain the total frequencies for each row and
column, respectively. Setting `remove_na = FALSE` will omit missing
values from the crosstable. Setting `proportions` to `"row"` or
`"column"` will add row or column percentages. Setting `proportions` to
`"full"` will add relative frequencies for the full table.

## See also

[as.prop.table](https://easystats.github.io/datawizard/reference/as.prop.table.md)

## Examples

``` r
# frequency tables -------
# ------------------------
data(efc)

# vector/factor
data_tabulate(efc$c172code)
#> carer's level of education (efc$c172code) <numeric>
#> # total N=100 valid N=90
#> 
#> Value |  N | Raw % | Valid % | Cumulative %
#> ------+----+-------+---------+-------------
#> 1     |  8 |     8 |    8.89 |         8.89
#> 2     | 66 |    66 |   73.33 |        82.22
#> 3     | 16 |    16 |   17.78 |       100.00
#> <NA>  | 10 |    10 |    <NA> |         <NA>

# drop missing values
data_tabulate(efc$c172code, remove_na = TRUE)
#> carer's level of education (efc$c172code) <numeric>
#> # total N=90 valid N=90
#> 
#> Value |  N | Raw % | Valid % | Cumulative %
#> ------+----+-------+---------+-------------
#> 1     |  8 |  8.89 |    8.89 |         8.89
#> 2     | 66 | 73.33 |   73.33 |        82.22
#> 3     | 16 | 17.78 |   17.78 |       100.00

# data frame
data_tabulate(efc, c("e42dep", "c172code"))
#> elder's dependency (e42dep) <categorical>
#> # total N=100 valid N=97
#> 
#> Value |  N | Raw % | Valid % | Cumulative %
#> ------+----+-------+---------+-------------
#> 1     |  2 |     2 |    2.06 |         2.06
#> 2     |  4 |     4 |    4.12 |         6.19
#> 3     | 28 |    28 |   28.87 |        35.05
#> 4     | 63 |    63 |   64.95 |       100.00
#> <NA>  |  3 |     3 |    <NA> |         <NA>
#> 
#> carer's level of education (c172code) <numeric>
#> # total N=100 valid N=90
#> 
#> Value |  N | Raw % | Valid % | Cumulative %
#> ------+----+-------+---------+-------------
#> 1     |  8 |     8 |    8.89 |         8.89
#> 2     | 66 |    66 |   73.33 |        82.22
#> 3     | 16 |    16 |   17.78 |       100.00
#> <NA>  | 10 |    10 |    <NA> |         <NA>

# grouped data frame
suppressPackageStartupMessages(library(poorman, quietly = TRUE))
efc %>%
  group_by(c172code) %>%
  data_tabulate("e16sex")
#> elder's gender (e16sex) <numeric>
#> Grouped by c172code (1)
#> # total N=8 valid N=8
#> 
#> Value | N | Raw % | Valid % | Cumulative %
#> ------+---+-------+---------+-------------
#> 1     | 5 | 62.50 |   62.50 |        62.50
#> 2     | 3 | 37.50 |   37.50 |       100.00
#> <NA>  | 0 |  0.00 |    <NA> |         <NA>
#> 
#> elder's gender (e16sex) <numeric>
#> Grouped by c172code (2)
#> # total N=66 valid N=66
#> 
#> Value |  N | Raw % | Valid % | Cumulative %
#> ------+----+-------+---------+-------------
#> 1     | 32 | 48.48 |   48.48 |        48.48
#> 2     | 34 | 51.52 |   51.52 |       100.00
#> <NA>  |  0 |  0.00 |    <NA> |         <NA>
#> 
#> elder's gender (e16sex) <numeric>
#> Grouped by c172code (3)
#> # total N=16 valid N=16
#> 
#> Value |  N | Raw % | Valid % | Cumulative %
#> ------+----+-------+---------+-------------
#> 1     |  4 |    25 |      25 |           25
#> 2     | 12 |    75 |      75 |          100
#> <NA>  |  0 |     0 |    <NA> |         <NA>
#> 
#> elder's gender (e16sex) <numeric>
#> Grouped by c172code (NA)
#> # total N=10 valid N=10
#> 
#> Value | N | Raw % | Valid % | Cumulative %
#> ------+---+-------+---------+-------------
#> 1     | 5 |    50 |      50 |           50
#> 2     | 5 |    50 |      50 |          100
#> <NA>  | 0 |     0 |    <NA> |         <NA>

# collapse tables
efc %>%
  group_by(c172code) %>%
  data_tabulate("e16sex", collapse = TRUE)
#> # Frequency Table
#> 
#> Variable |         Group | Value |  N | Raw % | Valid % | Cumulative %
#> ---------+---------------+-------+----+-------+---------+-------------
#> e16sex   |  c172code (1) |     1 |  5 | 62.50 |   62.50 |        62.50
#>          |               |     2 |  3 | 37.50 |   37.50 |       100.00
#>          |               |  <NA> |  0 |  0.00 |    <NA> |         <NA>
#> ---------+---------------+-------+----+-------+---------+-------------
#> e16sex   |  c172code (2) |     1 | 32 | 48.48 |   48.48 |        48.48
#>          |               |     2 | 34 | 51.52 |   51.52 |       100.00
#>          |               |  <NA> |  0 |  0.00 |    <NA> |         <NA>
#> ---------+---------------+-------+----+-------+---------+-------------
#> e16sex   |  c172code (3) |     1 |  4 |    25 |      25 |           25
#>          |               |     2 | 12 |    75 |      75 |          100
#>          |               |  <NA> |  0 |     0 |    <NA> |         <NA>
#> ---------+---------------+-------+----+-------+---------+-------------
#> e16sex   | c172code (NA) |     1 |  5 |    50 |      50 |           50
#>          |               |     2 |  5 |    50 |      50 |          100
#>          |               |  <NA> |  0 |     0 |    <NA> |         <NA>
#> ----------------------------------------------------------------------

# for larger N's (> 100000), a big mark is automatically added
set.seed(123)
x <- sample(1:3, 1e6, TRUE)
data_tabulate(x, name = "Large Number")
#> Large Number (x) <integer>
#> # total N=1,000,000 valid N=1,000,000
#> 
#> Value |       N | Raw % | Valid % | Cumulative %
#> ------+---------+-------+---------+-------------
#> 1     | 333,852 | 33.39 |   33.39 |        33.39
#> 2     | 332,910 | 33.29 |   33.29 |        66.68
#> 3     | 333,238 | 33.32 |   33.32 |       100.00
#> <NA>  |       0 |  0.00 |    <NA> |         <NA>

# to remove the big mark, use "print(..., big_mark = "")"
print(data_tabulate(x), big_mark = "")
#> x <integer>
#> # total N=1000000 valid N=1000000
#> 
#> Value |      N | Raw % | Valid % | Cumulative %
#> ------+--------+-------+---------+-------------
#> 1     | 333852 | 33.39 |   33.39 |        33.39
#> 2     | 332910 | 33.29 |   33.29 |        66.68
#> 3     | 333238 | 33.32 |   33.32 |       100.00
#> <NA>  |      0 |  0.00 |    <NA> |         <NA>

# weighted frequencies
set.seed(123)
efc$weights <- abs(rnorm(n = nrow(efc), mean = 1, sd = 0.5))
data_tabulate(efc$e42dep, weights = efc$weights)
#> elder's dependency (efc$e42dep) <categorical>
#> # total N=105 valid N=100 (weighted)
#> 
#> Value |  N | Raw % | Valid % | Cumulative %
#> ------+----+-------+---------+-------------
#> 1     |  3 |  2.86 |       3 |            3
#> 2     |  4 |  3.81 |       4 |            7
#> 3     | 26 | 24.76 |      26 |           33
#> 4     | 67 | 63.81 |      67 |          100
#> <NA>  |  5 |  4.76 |    <NA> |         <NA>

# crosstables ------
# ------------------

# add some missing values
set.seed(123)
efc$e16sex[sample.int(nrow(efc), 5)] <- NA

data_tabulate(efc, "c172code", by = "e16sex")
#> c172code | male | female | <NA> | Total
#> ---------+------+--------+------+------
#> 1        |    5 |      2 |    1 |     8
#> 2        |   30 |     34 |    2 |    66
#> 3        |    4 |     10 |    2 |    16
#> <NA>     |    5 |      5 |    0 |    10
#> ---------+------+--------+------+------
#> Total    |   44 |     51 |    5 |   100

# add row and column percentages
data_tabulate(efc, "c172code", by = "e16sex", proportions = "row")
#> c172code |       male |     female |      <NA> | Total
#> ---------+------------+------------+-----------+------
#> 1        |  5 (62.5%) |  2 (25.0%) | 1 (12.5%) |     8
#> 2        | 30 (45.5%) | 34 (51.5%) | 2  (3.0%) |    66
#> 3        |  4 (25.0%) | 10 (62.5%) | 2 (12.5%) |    16
#> <NA>     |  5 (50.0%) |  5 (50.0%) | 0  (0.0%) |    10
#> ---------+------------+------------+-----------+------
#> Total    |         44 |         51 |         5 |   100
data_tabulate(efc, "c172code", by = "e16sex", proportions = "column")
#> c172code |       male |     female |      <NA> | Total
#> ---------+------------+------------+-----------+------
#> 1        |  5 (11.4%) |  2  (3.9%) | 1 (20.0%) |     8
#> 2        | 30 (68.2%) | 34 (66.7%) | 2 (40.0%) |    66
#> 3        |  4  (9.1%) | 10 (19.6%) | 2 (40.0%) |    16
#> <NA>     |  5 (11.4%) |  5  (9.8%) | 0  (0.0%) |    10
#> ---------+------------+------------+-----------+------
#> Total    |         44 |         51 |         5 |   100

# omit missing values
data_tabulate(
  efc$c172code,
  by = efc$e16sex,
  proportions = "column",
  remove_na = TRUE
)
#> efc$c172code |       male |     female | Total
#> -------------+------------+------------+------
#> 1            |  5 (12.8%) |  2  (4.3%) |     7
#> 2            | 30 (76.9%) | 34 (73.9%) |    64
#> 3            |  4 (10.3%) | 10 (21.7%) |    14
#> -------------+------------+------------+------
#> Total        |         39 |         46 |    85

# round percentages
out <- data_tabulate(efc, "c172code", by = "e16sex", proportions = "column")
print(out, digits = 0)
#> c172code |     male |   female |    <NA> | Total
#> ---------+----------+----------+---------+------
#> 1        |  5 (11%) |  2  (4%) | 1 (20%) |     8
#> 2        | 30 (68%) | 34 (67%) | 2 (40%) |    66
#> 3        |  4  (9%) | 10 (20%) | 2 (40%) |    16
#> <NA>     |  5 (11%) |  5 (10%) | 0  (0%) |    10
#> ---------+----------+----------+---------+------
#> Total    |       44 |       51 |       5 |   100
```
