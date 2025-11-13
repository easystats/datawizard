# Convert a crosstable to a frequency or a propensity table

`as.prop.table()` is an S3 generic. It can be used on objects of class
`datawizard_crosstab` created by
[`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
when it was run with the arguments `by` and `proportions`.

## Usage

``` r
as.prop.table(x, ...)

# S3 method for class 'datawizard_crosstab'
as.prop.table(x, remove_na = TRUE, simplify = FALSE, verbose = TRUE, ...)

# S3 method for class 'datawizard_tables'
as.data.frame(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  stringsAsFactors = FALSE,
  add_total = FALSE
)

# S3 method for class 'datawizard_table'
as.table(x, remove_na = TRUE, simplify = FALSE, verbose = TRUE, ...)
```

## Arguments

- x:

  An object created by
  [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md).
  It must be of class `datawizard_crosstab` for `as.prop.table()`.

- ...:

  not used.

- remove_na:

  Logical, if `FALSE`, missing values are included in the frequency or
  crosstable, else missing values are omitted. Note that the default for
  the [`as.table()`](https://rdrr.io/r/base/table.html) method is
  `remove_na = TRUE`, so that missing values are not included in the
  returned table, which makes more sense for post-processing of the
  table, e.g. using
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

- simplify:

  Logical, if `TRUE`, the returned table is simplified to a single table
  object if there is only one frequency or contingency table input.
  Else, always for multiple table inputs or when `simplify = FALSE`, a
  list of tables is returned. This is only relevant for the
  [`as.table()`](https://rdrr.io/r/base/table.html) methods. To ensure
  consistent output, the default is `FALSE`.

- verbose:

  Toggle warnings and messages.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  logical. If `TRUE`, setting row names and converting column names (to
  syntactic names: see
  [`make.names`](https://rdrr.io/r/base/make.names.html)) is optional.
  Note that all of R's base package
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  use `optional` only for column names treatment, basically with the
  meaning of
  [`data.frame`](https://rdrr.io/r/base/data.frame.html)`(*, check.names = !optional)`.
  See also the `make.names` argument of the `matrix` method.

- stringsAsFactors:

  logical: should the character vector be converted to a factor?

- add_total:

  For crosstables (i.e. when `by` is not `NULL`), a row and column with
  the total N values are added to the data frame. `add_total` has no
  effect in
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) for
  simple frequency tables.

## See also

[data_tabulate](https://easystats.github.io/datawizard/reference/data_tabulate.md)

## Examples

``` r
data(efc)

# Some cross tabulation
cross <- data_tabulate(efc, select = "e42dep", by = "c172code", proportions = "row")
cross
#> e42dep | low level of education | intermediate level of education
#> -------+------------------------+--------------------------------
#> 1      |              0  (0.0%) |                      2 (100.0%)
#> 2      |              0  (0.0%) |                      4 (100.0%)
#> 3      |              4 (14.3%) |                     16  (57.1%)
#> 4      |              4  (6.3%) |                     42  (66.7%)
#> <NA>   |              0  (0.0%) |                      2  (66.7%)
#> -------+------------------------+--------------------------------
#> Total  |                      8 |                              66
#> 
#> e42dep | high level of education |      <NA> | Total
#> -------+-------------------------+-----------+------
#> 1      |               0  (0.0%) | 0  (0.0%) |     2
#> 2      |               0  (0.0%) | 0  (0.0%) |     4
#> 3      |               6 (21.4%) | 2  (7.1%) |    28
#> 4      |              10 (15.9%) | 7 (11.1%) |    63
#> <NA>   |               0  (0.0%) | 1 (33.3%) |     3
#> -------+-------------------------+-----------+------
#> Total  |                      16 |        10 |   100

# Convert to a propensity table
as.prop.table(cross)
#> Removing NA values from frequency table.
#> [[1]]
#>   low level of education intermediate level of education
#> 1             0.00000000                      1.00000000
#> 2             0.00000000                      1.00000000
#> 3             0.14285714                      0.57142857
#> 4             0.06349206                      0.66666667
#>   high level of education
#> 1              0.00000000
#> 2              0.00000000
#> 3              0.21428571
#> 4              0.15873016
#> 

# Convert to data.frame
result <- data_tabulate(efc, "c172code", by = "e16sex")
as.data.frame(result)
#>        var        table
#> 1 c172code c(1, 2, ....
as.data.frame(result)$table
#> [[1]]
#>   c172code male female NA
#> 1        1    5      3  0
#> 2        2   32     34  0
#> 3        3    4     12  0
#> 4     <NA>    5      5  0
#> 
as.data.frame(result, add_total = TRUE)$table
#> [[1]]
#>   c172code male female <NA> Total
#> 1        1    5      3    0     8
#> 2        2   32     34    0    66
#> 3        3    4     12    0    16
#> 4     <NA>    5      5    0    10
#> 5    Total   46     54    0   100
#> 

# Convert to a table that can be passed to chisq.test()

out <- data_tabulate(efc, "c172code", by = "e16sex")
# we need to simplify the output, else we get a list of tables
tbl <- as.table(out, simplify = TRUE)
#> Removing NA values from frequency table.
tbl
#>   male female
#> 1    5      3
#> 2   32     34
#> 3    4     12
suppressWarnings(chisq.test(tbl))
#> 
#>  Pearson's Chi-squared test
#> 
#> data:  tbl
#> X-squared = 3.8802, df = 2, p-value = 0.1437
#> 

# apply chisq.test to each table
out <- data_tabulate(efc, c("c172code", "e16sex"))
suppressWarnings(lapply(as.table(out), chisq.test))
#> Removing NA values from frequency table.
#> [[1]]
#> 
#>  Chi-squared test for given probabilities
#> 
#> data:  X[[i]]
#> X-squared = 65.867, df = 2, p-value = 4.98e-15
#> 
#> 
#> [[2]]
#> 
#>  Chi-squared test for given probabilities
#> 
#> data:  X[[i]]
#> X-squared = 0.64, df = 1, p-value = 0.4237
#> 
#> 

# can also handle grouped data frames
d <- data_group(mtcars, "am")
x <- data_tabulate(d, "cyl", by = "gear")
as.table(x)
#> $`am (0)`
#>    3  4
#> 4  1  2
#> 6  2  2
#> 8 12  0
#> 
#> $`am (1)`
#>   4 5
#> 4 6 2
#> 6 2 1
#> 8 0 2
#> 
```
