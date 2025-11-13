# Reshape (pivot) data from wide to long

This function "lengthens" data, increasing the number of rows and
decreasing the number of columns. This is a dependency-free base-R
equivalent of
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).

## Usage

``` r
data_to_long(
  data,
  select = "all",
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  values_to = "value",
  values_drop_na = FALSE,
  rows_to = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  ...,
  cols
)

reshape_longer(
  data,
  select = "all",
  names_to = "name",
  names_prefix = NULL,
  names_sep = NULL,
  names_pattern = NULL,
  values_to = "value",
  values_drop_na = FALSE,
  rows_to = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  ...,
  cols
)
```

## Arguments

- data:

  A data frame to convert to long format, so that it has more rows and
  fewer columns after the operation.

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

- names_to:

  The name of the new column (variable) that will contain the *names*
  from columns in `select` as values, to identify the source of the
  values. `names_to` can be a character vector with more than one column
  name, in which case `names_sep` or `names_pattern` must be provided in
  order to identify which parts of the column names go into newly
  created columns. See also 'Examples'.

- names_prefix:

  A regular expression used to remove matching text from the start of
  each variable name.

- names_sep, names_pattern:

  If `names_to` contains multiple values, this argument controls how the
  column name is broken up. `names_pattern` takes a regular expression
  containing matching groups, i.e. "()".

- values_to:

  The name of the new column that will contain the *values* of the
  columns in `select`.

- values_drop_na:

  If `TRUE`, will drop rows that contain only `NA` in the `values_to`
  column. This effectively converts explicit missing values to implicit
  missing values, and should generally be used only when missing values
  in data were created by its structure.

- rows_to:

  The name of the column that will contain the row names or row numbers
  from the original data. If `NULL`, will be removed.

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

- ...:

  Currently not used.

- cols:

  Identical to `select`. This argument is here to ensure compatibility
  with
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).
  If both `select` and `cols` are provided, `cols` is used.

## Value

If a tibble was provided as input, `reshape_longer()` also returns a
tibble. Otherwise, it returns a data frame.

## Details

Reshaping data into long format usually means that the input data frame
is in *wide* format, where multiple measurements taken on the same
subject are stored in multiple columns (variables). The long format
stores the same information in a single column, with each measurement
per subject stored in a separate row. The values of all variables that
are not in `select` will be repeated.

The necessary information for `data_to_long()` is:

- The columns that contain the repeated measurements (`select`).

- The name of the newly created column that will contain the names of
  the columns in `select` (`names_to`), to identify the source of the
  values. `names_to` can also be a character vector with more than one
  column name, in which case `names_sep` or `names_pattern` must be
  provided to specify which parts of the column names go into the newly
  created columns.

- The name of the newly created column that contains the values of the
  columns in `select` (`values_to`).

In other words: repeated measurements that are spread across several
columns will be gathered into a single column (`values_to`), with the
original column names, that identify the source of the gathered values,
stored in one or more new columns (`names_to`).

## See also

- Add a prefix or suffix to column names:
  [`data_addprefix()`](https://easystats.github.io/datawizard/reference/data_prefix_suffix.md),
  [`data_addsuffix()`](https://easystats.github.io/datawizard/reference/data_prefix_suffix.md)

- Functions to reorder or remove columns:
  [`data_reorder()`](https://easystats.github.io/datawizard/reference/data_relocate.md),
  [`data_relocate()`](https://easystats.github.io/datawizard/reference/data_relocate.md),
  [`data_remove()`](https://easystats.github.io/datawizard/reference/data_relocate.md)

- Functions to reshape, pivot or rotate data frames: `data_to_long()`,
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
wide_data <- setNames(
  data.frame(replicate(2, rnorm(8))),
  c("Time1", "Time2")
)
wide_data$ID <- 1:8
wide_data
#>        Time1       Time2 ID
#> 1  1.2394959  2.52833655  1
#> 2 -0.1089660  0.54909674  2
#> 3 -0.1172420  0.23821292  3
#> 4  0.1830826 -1.04889314  4
#> 5  1.2805549  1.29476325  5
#> 6 -1.7272706  0.82553984  6
#> 7  1.6901844 -0.05568601  7
#> 8  0.5038124 -0.78438222  8

# Default behaviour (equivalent to tidyr::pivot_longer(wide_data, cols = 1:3))
# probably doesn't make much sense to mix "time" and "id"
data_to_long(wide_data)
#>     name       value
#> 1  Time1  1.23949589
#> 2  Time2  2.52833655
#> 3     ID  1.00000000
#> 4  Time1 -0.10896597
#> 5  Time2  0.54909674
#> 6     ID  2.00000000
#> 7  Time1 -0.11724196
#> 8  Time2  0.23821292
#> 9     ID  3.00000000
#> 10 Time1  0.18308261
#> 11 Time2 -1.04889314
#> 12    ID  4.00000000
#> 13 Time1  1.28055488
#> 14 Time2  1.29476325
#> 15    ID  5.00000000
#> 16 Time1 -1.72727063
#> 17 Time2  0.82553984
#> 18    ID  6.00000000
#> 19 Time1  1.69018435
#> 20 Time2 -0.05568601
#> 21    ID  7.00000000
#> 22 Time1  0.50381245
#> 23 Time2 -0.78438222
#> 24    ID  8.00000000

# Customizing the names
data_to_long(
  wide_data,
  select = c("Time1", "Time2"),
  names_to = "Timepoint",
  values_to = "Score"
)
#>    ID Timepoint       Score
#> 1   1     Time1  1.23949589
#> 2   1     Time2  2.52833655
#> 3   2     Time1 -0.10896597
#> 4   2     Time2  0.54909674
#> 5   3     Time1 -0.11724196
#> 6   3     Time2  0.23821292
#> 7   4     Time1  0.18308261
#> 8   4     Time2 -1.04889314
#> 9   5     Time1  1.28055488
#> 10  5     Time2  1.29476325
#> 11  6     Time1 -1.72727063
#> 12  6     Time2  0.82553984
#> 13  7     Time1  1.69018435
#> 14  7     Time2 -0.05568601
#> 15  8     Time1  0.50381245
#> 16  8     Time2 -0.78438222

# Reshape multiple columns into long format.
mydat <- data.frame(
  age = c(20, 30, 40),
  sex = c("Female", "Male", "Male"),
  score_t1 = c(30, 35, 32),
  score_t2 = c(33, 34, 37),
  score_t3 = c(36, 35, 38),
  speed_t1 = c(2, 3, 1),
  speed_t2 = c(3, 4, 5),
  speed_t3 = c(1, 8, 6)
)
# The column names are split into two columns: "type" and "time". The
# pattern for splitting column names is provided in `names_pattern`. Values
# of all "score_*" and "speed_*" columns are gathered into a single column
# named "count".
data_to_long(
  mydat,
  select = 3:8,
  names_to = c("type", "time"),
  names_pattern = "(score|speed)_t(\\d+)",
  values_to = "count"
)
#>    age    sex  type time count
#> 1   20 Female score    1    30
#> 2   20 Female score    2    33
#> 3   20 Female score    3    36
#> 4   20 Female speed    1     2
#> 5   20 Female speed    2     3
#> 6   20 Female speed    3     1
#> 7   30   Male score    1    35
#> 8   30   Male score    2    34
#> 9   30   Male score    3    35
#> 10  30   Male speed    1     3
#> 11  30   Male speed    2     4
#> 12  30   Male speed    3     8
#> 13  40   Male score    1    32
#> 14  40   Male score    2    37
#> 15  40   Male score    3    38
#> 16  40   Male speed    1     1
#> 17  40   Male speed    2     5
#> 18  40   Male speed    3     6

# Full example
# ------------------
data <- psych::bfi # Wide format with one row per participant's personality test

# Pivot long format
very_long_data <- data_to_long(data,
  select = regex("\\d"), # Select all columns that contain a digit
  names_to = "Item",
  values_to = "Score",
  rows_to = "Participant"
)
head(very_long_data)
#>   gender education age Participant Item Score
#> 1      1        NA  16       61617   A1     2
#> 2      1        NA  16       61617   A2     4
#> 3      1        NA  16       61617   A3     3
#> 4      1        NA  16       61617   A4     4
#> 5      1        NA  16       61617   A5     4
#> 6      1        NA  16       61617   C1     2

even_longer_data <- data_to_long(
  tidyr::who,
  select = new_sp_m014:newrel_f65,
  names_to = c("diagnosis", "gender", "age"),
  names_pattern = "new_?(.*)_(.)(.*)",
  values_to = "count"
)
head(even_longer_data)
#> # A tibble: 6 × 8
#>   country     iso2  iso3   year diagnosis gender age   count
#>   <chr>       <chr> <chr> <dbl> <chr>     <chr>  <chr> <dbl>
#> 1 Afghanistan AF    AFG    1980 sp        m      014      NA
#> 2 Afghanistan AF    AFG    1980 sp        m      1524     NA
#> 3 Afghanistan AF    AFG    1980 sp        m      2534     NA
#> 4 Afghanistan AF    AFG    1980 sp        m      3544     NA
#> 5 Afghanistan AF    AFG    1980 sp        m      4554     NA
#> 6 Afghanistan AF    AFG    1980 sp        m      5564     NA
```
