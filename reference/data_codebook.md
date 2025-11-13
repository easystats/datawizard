# Generate a codebook of a data frame.

`data_codebook()` generates codebooks from data frames, i.e. overviews
of all variables and some more information about each variable (like
labels, values or value range, frequencies, amount of missing values).

## Usage

``` r
data_codebook(
  data,
  select = NULL,
  exclude = NULL,
  variable_label_width = NULL,
  value_label_width = NULL,
  max_values = 10,
  range_at = 6,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)

# S3 method for class 'data_codebook'
print_html(
  x,
  font_size = "100%",
  line_padding = 3,
  row_color = "#eeeeee",
  ...
)

# S3 method for class 'data_codebook'
display(
  object,
  format = "markdown",
  font_size = "100%",
  line_padding = 3,
  row_color = "#eeeeee",
  ...
)
```

## Arguments

- data:

  A data frame, or an object that can be coerced to a data frame.

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

- variable_label_width:

  Length of variable labels. Longer labels will be wrapped at
  `variable_label_width` chars. If `NULL`, longer labels will not be
  split into multiple lines. Only applies to *labelled data*.

- value_label_width:

  Length of value labels. Longer labels will be shortened, where the
  remaining part is truncated. Only applies to *labelled data* or factor
  levels.

- max_values:

  Number of maximum values that should be displayed. Can be used to
  avoid too many rows when variables have lots of unique values.

- range_at:

  Indicates how many unique values in a numeric vector are needed in
  order to print a range for that variable instead of a frequency table
  for all numeric values. Can be useful if the data contains numeric
  variables with only a few unique values and where full frequency
  tables instead of value ranges should be displayed.

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

  Toggle warnings and messages on or off.

- ...:

  Arguments passed to or from other methods.

- x:

  A (grouped) data frame, a vector or a statistical model (for
  [`unstandardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  cannot be a model).

- font_size:

  For HTML tables, the font size.

- line_padding:

  For HTML tables, the distance (in pixel) between lines.

- row_color:

  For HTML tables, the fill color for odd rows.

- object:

  An object returned by
  [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md).

- format:

  String, indicating the output format. Can be `"markdown"` `"html"`, or
  `"tt"`. `format = "html"` create an HTML table using the *gt* package.
  `format = "tt"` creates a `tinytable` object, which is either printed
  as markdown or HTML table, depending on the environment. See
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html)
  for details.

## Value

A formatted data frame, summarizing the content of the data frame.
Returned columns include the column index of the variables in the
original data frame (`ID`), column name, variable label (if data is
labelled), type of variable, number of missing values, unique values (or
value range), value labels (for labelled data), and a frequency table (N
for each value). Most columns are formatted as character vectors.

## Note

There are methods to [`print()`](https://rdrr.io/r/base/print.html) the
data frame in a nicer output, as well methods for printing in markdown
or HTML format
([`print_md()`](https://easystats.github.io/insight/reference/display.html)
and
[`print_html()`](https://easystats.github.io/insight/reference/display.html)).
The [`print()`](https://rdrr.io/r/base/print.html) method for text
outputs passes arguments in `...` to
[`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html).

## Examples

``` r
data(iris)
data_codebook(iris, select = starts_with("Sepal"))
#> iris (150 rows and 5 variables, 2 shown)
#> 
#> ID | Name         | Type    | Missings |     Values |   N
#> ---+--------------+---------+----------+------------+----
#> 1  | Sepal.Length | numeric | 0 (0.0%) | [4.3, 7.9] | 150
#> ---+--------------+---------+----------+------------+----
#> 2  | Sepal.Width  | numeric | 0 (0.0%) |   [2, 4.4] | 150
#> ---------------------------------------------------------

data(efc)
data_codebook(efc)
#> efc (100 rows and 5 variables, 5 shown)
#> 
#> ID | Name     | Label                                    | Type       
#> ---+----------+------------------------------------------+------------
#> 1  | c12hour  | average number of hours of care per week | numeric    
#> ---+----------+------------------------------------------+------------
#> 2  | e16sex   | elder's gender                           | numeric    
#>    |          |                                          |            
#> ---+----------+------------------------------------------+------------
#> 3  | e42dep   | elder's dependency                       | categorical
#>    |          |                                          |            
#>    |          |                                          |            
#>    |          |                                          |            
#> ---+----------+------------------------------------------+------------
#> 4  | c172code | carer's level of education               | numeric    
#>    |          |                                          |            
#>    |          |                                          |            
#> ---+----------+------------------------------------------+------------
#> 5  | neg_c_7  | Negative impact with 7 items             | numeric    
#> ----------------------------------------------------------------------
#> 
#> ID |   Missings |   Values | Value Labels                    |          N
#> ---+------------+----------+---------------------------------+-----------
#> 1  |   2 (2.0%) | [5, 168] |                                 |         98
#> ---+------------+----------+---------------------------------+-----------
#> 2  |   0 (0.0%) |        1 | male                            | 46 (46.0%)
#>    |            |        2 | female                          | 54 (54.0%)
#> ---+------------+----------+---------------------------------+-----------
#> 3  |   3 (3.0%) |        1 | independent                     |  2 ( 2.1%)
#>    |            |        2 | slightly dependent              |  4 ( 4.1%)
#>    |            |        3 | moderately dependent            | 28 (28.9%)
#>    |            |        4 | severely dependent              | 63 (64.9%)
#> ---+------------+----------+---------------------------------+-----------
#> 4  | 10 (10.0%) |        1 | low level of education          |  8 ( 8.9%)
#>    |            |        2 | intermediate level of education | 66 (73.3%)
#>    |            |        3 | high level of education         | 16 (17.8%)
#> ---+------------+----------+---------------------------------+-----------
#> 5  |   3 (3.0%) |  [7, 28] |                                 |         97
#> -------------------------------------------------------------------------

# shorten labels
data_codebook(efc, variable_label_width = 20, value_label_width = 15)
#> efc (100 rows and 5 variables, 5 shown)
#> 
#> ID | Name     | Label              | Type        |   Missings |   Values
#> ---+----------+--------------------+-------------+------------+---------
#> 1  | c12hour  | average number of  | numeric     |   2 (2.0%) | [5, 168]
#>    |          | hours of care per  |             |            |         
#>    |          | week               |             |            |         
#> ---+----------+--------------------+-------------+------------+---------
#> 2  | e16sex   | elder's gender     | numeric     |   0 (0.0%) |        1
#>    |          |                    |             |            |        2
#> ---+----------+--------------------+-------------+------------+---------
#> 3  | e42dep   | elder's dependency | categorical |   3 (3.0%) |        1
#>    |          |                    |             |            |        2
#>    |          |                    |             |            |        3
#>    |          |                    |             |            |        4
#> ---+----------+--------------------+-------------+------------+---------
#> 4  | c172code | carer's level of   | numeric     | 10 (10.0%) |        1
#>    |          | education          |             |            |        2
#>    |          |                    |             |            |        3
#> ---+----------+--------------------+-------------+------------+---------
#> 5  | neg_c_7  | Negative impact    | numeric     |   3 (3.0%) |  [7, 28]
#>    |          | with 7 items       |             |            |         
#> ------------------------------------------------------------------------
#> 
#> ID | Value Labels     |          N
#> ---+------------------+-----------
#> 1  |                  |         98
#>    |                  |           
#>    |                  |           
#> ---+------------------+-----------
#> 2  | male             | 46 (46.0%)
#>    | female           | 54 (54.0%)
#> ---+------------------+-----------
#> 3  | independent      |  2 ( 2.1%)
#>    | slightly...      |  4 ( 4.1%)
#>    | moderately...    | 28 (28.9%)
#>    | severely...      | 63 (64.9%)
#> ---+------------------+-----------
#> 4  | low level of...  |  8 ( 8.9%)
#>    | intermediate...  | 66 (73.3%)
#>    | high level of... | 16 (17.8%)
#> ---+------------------+-----------
#> 5  |                  |         97
#>    |                  |           
#> ----------------------------------

# automatic range for numerics at more than 5 unique values
data(mtcars)
data_codebook(mtcars, select = starts_with("c"))
#> mtcars (32 rows and 11 variables, 2 shown)
#> 
#> ID | Name | Type    | Missings | Values |          N
#> ---+------+---------+----------+--------+-----------
#> 2  | cyl  | numeric | 0 (0.0%) |      4 | 11 (34.4%)
#>    |      |         |          |      6 |  7 (21.9%)
#>    |      |         |          |      8 | 14 (43.8%)
#> ---+------+---------+----------+--------+-----------
#> 11 | carb | numeric | 0 (0.0%) | [1, 8] |         32
#> ----------------------------------------------------

# force all values to be displayed
data_codebook(mtcars, select = starts_with("c"), range_at = 100)
#> mtcars (32 rows and 11 variables, 2 shown)
#> 
#> ID | Name | Type    | Missings | Values |          N
#> ---+------+---------+----------+--------+-----------
#> 2  | cyl  | numeric | 0 (0.0%) |      4 | 11 (34.4%)
#>    |      |         |          |      6 |  7 (21.9%)
#>    |      |         |          |      8 | 14 (43.8%)
#> ---+------+---------+----------+--------+-----------
#> 11 | carb | numeric | 0 (0.0%) |      1 |  7 (21.9%)
#>    |      |         |          |      2 | 10 (31.2%)
#>    |      |         |          |      3 |  3 ( 9.4%)
#>    |      |         |          |      4 | 10 (31.2%)
#>    |      |         |          |      6 |  1 ( 3.1%)
#>    |      |         |          |      8 |  1 ( 3.1%)
#> ----------------------------------------------------
```
