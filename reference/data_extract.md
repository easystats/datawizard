# Extract one or more columns or elements from an object

`data_extract()` (or its alias `extract()`) is similar to `$`. It
extracts either a single column or element from an object (e.g., a data
frame, list), or multiple columns resp. elements.

## Usage

``` r
data_extract(data, select, ...)

# S3 method for class 'data.frame'
data_extract(
  data,
  select,
  name = NULL,
  extract = "all",
  as_data_frame = FALSE,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  The object to subset. Methods are currently available for data frames
  and data frame extensions (e.g., tibbles).

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

- ...:

  For use by future methods.

- name:

  An optional argument that specifies the column to be used as names for
  the vector elements after extraction. Must be specified either as
  literal variable name (e.g., `column_name`) or as string
  (`"column_name"`). `name` will be ignored when a data frame is
  returned.

- extract:

  String, indicating which element will be extracted when `select`
  matches multiple variables. Can be `"all"` (the default) to return all
  matched variables, `"first"` or `"last"` to return the first or last
  match, or `"odd"` and `"even"` to return all odd-numbered or
  even-numbered matches. Note that `"first"` or `"last"` return a vector
  (unless `as_data_frame = TRUE`), while `"all"` can return a vector (if
  only one match was found) *or* a data frame (for more than one match).
  Type safe return values are only possible when `extract` is `"first"`
  or `"last"` (will always return a vector) or when
  `as_data_frame = TRUE` (always returns a data frame).

- as_data_frame:

  Logical, if `TRUE`, will always return a data frame, even if only one
  variable was matched. If `FALSE`, either returns a vector or a data
  frame. See `extract` for details.

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

A vector (or a data frame) containing the extracted element, or `NULL`
if no matching variable was found.

## Details

`data_extract()` can be used to select multiple variables or pull a
single variable from a data frame. Thus, the return value is by default
not type safe - `data_extract()` either returns a vector or a data
frame.

### Extracting single variables (vectors)

When `select` is the name of a single column, or when select only
matches one column, a vector is returned. A single variable is also
returned when `extract` is either `"first` or `"last"`. Setting
`as_data_frame` to `TRUE` overrides this behaviour and *always* returns
a data frame.

### Extracting a data frame of variables

When `select` is a character vector containing more than one column name
(or a numeric vector with more than one valid column indices), or when
`select` uses one of the supported select-helpers that match multiple
columns, a data frame is returned. Setting `as_data_frame` to `TRUE`
*always* returns a data frame.

## Examples

``` r
# single variable
data_extract(mtcars, cyl, name = gear)
#> 4 4 4 3 3 3 3 4 4 4 4 3 3 3 3 3 3 4 4 4 3 3 3 3 3 4 5 5 5 5 5 4 
#> 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4 
data_extract(mtcars, "cyl", name = gear)
#> 4 4 4 3 3 3 3 4 4 4 4 3 3 3 3 3 3 4 4 4 3 3 3 3 3 4 5 5 5 5 5 4 
#> 6 6 4 6 8 6 8 4 4 6 6 8 8 8 8 8 8 4 4 4 4 8 8 8 8 4 4 4 8 6 8 4 
data_extract(mtcars, -1, name = gear)
#>                     cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4             6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag         6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710            4 108.0  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive        6 258.0 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout     8 360.0 175 3.15 3.440 17.02  0  0    3    2
#> Valiant               6 225.0 105 2.76 3.460 20.22  1  0    3    1
#> Duster 360            8 360.0 245 3.21 3.570 15.84  0  0    3    4
#> Merc 240D             4 146.7  62 3.69 3.190 20.00  1  0    4    2
#> Merc 230              4 140.8  95 3.92 3.150 22.90  1  0    4    2
#> Merc 280              6 167.6 123 3.92 3.440 18.30  1  0    4    4
#> Merc 280C             6 167.6 123 3.92 3.440 18.90  1  0    4    4
#> Merc 450SE            8 275.8 180 3.07 4.070 17.40  0  0    3    3
#> Merc 450SL            8 275.8 180 3.07 3.730 17.60  0  0    3    3
#> Merc 450SLC           8 275.8 180 3.07 3.780 18.00  0  0    3    3
#> Cadillac Fleetwood    8 472.0 205 2.93 5.250 17.98  0  0    3    4
#> Lincoln Continental   8 460.0 215 3.00 5.424 17.82  0  0    3    4
#> Chrysler Imperial     8 440.0 230 3.23 5.345 17.42  0  0    3    4
#> Fiat 128              4  78.7  66 4.08 2.200 19.47  1  1    4    1
#> Honda Civic           4  75.7  52 4.93 1.615 18.52  1  1    4    2
#> Toyota Corolla        4  71.1  65 4.22 1.835 19.90  1  1    4    1
#> Toyota Corona         4 120.1  97 3.70 2.465 20.01  1  0    3    1
#> Dodge Challenger      8 318.0 150 2.76 3.520 16.87  0  0    3    2
#> AMC Javelin           8 304.0 150 3.15 3.435 17.30  0  0    3    2
#> Camaro Z28            8 350.0 245 3.73 3.840 15.41  0  0    3    4
#> Pontiac Firebird      8 400.0 175 3.08 3.845 17.05  0  0    3    2
#> Fiat X1-9             4  79.0  66 4.08 1.935 18.90  1  1    4    1
#> Porsche 914-2         4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Lotus Europa          4  95.1 113 3.77 1.513 16.90  1  1    5    2
#> Ford Pantera L        8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino          6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora         8 301.0 335 3.54 3.570 14.60  0  1    5    8
#> Volvo 142E            4 121.0 109 4.11 2.780 18.60  1  1    4    2
data_extract(mtcars, cyl, name = 0)
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>                   6                   6                   4                   6 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>                   8                   6                   8                   4 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>                   4                   6                   6                   8 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>                   8                   8                   8                   8 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>                   8                   4                   4                   4 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   4                   8                   8                   8 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>                   8                   4                   4                   4 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>                   8                   6                   8                   4 
data_extract(mtcars, cyl, name = "row.names")
#>           Mazda RX4       Mazda RX4 Wag          Datsun 710      Hornet 4 Drive 
#>                   6                   6                   4                   6 
#>   Hornet Sportabout             Valiant          Duster 360           Merc 240D 
#>                   8                   6                   8                   4 
#>            Merc 230            Merc 280           Merc 280C          Merc 450SE 
#>                   4                   6                   6                   8 
#>          Merc 450SL         Merc 450SLC  Cadillac Fleetwood Lincoln Continental 
#>                   8                   8                   8                   8 
#>   Chrysler Imperial            Fiat 128         Honda Civic      Toyota Corolla 
#>                   8                   4                   4                   4 
#>       Toyota Corona    Dodge Challenger         AMC Javelin          Camaro Z28 
#>                   4                   8                   8                   8 
#>    Pontiac Firebird           Fiat X1-9       Porsche 914-2        Lotus Europa 
#>                   8                   4                   4                   4 
#>      Ford Pantera L        Ferrari Dino       Maserati Bora          Volvo 142E 
#>                   8                   6                   8                   4 

# selecting multiple variables
head(data_extract(iris, starts_with("Sepal")))
#>   Sepal.Length Sepal.Width
#> 1          5.1         3.5
#> 2          4.9         3.0
#> 3          4.7         3.2
#> 4          4.6         3.1
#> 5          5.0         3.6
#> 6          5.4         3.9
head(data_extract(iris, ends_with("Width")))
#>   Sepal.Width Petal.Width
#> 1         3.5         0.2
#> 2         3.0         0.2
#> 3         3.2         0.2
#> 4         3.1         0.2
#> 5         3.6         0.2
#> 6         3.9         0.4
head(data_extract(iris, 2:4))
#>   Sepal.Width Petal.Length Petal.Width
#> 1         3.5          1.4         0.2
#> 2         3.0          1.4         0.2
#> 3         3.2          1.3         0.2
#> 4         3.1          1.5         0.2
#> 5         3.6          1.4         0.2
#> 6         3.9          1.7         0.4

# select first of multiple variables
data_extract(iris, starts_with("Sepal"), extract = "first")
#>   [1] 5.1 4.9 4.7 4.6 5.0 5.4 4.6 5.0 4.4 4.9 5.4 4.8 4.8 4.3 5.8 5.7 5.4 5.1
#>  [19] 5.7 5.1 5.4 5.1 4.6 5.1 4.8 5.0 5.0 5.2 5.2 4.7 4.8 5.4 5.2 5.5 4.9 5.0
#>  [37] 5.5 4.9 4.4 5.1 5.0 4.5 4.4 5.0 5.1 4.8 5.1 4.6 5.3 5.0 7.0 6.4 6.9 5.5
#>  [55] 6.5 5.7 6.3 4.9 6.6 5.2 5.0 5.9 6.0 6.1 5.6 6.7 5.6 5.8 6.2 5.6 5.9 6.1
#>  [73] 6.3 6.1 6.4 6.6 6.8 6.7 6.0 5.7 5.5 5.5 5.8 6.0 5.4 6.0 6.7 6.3 5.6 5.5
#>  [91] 5.5 6.1 5.8 5.0 5.6 5.7 5.7 6.2 5.1 5.7 6.3 5.8 7.1 6.3 6.5 7.6 4.9 7.3
#> [109] 6.7 7.2 6.5 6.4 6.8 5.7 5.8 6.4 6.5 7.7 7.7 6.0 6.9 5.6 7.7 6.3 6.7 7.2
#> [127] 6.2 6.1 6.4 7.2 7.4 7.9 6.4 6.3 6.1 7.7 6.3 6.4 6.0 6.9 6.7 6.9 5.8 6.8
#> [145] 6.7 6.7 6.3 6.5 6.2 5.9

# select first of multiple variables, return as data frame
head(data_extract(iris, starts_with("Sepal"), extract = "first", as_data_frame = TRUE))
#>   Sepal.Length
#> 1          5.1
#> 2          4.9
#> 3          4.7
#> 4          4.6
#> 5          5.0
#> 6          5.4
```
