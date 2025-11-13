# Rotate a data frame

This function rotates a data frame, i.e. columns become rows and vice
versa. It's the equivalent of using
[`t()`](https://rdrr.io/r/base/t.html) but restores the `data.frame`
class, preserves attributes and prints a warning if the data type is
modified (see example).

## Usage

``` r
data_rotate(data, rownames = NULL, colnames = FALSE, verbose = TRUE)

data_transpose(data, rownames = NULL, colnames = FALSE, verbose = TRUE)
```

## Arguments

- data:

  A data frame.

- rownames:

  Character vector (optional). If not `NULL`, the data frame's rownames
  will be added as (first) column to the output, with `rownames` being
  the name of this column.

- colnames:

  Logical or character vector (optional). If `TRUE`, the values of the
  first column in `x` will be used as column names in the rotated data
  frame. If a character vector, values from that column are used as
  column names.

- verbose:

  Toggle warnings.

## Value

A (rotated) data frame.

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
  `data_rotate()`

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
x <- mtcars[1:3, 1:4]

x
#>                mpg cyl disp  hp
#> Mazda RX4     21.0   6  160 110
#> Mazda RX4 Wag 21.0   6  160 110
#> Datsun 710    22.8   4  108  93

data_rotate(x)
#>      Mazda RX4 Mazda RX4 Wag Datsun 710
#> mpg         21            21       22.8
#> cyl          6             6        4.0
#> disp       160           160      108.0
#> hp         110           110       93.0
data_rotate(x, rownames = "property")
#>   property Mazda RX4 Mazda RX4 Wag Datsun 710
#> 1      mpg        21            21       22.8
#> 2      cyl         6             6        4.0
#> 3     disp       160           160      108.0
#> 4       hp       110           110       93.0

# use values in 1. column as column name
data_rotate(x, colnames = TRUE)
#>       21  21 22.8
#> cyl    6   6    4
#> disp 160 160  108
#> hp   110 110   93
data_rotate(x, rownames = "property", colnames = TRUE)
#>   property  21  21 22.8
#> 1      cyl   6   6    4
#> 2     disp 160 160  108
#> 3       hp 110 110   93

# use either first column or specific column for column names
x <- data.frame(a = 1:5, b = 11:15, c = 21:25)
data_rotate(x, colnames = TRUE)
#>    1  2  3  4  5
#> b 11 12 13 14 15
#> c 21 22 23 24 25
data_rotate(x, colnames = "c")
#>   21 22 23 24 25
#> a  1  2  3  4  5
#> b 11 12 13 14 15
```
