# Return or remove variables or observations that are completely missing

These functions check which rows or columns of a data frame completely
contain missing values, i.e. which observations or variables completely
have missing values, and either (1) returns their indices; or (2)
removes them from the data frame.

## Usage

``` r
empty_columns(x)

empty_rows(x)

remove_empty_columns(x)

remove_empty_rows(x)

remove_empty(x)
```

## Arguments

- x:

  A data frame.

## Value

- For `empty_columns()` and `empty_rows()`, a numeric (named) vector
  with row or column indices of those variables that completely have
  missing values.

- For `remove_empty_columns()` and `remove_empty_rows()`, a data frame
  with "empty" columns or rows removed, respectively.

- For `remove_empty()`, **both** empty rows and columns will be removed.

## Details

For character vectors, empty string values (i.e. `""`) are also
considered as missing value. Thus, if a character vector only contains
`NA` and `""`, it is considered as empty variable and will be removed.
Same applies to observations (rows) that only contain `NA` or `""`.

## Examples

``` r
tmp <- data.frame(
  a = c(1, 2, 3, NA, 5),
  b = c(1, NA, 3, NA, 5),
  c = c(NA, NA, NA, NA, NA),
  d = c(1, NA, 3, NA, 5)
)

tmp
#>    a  b  c  d
#> 1  1  1 NA  1
#> 2  2 NA NA NA
#> 3  3  3 NA  3
#> 4 NA NA NA NA
#> 5  5  5 NA  5

# indices of empty columns or rows
empty_columns(tmp)
#> c 
#> 3 
empty_rows(tmp)
#> [1] 4

# remove empty columns or rows
remove_empty_columns(tmp)
#>    a  b  d
#> 1  1  1  1
#> 2  2 NA NA
#> 3  3  3  3
#> 4 NA NA NA
#> 5  5  5  5
remove_empty_rows(tmp)
#>   a  b  c  d
#> 1 1  1 NA  1
#> 2 2 NA NA NA
#> 3 3  3 NA  3
#> 5 5  5 NA  5

# remove empty columns and rows
remove_empty(tmp)
#>   a  b  d
#> 1 1  1  1
#> 2 2 NA NA
#> 3 3  3  3
#> 5 5  5  5

# also remove "empty" character vectors
tmp <- data.frame(
  a = c(1, 2, 3, NA, 5),
  b = c(1, NA, 3, NA, 5),
  c = c("", "", "", "", ""),
  stringsAsFactors = FALSE
)
empty_columns(tmp)
#> c 
#> 3 
```
