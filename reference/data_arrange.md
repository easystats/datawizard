# Arrange rows by column values

`data_arrange()` orders the rows of a data frame by the values of
selected columns.

## Usage

``` r
data_arrange(data, select = NULL, safe = TRUE)
```

## Arguments

- data:

  A data frame, or an object that can be coerced to a data frame.

- select:

  Character vector of column names. Use a dash just before column name
  to arrange in decreasing order, for example `"-x1"`.

- safe:

  Do not throw an error if one of the variables specified doesn't exist.

## Value

A data frame.

## Examples

``` r

# Arrange using several variables
data_arrange(head(mtcars), c("gear", "carb"))
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4

# Arrange in decreasing order
data_arrange(head(mtcars), "-carb")
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

# Throw an error if one of the variables specified doesn't exist
try(data_arrange(head(mtcars), c("gear", "foo"), safe = FALSE))
#> Error : The following column(s) don't exist in the dataset: foo.
#>   Possibly misspelled?
```
