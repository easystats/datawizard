# Restore the type of columns according to a reference data frame

Restore the type of columns according to a reference data frame

## Usage

``` r
data_restoretype(data, reference = NULL, ...)
```

## Arguments

- data:

  A data frame for which to restore the column types.

- reference:

  A reference data frame from which to find the correct column types. If
  `NULL`, each column is converted to numeric if it doesn't generate
  `NA`s. For example, `c("1", "2")` can be converted to numeric but not
  `c("Sepal.Length")`.

- ...:

  Currently not used.

## Value

A data frame with columns whose types have been restored based on the
reference data frame.

## Examples

``` r
data <- data.frame(
  Sepal.Length = c("1", "3", "2"),
  Species = c("setosa", "versicolor", "setosa"),
  New = c("1", "3", "4")
)

fixed <- data_restoretype(data, reference = iris)
summary(fixed)
#>   Sepal.Length       Species      New           
#>  Min.   :1.0   setosa    :2   Length:3          
#>  1st Qu.:1.5   versicolor:1   Class :character  
#>  Median :2.0   virginica :0   Mode  :character  
#>  Mean   :2.0                                    
#>  3rd Qu.:2.5                                    
#>  Max.   :3.0                                    
```
