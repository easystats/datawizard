
# `datawizard`: Easy Data Wrangling <img src='man/figures/logo.png' align="right" height="139" />

[![publication](https://img.shields.io/badge/Cite-Unpublished-yellow)](https://github.com/easystats/datawizard/blob/master/inst/CITATION)
[![downloads](http://cranlogs.r-pkg.org/badges/datawizard)](https://cran.r-project.org/package=datawizard)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/datawizard)](https://cranlogs.r-pkg.org/)

***Hockety pockety wockety wack, prepare this data forth and back***
:sparkles:

`datawizard` is a lightweight package to easily manipulate, clean,
transform, and prepare your data for analysis.

<img src='https://media.giphy.com/media/VcizxCUIgaKpa/giphy.gif' height="150" />

# Installation

| Type        | Source | Command                                           |
|-------------|--------|---------------------------------------------------|
| Release     | CRAN   | `install.packages("datawizard")`                  |
| Development | GitHub | `remotes::install_github("easystats/datawizard")` |

# Features

## Data wrangling

### Select and filter

### Transform

The packages also contains multiple functions to help transform data.

For example, to standardize (*z*-score) data:

``` r
# before
summary(swiss)
#>    Fertility      Agriculture     Examination      Education    
#>  Min.   :35.00   Min.   : 1.20   Min.   : 3.00   Min.   : 1.00  
#>  1st Qu.:64.70   1st Qu.:35.90   1st Qu.:12.00   1st Qu.: 6.00  
#>  Median :70.40   Median :54.10   Median :16.00   Median : 8.00  
#>  Mean   :70.14   Mean   :50.66   Mean   :16.49   Mean   :10.98  
#>  3rd Qu.:78.45   3rd Qu.:67.65   3rd Qu.:22.00   3rd Qu.:12.00  
#>  Max.   :92.50   Max.   :89.70   Max.   :37.00   Max.   :53.00  
#>     Catholic       Infant.Mortality
#>  Min.   :  2.150   Min.   :10.80   
#>  1st Qu.:  5.195   1st Qu.:18.15   
#>  Median : 15.140   Median :20.00   
#>  Mean   : 41.144   Mean   :19.94   
#>  3rd Qu.: 93.125   3rd Qu.:21.70   
#>  Max.   :100.000   Max.   :26.60

# after
summary(standardize(swiss))
#>    Fertility         Agriculture       Examination         Education      
#>  Min.   :-2.81327   Min.   :-2.1778   Min.   :-1.69084   Min.   :-1.0378  
#>  1st Qu.:-0.43569   1st Qu.:-0.6499   1st Qu.:-0.56273   1st Qu.:-0.5178  
#>  Median : 0.02061   Median : 0.1515   Median :-0.06134   Median :-0.3098  
#>  Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000  
#>  3rd Qu.: 0.66504   3rd Qu.: 0.7481   3rd Qu.: 0.69074   3rd Qu.: 0.1062  
#>  Max.   : 1.78978   Max.   : 1.7190   Max.   : 2.57094   Max.   : 4.3702  
#>     Catholic       Infant.Mortality  
#>  Min.   :-0.9350   Min.   :-3.13886  
#>  1st Qu.:-0.8620   1st Qu.:-0.61543  
#>  Median :-0.6235   Median : 0.01972  
#>  Mean   : 0.0000   Mean   : 0.00000  
#>  3rd Qu.: 1.2464   3rd Qu.: 0.60337  
#>  Max.   : 1.4113   Max.   : 2.28566
```

To winsorize data:

``` r
# before
anscombe
#>    x1 x2 x3 x4    y1   y2    y3    y4
#> 1  10 10 10  8  8.04 9.14  7.46  6.58
#> 2   8  8  8  8  6.95 8.14  6.77  5.76
#> 3  13 13 13  8  7.58 8.74 12.74  7.71
#> 4   9  9  9  8  8.81 8.77  7.11  8.84
#> 5  11 11 11  8  8.33 9.26  7.81  8.47
#> 6  14 14 14  8  9.96 8.10  8.84  7.04
#> 7   6  6  6  8  7.24 6.13  6.08  5.25
#> 8   4  4  4 19  4.26 3.10  5.39 12.50
#> 9  12 12 12  8 10.84 9.13  8.15  5.56
#> 10  7  7  7  8  4.82 7.26  6.42  7.91
#> 11  5  5  5  8  5.68 4.74  5.73  6.89

# after
winsorize(anscombe)
#>       x1 x2 x3 x4   y1   y2   y3   y4
#>  [1,] 10 10 10  8 8.04 9.13 7.46 6.58
#>  [2,]  8  8  8  8 6.95 8.14 6.77 5.76
#>  [3,] 12 12 12  8 7.58 8.74 8.15 7.71
#>  [4,]  9  9  9  8 8.81 8.77 7.11 8.47
#>  [5,] 11 11 11  8 8.33 9.13 7.81 8.47
#>  [6,] 12 12 12  8 8.81 8.10 8.15 7.04
#>  [7,]  6  6  6  8 7.24 6.13 6.08 5.76
#>  [8,]  6  6  6  8 5.68 6.13 6.08 8.47
#>  [9,] 12 12 12  8 8.81 9.13 8.15 5.76
#> [10,]  7  7  7  8 5.68 7.26 6.42 7.91
#> [11,]  6  6  6  8 5.68 6.13 6.08 6.89
```

To rank-transform data:

``` r
# before
head(trees)
#>   Girth Height Volume
#> 1   8.3     70   10.3
#> 2   8.6     65   10.3
#> 3   8.8     63   10.2
#> 4  10.5     72   16.4
#> 5  10.7     81   18.8
#> 6  10.8     83   19.7

# after
head(ranktransform(trees))
#>   Girth Height Volume
#> 1     1    6.0    2.5
#> 2     2    3.0    2.5
#> 3     3    1.0    1.0
#> 4     4    8.5    5.0
#> 5     5   25.5    7.0
#> 6     6   28.0    9.0
```

### Reshape

A common data wrangling task is to reshape data.

Either to go from wide/Cartesian to long/tidy format

``` r
library(datawizard)

wide_data <- data.frame(replicate(5, rnorm(10)))

data_to_long(wide_data)
#>    Name       Value
#> 1    X1 -0.08281164
#> 2    X2 -1.12490028
#> 3    X3 -0.70632036
#> 4    X4 -0.70278946
#> 5    X5  0.07633326
#> 6    X1  1.93468099
#> 7    X2 -0.87430362
#> 8    X3  0.96687656
#> 9    X4  0.29986416
#> 10   X5 -0.23035595
#> 11   X1 -2.05128979
#> 12   X2  0.04386162
#> 13   X3 -0.71016648
#> 14   X4  1.14946968
#> 15   X5  0.31746484
#> 16   X1  0.27773897
#> 17   X2 -0.58397514
#> 18   X3 -0.05917365
#> 19   X4 -0.30164149
#> 20   X5 -1.59268440
#> 21   X1 -1.52596060
#> 22   X2 -0.82329858
#> 23   X3 -0.23094342
#> 24   X4 -0.54733935
#> 25   X5 -0.18194062
#> 26   X1 -0.26916362
#> 27   X2  0.11059280
#> 28   X3  0.69200045
#> 29   X4 -0.38540415
#> 30   X5  1.75614174
#> 31   X1  1.23305388
#> 32   X2  0.36472778
#> 33   X3  1.35682290
#> 34   X4  0.27637203
#> 35   X5  0.11394932
#> 36   X1  0.63360774
#> 37   X2  0.05370100
#> 38   X3  1.78872284
#> 39   X4  0.15186081
#> 40   X5 -0.29216508
#> 41   X1  0.35271746
#> 42   X2  1.36867235
#> 43   X3  0.41071582
#> 44   X4 -0.43138079
#> 45   X5  1.75409316
#> 46   X1 -0.56048248
#> 47   X2 -0.38045724
#> 48   X3 -2.18785470
#> 49   X4 -1.87050006
#> 50   X5  1.80958455
```

or the other way

``` r
long_data <- data_to_long(wide_data, rows_to = "Row_ID") # Save row number

data_to_wide(long_data,
  colnames_from = "Name",
  values_from = "Value",
  rows_from = "Row_ID"
)
#>    Row_ID    Value_X1    Value_X2    Value_X3   Value_X4    Value_X5
#> 1       1 -0.08281164 -1.12490028 -0.70632036 -0.7027895  0.07633326
#> 2       2  1.93468099 -0.87430362  0.96687656  0.2998642 -0.23035595
#> 3       3 -2.05128979  0.04386162 -0.71016648  1.1494697  0.31746484
#> 4       4  0.27773897 -0.58397514 -0.05917365 -0.3016415 -1.59268440
#> 5       5 -1.52596060 -0.82329858 -0.23094342 -0.5473394 -0.18194062
#> 6       6 -0.26916362  0.11059280  0.69200045 -0.3854041  1.75614174
#> 7       7  1.23305388  0.36472778  1.35682290  0.2763720  0.11394932
#> 8       8  0.63360774  0.05370100  1.78872284  0.1518608 -0.29216508
#> 9       9  0.35271746  1.36867235  0.41071582 -0.4313808  1.75409316
#> 10     10 -0.56048248 -0.38045724 -2.18785470 -1.8705001  1.80958455
```

## Data proprties

`datawizard` provides a way to provide comprehensive descriptive summary
for all variables in a dataframe:

``` r
data(iris)
describe_distribution(iris)
#> Variable     | Mean |   SD |  IQR | Min | Max | Skewness | Kurtosis |   n | n_Missing
#> -------------------------------------------------------------------------------------
#> Sepal.Length |  5.8 | 0.83 | 1.30 | 4.3 | 7.9 |     0.31 |    -0.55 | 150 |         0
#> Sepal.Width  |  3.1 | 0.44 | 0.52 | 2.0 | 4.4 |     0.32 |     0.23 | 150 |         0
#> Petal.Length |  3.8 | 1.77 | 3.52 | 1.0 | 6.9 |    -0.27 |    -1.40 | 150 |         0
#> Petal.Width  |  1.2 | 0.76 | 1.50 | 0.1 | 2.5 |    -0.10 |    -1.34 | 150 |         0
```

Or even just a variable

``` r
describe_distribution(mtcars$wt)
#> Mean |   SD | IQR | Min | Max | Skewness | Kurtosis |  n | n_Missing
#> --------------------------------------------------------------------
#> 3.2  | 0.98 | 1.2 | 1.5 | 5.4 |     0.47 |     0.42 | 32 |         0
```

# Contributing and Support

In case you want to file an issue or contribute in another way to the
package, please follow [this
guide](https://github.com/easystats/datawizard/blob/master/.github/CONTRIBUTING.md).
For questions about the functionality, you may either contact us via
email or also file an issue.

# Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://easystats.github.io/datawizard/CODE_OF_CONDUCT.html).
By participating in this project you agree to abide by its terms.
