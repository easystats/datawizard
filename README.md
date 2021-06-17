
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
