
# datawizard <img src='man/figures/logo.png' align="right" height="139" />

[![publication](https://img.shields.io/badge/Cite-Unpublished-yellow)](https://github.com/easystats/datawizard/blob/master/inst/CITATION)
[![downloads](http://cranlogs.r-pkg.org/badges/datawizard)](https://cran.r-project.org/package=datawizard)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/datawizard)](https://cranlogs.r-pkg.org/)

***Hockety pockety wockety wack, prepare this data forth and back***
:sparkles:

`datawizard` is a lightweight package to easily manipulate, clean,
transform, and prepare your data for analysis.

<img src='https://media.giphy.com/media/VcizxCUIgaKpa/giphy.gif' height="150" />

## Installation

[![CRAN](http://www.r-pkg.org/badges/version/datawizard)](https://cran.r-project.org/package=datawizard)
![Tests](https://github.com/easystats/datawizard/workflows/R-check/badge.svg)
[![codecov](https://codecov.io/gh/easystats/datawizard/branch/master/graph/badge.svg)](https://codecov.io/gh/easystats/datawizard)

<!-- Run the following to install the stable release of **modelbased** from CRAN: -->
<!-- ```{r, warning=FALSE, message=FALSE, eval=FALSE} -->
<!-- install.packages("modelbased") -->
<!-- ``` -->
<!-- Or this one to install the latest development version: -->
<!-- ```{r, warning=FALSE, message=FALSE, eval=FALSE} -->
<!-- install.packages("remotes") -->
<!-- remotes::install_github("easystats/modelbased") -->
<!-- ``` -->

Run the following to install **datawizard**:

``` r
install.packages("remotes")
remotes::install_github("easystats/datawizard")
```

# Features

## Datasets

### Included datasets

### Data simulation

## Data wrangling

### Select & filter

### Transform

### Reshape

## Data proprties

### Describe a Distribution

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

## Miscellaneous
