
# `datawizard`: Easy Data Wrangling and Statistical Transformations <img src='man/figures/logo.png' align="right" height="139" />

[![publication](https://img.shields.io/badge/Cite-Unpublished-yellow)](https://github.com/easystats/datawizard/blob/master/inst/CITATION)
[![downloads](http://cranlogs.r-pkg.org/badges/datawizard)](https://cran.r-project.org/package=datawizard)
[![total](https://cranlogs.r-pkg.org/badges/grand-total/datawizard)](https://cranlogs.r-pkg.org/)
[![status](https://tinyverse.netlify.com/badge/datawizard)](https://CRAN.R-project.org/package=datawizard)

<!-- ***:sparkles: Hockety pockety wockety wack, prepare this data forth and back*** -->
<!-- ***Hockety pockety wockety wock, messy data is in shock*** -->
<!-- ***Hockety pockety wockety woss, you can cite i-it from JOSS*** <sup>(soon)</sup> -->
<!-- ***Hockety pockety wockety wass, datawizard saves your ass! :sparkles:*** -->

`{datawizard}` is a lightweight package to easily manipulate, clean,
transform, and prepare your data for analysis. It is part of the
[easystats ecosystem](https://easystats.github.io/easystats/), a suite
of R packages to deal with your entire statistical analysis, from
cleaning the data to reporting the results.

Most courses and tutorials about statistical modeling assume that you
are working with a clean and tidy dataset. In practice, however, a major
part of doing statistical modeling is preparing your data–cleaning up
values, creating new columns, reshaping the dataset, or transforming
some variables. `{datawizard}` provides easy to use tools to perform
these common, critical, and sometimes tedious data preparation tasks.

</br>

<img src='https://media.giphy.com/media/VcizxCUIgaKpa/giphy.gif' width="300"/>

</br>

# Installation

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/datawizard)](https://cran.r-project.org/package=datawizard)
[![insight status
badge](https://easystats.r-universe.dev/badges/datawizard)](https://easystats.r-universe.dev)
[![R
check](https://github.com/easystats/datawizard/workflows/R-check/badge.svg?branch=main)](https://github.com/easystats/datawizard/actions)

| Type        | Source     | Command                                                                      |
|-------------|------------|------------------------------------------------------------------------------|
| Release     | CRAN       | `install.packages("datawizard")`                                             |
| Development | r-universe | `install.packages("datawizard", repos = "https://easystats.r-universe.dev")` |
| Development | GitHub     | `remotes::install_github("easystats/datawizard")`                            |

# Citation

To cite the package, run the following command:

``` r
citation("datawizard")

To cite datawizard in publications use:

  Patil, Makowski, Ben-Shachar, Wiernik, Bacher, & Lüdecke (2022).
  datawizard: An R Package for Easy Data Preparation and Statistical
  Transformations. CRAN. Available from
  https://easystats.github.io/datawizard/

A BibTeX entry for LaTeX users is

  @Article{,
    title = {datawizard: An R Package for Easy Data Preparation and Statistical Transformations},
    author = {Indrajeet Patil and Dominique Makowski and Mattan S. Ben-Shachar and Brenton M. Wiernik and Etienne Bacher and Daniel Lüdecke},
    journal = {CRAN},
    year = {2022},
    note = {R package},
    url = {https://easystats.github.io/datawizard/},
  }
```

# Features

## Data wrangling

### Select, filter and remove variables

The package provides helpers to filter rows meeting certain conditions…

``` r
data_match(mtcars, data.frame(vs = 0, am = 1))
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino   19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora  15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
```

… or logical expressions:

``` r
data_filter(mtcars, vs == 0 & am == 1)
#>                 mpg cyl  disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4      21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag  21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
#> Porsche 914-2  26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
#> Ford Pantera L 15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
#> Ferrari Dino   19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
#> Maserati Bora  15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
```

Finding columns in a data frame, or retrieving the data of selected
columns, can be achieved using `find_columns()` or `get_columns()`:

``` r
# find column names matching a pattern
find_columns(iris, starts_with("Sepal"))
#> [1] "Sepal.Length" "Sepal.Width"

# return data columns matching a pattern
get_columns(iris, starts_with("Sepal")) |> head()
#>   Sepal.Length Sepal.Width
#> 1          5.1         3.5
#> 2          4.9         3.0
#> 3          4.7         3.2
#> 4          4.6         3.1
#> 5          5.0         3.6
#> 6          5.4         3.9
```

It is also possible to extract one or more variables:

``` r
# single variable
data_extract(mtcars, "gear")
#>  [1] 4 4 4 3 3 3 3 4 4 4 4 3 3 3 3 3 3 4 4 4 3 3 3 3 3 4 5 5 5 5 5 4

# more variables
head(data_extract(iris, ends_with("Width")))
#>   Sepal.Width Petal.Width
#> 1         3.5         0.2
#> 2         3.0         0.2
#> 3         3.2         0.2
#> 4         3.1         0.2
#> 5         3.6         0.2
#> 6         3.9         0.4
```

Due to the consistent API, removing variables is just as simple:

``` r
head(data_remove(iris, starts_with("Sepal")))
#>   Petal.Length Petal.Width Species
#> 1          1.4         0.2  setosa
#> 2          1.4         0.2  setosa
#> 3          1.3         0.2  setosa
#> 4          1.5         0.2  setosa
#> 5          1.4         0.2  setosa
#> 6          1.7         0.4  setosa
```

### Reorder or rename

``` r
head(data_relocate(iris, select = "Species", before = "Sepal.Length"))
#>   Species Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1  setosa          5.1         3.5          1.4         0.2
#> 2  setosa          4.9         3.0          1.4         0.2
#> 3  setosa          4.7         3.2          1.3         0.2
#> 4  setosa          4.6         3.1          1.5         0.2
#> 5  setosa          5.0         3.6          1.4         0.2
#> 6  setosa          5.4         3.9          1.7         0.4
```

``` r
head(data_rename(iris, c("Sepal.Length", "Sepal.Width"), c("length", "width")))
#>   length width Petal.Length Petal.Width Species
#> 1    5.1   3.5          1.4         0.2  setosa
#> 2    4.9   3.0          1.4         0.2  setosa
#> 3    4.7   3.2          1.3         0.2  setosa
#> 4    4.6   3.1          1.5         0.2  setosa
#> 5    5.0   3.6          1.4         0.2  setosa
#> 6    5.4   3.9          1.7         0.4  setosa
```

### Merge

``` r
x <- data.frame(a = 1:3, b = c("a", "b", "c"), c = 5:7, id = 1:3)
y <- data.frame(c = 6:8, d = c("f", "g", "h"), e = 100:102, id = 2:4)

x
#>   a b c id
#> 1 1 a 5  1
#> 2 2 b 6  2
#> 3 3 c 7  3
y
#>   c d   e id
#> 1 6 f 100  2
#> 2 7 g 101  3
#> 3 8 h 102  4

data_merge(x, y, join = "full")
#>    a    b c id    d   e
#> 3  1    a 5  1 <NA>  NA
#> 1  2    b 6  2    f 100
#> 2  3    c 7  3    g 101
#> 4 NA <NA> 8  4    h 102

data_merge(x, y, join = "left")
#>   a b c id    d   e
#> 3 1 a 5  1 <NA>  NA
#> 1 2 b 6  2    f 100
#> 2 3 c 7  3    g 101

data_merge(x, y, join = "right")
#>    a    b c id d   e
#> 1  2    b 6  2 f 100
#> 2  3    c 7  3 g 101
#> 3 NA <NA> 8  4 h 102

data_merge(x, y, join = "semi", by = "c")
#>   a b c id
#> 2 2 b 6  2
#> 3 3 c 7  3

data_merge(x, y, join = "anti", by = "c")
#>   a b c id
#> 1 1 a 5  1

data_merge(x, y, join = "inner")
#>   a b c id d   e
#> 1 2 b 6  2 f 100
#> 2 3 c 7  3 g 101

data_merge(x, y, join = "bind")
#>    a    b c id    d   e
#> 1  1    a 5  1 <NA>  NA
#> 2  2    b 6  2 <NA>  NA
#> 3  3    c 7  3 <NA>  NA
#> 4 NA <NA> 6  2    f 100
#> 5 NA <NA> 7  3    g 101
#> 6 NA <NA> 8  4    h 102
```

### Reshape

A common data wrangling task is to reshape data.

Either to go from wide/Cartesian to long/tidy format

``` r
wide_data <- data.frame(replicate(5, rnorm(10)))

head(data_to_long(wide_data))
#>   name       value
#> 1   X1 -0.08281164
#> 2   X2 -1.12490028
#> 3   X3 -0.70632036
#> 4   X4 -0.70278946
#> 5   X5  0.07633326
#> 6   X1  1.93468099
```

or the other way

``` r
long_data <- data_to_long(wide_data, rows_to = "Row_ID") # Save row number

data_to_wide(long_data,
  names_from = "name",
  values_from = "value",
  id_cols = "Row_ID"
)
#>    Row_ID          X1          X2          X3         X4          X5
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

### Empty rows and columns

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
```

### Recode or cut dataframe

``` r
set.seed(123)
x <- sample(1:10, size = 50, replace = TRUE)

table(x)
#> x
#>  1  2  3  4  5  6  7  8  9 10 
#>  2  3  5  3  7  5  5  2 11  7

# cut into 3 groups, based on distribution (quantiles)
table(categorize(x, split = "quantile", n_groups = 3))
#> 
#>  1  2  3 
#> 13 19 18
```

## Data Transformations

The packages also contains multiple functions to help transform data.

### Standardize

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

### Winsorize

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
#>    x1 x2 x3 x4   y1   y2   y3   y4
#> 1  10 10 10  8 8.04 9.13 7.46 6.58
#> 2   8  8  8  8 6.95 8.14 6.77 5.76
#> 3  12 12 12  8 7.58 8.74 8.15 7.71
#> 4   9  9  9  8 8.81 8.77 7.11 8.47
#> 5  11 11 11  8 8.33 9.13 7.81 8.47
#> 6  12 12 12  8 8.81 8.10 8.15 7.04
#> 7   6  6  6  8 7.24 6.13 6.08 5.76
#> 8   6  6  6  8 5.68 6.13 6.08 8.47
#> 9  12 12 12  8 8.81 9.13 8.15 5.76
#> 10  7  7  7  8 5.68 7.26 6.42 7.91
#> 11  6  6  6  8 5.68 6.13 6.08 6.89
```

### Center

To grand-mean center data

``` r
center(anscombe)
#>    x1 x2 x3 x4          y1         y2    y3         y4
#> 1   1  1  1 -1  0.53909091  1.6390909 -0.04 -0.9209091
#> 2  -1 -1 -1 -1 -0.55090909  0.6390909 -0.73 -1.7409091
#> 3   4  4  4 -1  0.07909091  1.2390909  5.24  0.2090909
#> 4   0  0  0 -1  1.30909091  1.2690909 -0.39  1.3390909
#> 5   2  2  2 -1  0.82909091  1.7590909  0.31  0.9690909
#> 6   5  5  5 -1  2.45909091  0.5990909  1.34 -0.4609091
#> 7  -3 -3 -3 -1 -0.26090909 -1.3709091 -1.42 -2.2509091
#> 8  -5 -5 -5 10 -3.24090909 -4.4009091 -2.11  4.9990909
#> 9   3  3  3 -1  3.33909091  1.6290909  0.65 -1.9409091
#> 10 -2 -2 -2 -1 -2.68090909 -0.2409091 -1.08  0.4090909
#> 11 -4 -4 -4 -1 -1.82090909 -2.7609091 -1.77 -0.6109091
```

### Ranktransform

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

### Rescale

To rescale a numeric variable to a new range:

``` r
change_scale(c(0, 1, 5, -5, -2))
#> [1]  50  60 100   0  30
#> attr(,"min_value")
#> [1] -5
#> attr(,"range_difference")
#> [1] 10
#> attr(,"to_range")
#> [1]   0 100
```

### Rotate or transpose

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
```

## Data properties

`datawizard` provides a way to provide comprehensive descriptive summary
for all variables in a dataframe:

``` r
data(iris)
describe_distribution(iris)
#> Variable     | Mean |   SD |  IQR |        Range | Skewness | Kurtosis |   n | n_Missing
#> ----------------------------------------------------------------------------------------
#> Sepal.Length | 5.84 | 0.83 | 1.30 | [4.30, 7.90] |     0.31 |    -0.55 | 150 |         0
#> Sepal.Width  | 3.06 | 0.44 | 0.52 | [2.00, 4.40] |     0.32 |     0.23 | 150 |         0
#> Petal.Length | 3.76 | 1.77 | 3.52 | [1.00, 6.90] |    -0.27 |    -1.40 | 150 |         0
#> Petal.Width  | 1.20 | 0.76 | 1.50 | [0.10, 2.50] |    -0.10 |    -1.34 | 150 |         0
```

Or even just a variable

``` r
describe_distribution(mtcars$wt)
#> Mean |   SD |  IQR |        Range | Skewness | Kurtosis |  n | n_Missing
#> ------------------------------------------------------------------------
#> 3.22 | 0.98 | 1.19 | [1.51, 5.42] |     0.47 |     0.42 | 32 |         0
```

There are also some additional data properties that can be computed
using this package.

``` r
x <- (-10:10)^3 + rnorm(21, 0, 100)
smoothness(x, method = "diff")
#> [1] 1.791243
#> attr(,"class")
#> [1] "parameters_smoothness" "numeric"
```

## Function design and pipe-workflow

The design of the `{datawizard}` functions follows a design principle
that makes it easy for user to understand and remember how functions
work:

1.  the first argument is the data
2.  for methods that work on data frames, two arguments are following to
    `select` and `exclude` variables
3.  the following arguments are arguments related to the specific tasks
    of the functions

Most important, functions that accept data frames usually have this as
their first argument, and also return a (modified) data frame again.
Thus, `{datawizard}` integrates smoothly into a “pipe-workflow”.

``` r
iris |>
  # all rows where Species is "versicolor" or "virginica"
  data_filter(Species %in% c("versicolor", "virginica")) |>
  # select only columns with "." in names (i.e. drop Species)
  get_columns(contains(".")) |>
  # move columns that ends with "Length" to start of data frame
  data_relocate(ends_with("Length")) |>
  # remove fourth column
  data_remove(4) |>
  head()
#>    Sepal.Length Petal.Length Sepal.Width
#> 51          7.0          4.7         3.2
#> 52          6.4          4.5         3.2
#> 53          6.9          4.9         3.1
#> 54          5.5          4.0         2.3
#> 55          6.5          4.6         2.8
#> 56          5.7          4.5         2.8
```

# Contributing and Support

In case you want to file an issue or contribute in another way to the
package, please follow [this
guide](https://easystats.github.io/datawizard/CONTRIBUTING.html). For
questions about the functionality, you may either contact us via email
or also file an issue.

# Code of Conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://easystats.github.io/datawizard/CODE_OF_CONDUCT.html).
By participating in this project you agree to abide by its terms.
