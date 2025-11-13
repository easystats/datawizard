# Create new variables in a data frame

Create new variables or modify existing variables in a data frame.
Unlike [`base::transform()`](https://rdrr.io/r/base/transform.html),
`data_modify()` can be used on grouped data frames, and newly created
variables can be directly used.

## Usage

``` r
data_modify(data, ...)

# S3 method for class 'data.frame'
data_modify(data, ..., .if = NULL, .at = NULL, .modify = NULL)
```

## Arguments

- data:

  A data frame

- ...:

  One or more expressions that define the new variable name and the
  values or recoding of those new variables. These expressions can be
  one of:

  - A sequence of named, literal expressions, where the left-hand side
    refers to the name of the new variable, while the right-hand side
    represent the values of the new variable. Example:
    `Sepal.Width = center(Sepal.Width)`.

  - A vector of length 1 (which will be recycled to match the number of
    rows in the data), or of same length as the data.

  - A variable that contains a value to be used. Example:

        a <- "abc"
        data_modify(iris, var_abc = a) # var_abc contains "abc"

  - An expression can also be provided as string and wrapped in
    `as_expr()`. Example:

        data_modify(iris, as_expr("Sepal.Width = center(Sepal.Width)"))
        # or
        a <- "center(Sepal.Width)"
        data_modify(iris, Sepal.Width = as_expr(a))
        # or
        a <- "Sepal.Width = center(Sepal.Width)"
        data_modify(iris, as_expr(a))

    Note that `as_expr()` is no real function, which cannot be used
    outside of `data_modify()`, and hence it is not exported nor
    documented. Rather, it is only used for internally processing
    expressions.

  - Using `NULL` as right-hand side removes a variable from the data
    frame. Example: `Petal.Width = NULL`.

  - For data frames (including grouped ones), the function
    [`n()`](https://nathaneastwood.github.io/poorman/reference/context.html)
    can be used to count the number of observations and thereby, for
    instance, create index values by using `id = 1:n()` or
    `id = 3:(n()+2)` and similar. Note that, like `as_expr()`,
    [`n()`](https://nathaneastwood.github.io/poorman/reference/context.html)
    is also no true function and cannot be used outside of
    `data_modify()`.

  Note that newly created variables can be used in subsequent
  expressions, including `.at` or `.if`. See also 'Examples'.

- .if:

  A function that returns `TRUE` for columns in the data frame where
  `.if` applies. This argument is used in combination with the `.modify`
  argument. Note that only one of `.at` or `.if` can be provided, but
  not both at the same time. Newly created variables in `...` can also
  be selected, see 'Examples'.

- .at:

  A character vector of variable names that should be modified. This
  argument is used in combination with the `.modify` argument. Note that
  only one of `.at` or `.if` can be provided, but not both at the same
  time. Newly created variables in `...` can also be selected, see
  'Examples'.

- .modify:

  A function that modifies the variables defined in `.at` or `.if`. This
  argument is used in combination with either the `.at` or the `.if`
  argument. Note that the modified variable (i.e. the result from
  `.modify`) must be either of length 1 or of same length as the input
  variable.

## Note

`data_modify()` can also be used inside functions. However, it is
recommended to pass the recode-expression as character vector or list of
characters.

## Examples

``` r
data(efc)
new_efc <- data_modify(
  efc,
  c12hour_c = center(c12hour),
  c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE),
  c12hour_z2 = standardize(c12hour)
)
head(new_efc)
#>   c12hour e16sex e42dep c172code neg_c_7 c12hour_c  c12hour_z c12hour_z2
#> 1      16      2      3        2      12 -69.65306 -1.0466657 -1.0466657
#> 2     148      2      3        2      20  62.34694  0.9368777  0.9368777
#> 3      70      2      3        1      11 -15.65306 -0.2352161 -0.2352161
#> 4      NA      2   <NA>        2      10        NA         NA         NA
#> 5     168      2      4        2      12  82.34694  1.2374146  1.2374146
#> 6      16      2      4        2      19 -69.65306 -1.0466657 -1.0466657

# using strings instead of literal expressions
new_efc <- data_modify(
  efc,
  as_expr("c12hour_c = center(c12hour)"),
  as_expr("c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)"),
  as_expr("c12hour_z2 = standardize(c12hour)")
)
head(new_efc)
#>   c12hour e16sex e42dep c172code neg_c_7 c12hour_c  c12hour_z c12hour_z2
#> 1      16      2      3        2      12 -69.65306 -1.0466657 -1.0466657
#> 2     148      2      3        2      20  62.34694  0.9368777  0.9368777
#> 3      70      2      3        1      11 -15.65306 -0.2352161 -0.2352161
#> 4      NA      2   <NA>        2      10        NA         NA         NA
#> 5     168      2      4        2      12  82.34694  1.2374146  1.2374146
#> 6      16      2      4        2      19 -69.65306 -1.0466657 -1.0466657

# using a character vector, provided a variable
xpr <- c(
  "c12hour_c = center(c12hour)",
  "c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE)",
  "c12hour_z2 = standardize(c12hour)"
)
new_efc <- data_modify(efc, as_expr(xpr))
head(new_efc)
#>   c12hour e16sex e42dep c172code neg_c_7 c12hour_c  c12hour_z c12hour_z2
#> 1      16      2      3        2      12 -69.65306 -1.0466657 -1.0466657
#> 2     148      2      3        2      20  62.34694  0.9368777  0.9368777
#> 3      70      2      3        1      11 -15.65306 -0.2352161 -0.2352161
#> 4      NA      2   <NA>        2      10        NA         NA         NA
#> 5     168      2      4        2      12  82.34694  1.2374146  1.2374146
#> 6      16      2      4        2      19 -69.65306 -1.0466657 -1.0466657

# using character strings, provided as variable
stand <- "c12hour_c / sd(c12hour, na.rm = TRUE)"
new_efc <- data_modify(
  efc,
  c12hour_c = center(c12hour),
  c12hour_z = as_expr(stand)
)
head(new_efc)
#>   c12hour e16sex e42dep c172code neg_c_7 c12hour_c  c12hour_z
#> 1      16      2      3        2      12 -69.65306 -1.0466657
#> 2     148      2      3        2      20  62.34694  0.9368777
#> 3      70      2      3        1      11 -15.65306 -0.2352161
#> 4      NA      2   <NA>        2      10        NA         NA
#> 5     168      2      4        2      12  82.34694  1.2374146
#> 6      16      2      4        2      19 -69.65306 -1.0466657

# attributes - in this case, value and variable labels - are preserved
str(new_efc)
#> 'data.frame':    100 obs. of  7 variables:
#>  $ c12hour  : num  16 148 70 NA 168 16 161 110 28 40 ...
#>   ..- attr(*, "label")= chr "average number of hours of care per week"
#>  $ e16sex   : num  2 2 2 2 2 2 1 2 2 2 ...
#>   ..- attr(*, "label")= chr "elder's gender"
#>   ..- attr(*, "labels")= Named num [1:2] 1 2
#>   .. ..- attr(*, "names")= chr [1:2] "male" "female"
#>  $ e42dep   : Factor w/ 4 levels "1","2","3","4": 3 3 3 NA 4 4 4 4 4 4 ...
#>   ..- attr(*, "labels")= Named num [1:4] 1 2 3 4
#>   .. ..- attr(*, "names")= chr [1:4] "independent" "slightly dependent" "moderately dependent" "severely dependent"
#>   ..- attr(*, "label")= chr "elder's dependency"
#>  $ c172code : num  2 2 1 2 2 2 2 2 NA 2 ...
#>   ..- attr(*, "label")= chr "carer's level of education"
#>   ..- attr(*, "labels")= Named num [1:3] 1 2 3
#>   .. ..- attr(*, "names")= chr [1:3] "low level of education" "intermediate level of education" "high level of education"
#>  $ neg_c_7  : num  12 20 11 10 12 19 15 11 15 10 ...
#>   ..- attr(*, "label")= chr "Negative impact with 7 items"
#>  $ c12hour_c: 'dw_transformer' num  -69.7 62.3 -15.7 NA 82.3 ...
#>   ..- attr(*, "center")= num 85.7
#>   ..- attr(*, "scale")= num 1
#>   ..- attr(*, "robust")= logi FALSE
#>   ..- attr(*, "label")= chr "average number of hours of care per week"
#>  $ c12hour_z: 'dw_transformer' num  -1.047 0.937 -0.235 NA 1.237 ...
#>   ..- attr(*, "center")= num 85.7
#>   ..- attr(*, "scale")= num 1
#>   ..- attr(*, "robust")= logi FALSE
#>   ..- attr(*, "label")= chr "average number of hours of care per week"

# using `paste()` to build a string-expression
to_standardize <- c("Petal.Length", "Sepal.Length")
out <- data_modify(
  iris,
  as_expr(
    paste0(to_standardize, "_stand = standardize(", to_standardize, ")")
  )
)
head(out)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species Petal.Length_stand
#> 1          5.1         3.5          1.4         0.2  setosa          -1.335752
#> 2          4.9         3.0          1.4         0.2  setosa          -1.335752
#> 3          4.7         3.2          1.3         0.2  setosa          -1.392399
#> 4          4.6         3.1          1.5         0.2  setosa          -1.279104
#> 5          5.0         3.6          1.4         0.2  setosa          -1.335752
#> 6          5.4         3.9          1.7         0.4  setosa          -1.165809
#>   Sepal.Length_stand
#> 1         -0.8976739
#> 2         -1.1392005
#> 3         -1.3807271
#> 4         -1.5014904
#> 5         -1.0184372
#> 6         -0.5353840

# overwrite existing variable, remove old variable
out <- data_modify(iris, Petal.Length = 1 / Sepal.Length, Sepal.Length = NULL)
head(out)
#>   Sepal.Width Petal.Length Petal.Width Species
#> 1         3.5    0.1960784         0.2  setosa
#> 2         3.0    0.2040816         0.2  setosa
#> 3         3.2    0.2127660         0.2  setosa
#> 4         3.1    0.2173913         0.2  setosa
#> 5         3.6    0.2000000         0.2  setosa
#> 6         3.9    0.1851852         0.4  setosa

# works on grouped data
grouped_efc <- data_group(efc, "c172code")
new_efc <- data_modify(
  grouped_efc,
  c12hour_c = center(c12hour),
  c12hour_z = c12hour_c / sd(c12hour, na.rm = TRUE),
  c12hour_z2 = standardize(c12hour),
  id = 1:n()
)
head(new_efc)
#> # A tibble: 6 × 9
#> # Groups:   c172code [2]
#>   c12hour e16sex e42dep c172code neg_c_7 c12hour_c c12hour_z c12hour_z2    id
#>     <dbl>  <dbl> <fct>     <dbl>   <dbl>     <dbl>     <dbl>      <dbl> <int>
#> 1      16      2 3             2      12     -78.0    -1.16      -1.16      1
#> 2     148      2 3             2      20      54.0     0.804      0.804     2
#> 3      70      2 3             1      11     -17.1    -0.250     -0.250     1
#> 4      NA      2 NA            2      10      NA      NA         NA         3
#> 5     168      2 4             2      12      74.0     1.10       1.10      4
#> 6      16      2 4             2      19     -78.0    -1.16      -1.16      5

# works from inside functions
foo1 <- function(data, ...) {
  head(data_modify(data, ...))
}
foo1(iris, SW_fraction = Sepal.Width / 10)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species SW_fraction
#> 1          5.1         3.5          1.4         0.2  setosa        0.35
#> 2          4.9         3.0          1.4         0.2  setosa        0.30
#> 3          4.7         3.2          1.3         0.2  setosa        0.32
#> 4          4.6         3.1          1.5         0.2  setosa        0.31
#> 5          5.0         3.6          1.4         0.2  setosa        0.36
#> 6          5.4         3.9          1.7         0.4  setosa        0.39
# or
foo1(iris, as_expr("SW_fraction = Sepal.Width / 10"))
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species SW_fraction
#> 1          5.1         3.5          1.4         0.2  setosa        0.35
#> 2          4.9         3.0          1.4         0.2  setosa        0.30
#> 3          4.7         3.2          1.3         0.2  setosa        0.32
#> 4          4.6         3.1          1.5         0.2  setosa        0.31
#> 5          5.0         3.6          1.4         0.2  setosa        0.36
#> 6          5.4         3.9          1.7         0.4  setosa        0.39

# also with string arguments, using `as_expr()`
foo2 <- function(data, modification) {
  head(data_modify(data, as_expr(modification)))
}
foo2(iris, "SW_fraction = Sepal.Width / 10")
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species SW_fraction
#> 1          5.1         3.5          1.4         0.2  setosa        0.35
#> 2          4.9         3.0          1.4         0.2  setosa        0.30
#> 3          4.7         3.2          1.3         0.2  setosa        0.32
#> 4          4.6         3.1          1.5         0.2  setosa        0.31
#> 5          5.0         3.6          1.4         0.2  setosa        0.36
#> 6          5.4         3.9          1.7         0.4  setosa        0.39

# modify at specific positions or if condition is met
d <- iris[1:5, ]
data_modify(d, .at = "Species", .modify = as.numeric)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2       1
#> 2          4.9         3.0          1.4         0.2       1
#> 3          4.7         3.2          1.3         0.2       1
#> 4          4.6         3.1          1.5         0.2       1
#> 5          5.0         3.6          1.4         0.2       1
data_modify(d, .if = is.factor, .modify = as.numeric)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#> 1          5.1         3.5          1.4         0.2       1
#> 2          4.9         3.0          1.4         0.2       1
#> 3          4.7         3.2          1.3         0.2       1
#> 4          4.6         3.1          1.5         0.2       1
#> 5          5.0         3.6          1.4         0.2       1

# can be combined with dots
data_modify(d, new_length = Petal.Length * 2, .at = "Species", .modify = as.numeric)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species new_length
#> 1          5.1         3.5          1.4         0.2       1        2.8
#> 2          4.9         3.0          1.4         0.2       1        2.8
#> 3          4.7         3.2          1.3         0.2       1        2.6
#> 4          4.6         3.1          1.5         0.2       1        3.0
#> 5          5.0         3.6          1.4         0.2       1        2.8

# new variables used in `.at` or `.if`
data_modify(
  d,
  new_length = Petal.Length * 2,
  .at = c("Petal.Length", "new_length"),
  .modify = round
)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species new_length
#> 1          5.1         3.5            1         0.2  setosa          3
#> 2          4.9         3.0            1         0.2  setosa          3
#> 3          4.7         3.2            1         0.2  setosa          3
#> 4          4.6         3.1            2         0.2  setosa          3
#> 5          5.0         3.6            1         0.2  setosa          3

# combine "extract_column_names()" and ".at" argument
out <- data_modify(
  d,
  .at = extract_column_names(d, select = starts_with("Sepal")),
  .modify = as.factor
)
# "Sepal.Length" and "Sepal.Width" are now factors
str(out)
#> 'data.frame':    5 obs. of  5 variables:
#>  $ Sepal.Length: Factor w/ 5 levels "4.6","4.7","4.9",..: 5 3 2 1 4
#>  $ Sepal.Width : Factor w/ 5 levels "3","3.1","3.2",..: 4 1 3 2 5
#>  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4
#>  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2
#>  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1
```
