# A quick summary of selection syntax in \`{datawizard}\`

This vignette can be referred to by citing the following:

Patil et al., (2022). datawizard: An R Package for Easy Data Preparation
and Statistical Transformations. *Journal of Open Source Software*,
*7*(78), 4684, <https://doi.org/10.21105/joss.04684>

## Selecting variables

### Quoted names

This is the most simple way to select one or several variables. Just use
a character vector containing variables names, like in base R.

``` r

data_select(iris, c("Sepal.Length", "Petal.Width"))
#>    Sepal.Length Petal.Width
#> 1           4.3         0.1
#> 2           5.0         0.2
#> 3           7.7         2.2
#> 4           4.4         0.2
#> 5           5.9         1.8
#> 6           6.5         2.0
#> 7           5.5         1.3
#> 8           5.5         1.2
#> 9           5.8         1.9
#> 10          6.1         1.4
```

### Unquoted names

It is also possible to use unquoted names. This is useful if we use the
`tidyverse` and want to be consistent about the way variable names are
passed.

``` r

iris %>%
  group_by(Species) %>%
  standardise(Petal.Length) %>%
  ungroup()
#> # A tibble: 10 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species   
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>     
#>  1          4.3         3         -1.09          0.1 setosa    
#>  2          5           3.3        0.873         0.2 setosa    
#>  3          7.7         3.8        1.50          2.2 virginica 
#>  4          4.4         3.2        0.218         0.2 setosa    
#>  5          5.9         3         -0.542         1.8 virginica 
#>  6          6.5         3         -0.414         2   virginica 
#>  7          5.5         2.5       -1.09          1.3 versicolor
#>  8          5.5         2.6        0.218         1.2 versicolor
#>  9          5.8         2.7       -0.542         1.9 virginica 
#> 10          6.1         3          0.873         1.4 versicolor
```

### Positions

In addition to variable names, `select` can also take indices for the
variables to select in the dataframe.

``` r

data_select(iris, c(1, 2, 5))
#>    Sepal.Length Sepal.Width    Species
#> 1           4.3         3.0     setosa
#> 2           5.0         3.3     setosa
#> 3           7.7         3.8  virginica
#> 4           4.4         3.2     setosa
#> 5           5.9         3.0  virginica
#> 6           6.5         3.0  virginica
#> 7           5.5         2.5 versicolor
#> 8           5.5         2.6 versicolor
#> 9           5.8         2.7  virginica
#> 10          6.1         3.0 versicolor
```

### Functions

We can also pass a function to the `select` argument. This function will
be applied to all columns and should return `TRUE` or `FALSE`. For
example, if we want to keep only numeric columns, we can use
`is.numeric`.

``` r

data_select(iris, is.numeric)
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1           4.3         3.0          1.1         0.1
#> 2           5.0         3.3          1.4         0.2
#> 3           7.7         3.8          6.7         2.2
#> 4           4.4         3.2          1.3         0.2
#> 5           5.9         3.0          5.1         1.8
#> 6           6.5         3.0          5.2         2.0
#> 7           5.5         2.5          4.0         1.3
#> 8           5.5         2.6          4.4         1.2
#> 9           5.8         2.7          5.1         1.9
#> 10          6.1         3.0          4.6         1.4
```

Note that we can provide any custom function to `select`, *provided it
returns `TRUE` or `FALSE`* when applied to a column.

``` r

my_function <- function(i) {
  is.numeric(i) && mean(i, na.rm = TRUE) > 3.5
}

data_select(iris, my_function)
#>    Sepal.Length Petal.Length
#> 1           4.3          1.1
#> 2           5.0          1.4
#> 3           7.7          6.7
#> 4           4.4          1.3
#> 5           5.9          5.1
#> 6           6.5          5.2
#> 7           5.5          4.0
#> 8           5.5          4.4
#> 9           5.8          5.1
#> 10          6.1          4.6
```

### Patterns

With larger datasets, it would be tedious to write the names of
variables to select, and it would be fragile to rely on variable
positions as they may change later. To this end, we can use four select
helpers:
[`starts_with()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
[`ends_with()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
[`contains()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
and `regex()`. The first three can take several patterns, while
`regex()` takes a single regular expression.

``` r

data_select(iris, starts_with("Sep", "Peta"))
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1           4.3         3.0          1.1         0.1
#> 2           5.0         3.3          1.4         0.2
#> 3           7.7         3.8          6.7         2.2
#> 4           4.4         3.2          1.3         0.2
#> 5           5.9         3.0          5.1         1.8
#> 6           6.5         3.0          5.2         2.0
#> 7           5.5         2.5          4.0         1.3
#> 8           5.5         2.6          4.4         1.2
#> 9           5.8         2.7          5.1         1.9
#> 10          6.1         3.0          4.6         1.4

data_select(iris, ends_with("dth", "ies"))
#>    Sepal.Width Petal.Width    Species
#> 1          3.0         0.1     setosa
#> 2          3.3         0.2     setosa
#> 3          3.8         2.2  virginica
#> 4          3.2         0.2     setosa
#> 5          3.0         1.8  virginica
#> 6          3.0         2.0  virginica
#> 7          2.5         1.3 versicolor
#> 8          2.6         1.2 versicolor
#> 9          2.7         1.9  virginica
#> 10         3.0         1.4 versicolor

data_select(iris, contains("pal", "ec"))
#>    Sepal.Length Sepal.Width    Species
#> 1           4.3         3.0     setosa
#> 2           5.0         3.3     setosa
#> 3           7.7         3.8  virginica
#> 4           4.4         3.2     setosa
#> 5           5.9         3.0  virginica
#> 6           6.5         3.0  virginica
#> 7           5.5         2.5 versicolor
#> 8           5.5         2.6 versicolor
#> 9           5.8         2.7  virginica
#> 10          6.1         3.0 versicolor

data_select(iris, regex("^Sep|ies"))
#>    Sepal.Length Sepal.Width    Species
#> 1           4.3         3.0     setosa
#> 2           5.0         3.3     setosa
#> 3           7.7         3.8  virginica
#> 4           4.4         3.2     setosa
#> 5           5.9         3.0  virginica
#> 6           6.5         3.0  virginica
#> 7           5.5         2.5 versicolor
#> 8           5.5         2.6 versicolor
#> 9           5.8         2.7  virginica
#> 10          6.1         3.0 versicolor
```

Note: these functions are not exported by `datawizard` but are detected
and applied internally. This means that they won’t be detected by
autocompletion when we write them.

Note \#2: because these functions are not exported, they will not create
conflicts with the ones that come from the `tidyverse` and that have the
same name. Therefore, we can still use `dplyr` and its friends, it won’t
change anything for selection in `datawizard` functions!

## Excluding variables

What if we want to keep all variables except for a few ones? There are
two ways we can invert our selection.

The first way is to put a minus sign `"-"` in front of the `select`
argument.

``` r

data_select(iris, -c("Sepal.Length", "Petal.Width"))
#>    Sepal.Width Petal.Length    Species
#> 1          3.0          1.1     setosa
#> 2          3.3          1.4     setosa
#> 3          3.8          6.7  virginica
#> 4          3.2          1.3     setosa
#> 5          3.0          5.1  virginica
#> 6          3.0          5.2  virginica
#> 7          2.5          4.0 versicolor
#> 8          2.6          4.4 versicolor
#> 9          2.7          5.1  virginica
#> 10         3.0          4.6 versicolor

data_select(iris, -starts_with("Sep", "Peta"))
#>       Species
#> 1      setosa
#> 2      setosa
#> 3   virginica
#> 4      setosa
#> 5   virginica
#> 6   virginica
#> 7  versicolor
#> 8  versicolor
#> 9   virginica
#> 10 versicolor

data_select(iris, -is.numeric)
#>       Species
#> 1      setosa
#> 2      setosa
#> 3   virginica
#> 4      setosa
#> 5   virginica
#> 6   virginica
#> 7  versicolor
#> 8  versicolor
#> 9   virginica
#> 10 versicolor
```

Note that if we use numeric indices, we can’t mix negative and positive
values. This means that we have to use `select = -(1:2)` if we want to
exclude the first two columns; `select = -1:2` will *not* work:

``` r

data_select(iris, -(1:2))
#>    Petal.Length Petal.Width    Species
#> 1           1.1         0.1     setosa
#> 2           1.4         0.2     setosa
#> 3           6.7         2.2  virginica
#> 4           1.3         0.2     setosa
#> 5           5.1         1.8  virginica
#> 6           5.2         2.0  virginica
#> 7           4.0         1.3 versicolor
#> 8           4.4         1.2 versicolor
#> 9           5.1         1.9  virginica
#> 10          4.6         1.4 versicolor
```

Same thing for variable names:

``` r

data_select(iris, -(Petal.Length:Species))
#>    Sepal.Length Sepal.Width
#> 1           4.3         3.0
#> 2           5.0         3.3
#> 3           7.7         3.8
#> 4           4.4         3.2
#> 5           5.9         3.0
#> 6           6.5         3.0
#> 7           5.5         2.5
#> 8           5.5         2.6
#> 9           5.8         2.7
#> 10          6.1         3.0
```

The second way is to use the argument `exclude`. This argument has the
same possibilities as `select`. Although this may not be required in
most contexts, if we wanted to, we could use both `select` and `exclude`
arguments at the same time.

``` r

data_select(iris, exclude = c("Sepal.Length", "Petal.Width"))
#>    Sepal.Width Petal.Length    Species
#> 1          3.0          1.1     setosa
#> 2          3.3          1.4     setosa
#> 3          3.8          6.7  virginica
#> 4          3.2          1.3     setosa
#> 5          3.0          5.1  virginica
#> 6          3.0          5.2  virginica
#> 7          2.5          4.0 versicolor
#> 8          2.6          4.4 versicolor
#> 9          2.7          5.1  virginica
#> 10         3.0          4.6 versicolor

data_select(iris, exclude = starts_with("Sep", "Peta"))
#>       Species
#> 1      setosa
#> 2      setosa
#> 3   virginica
#> 4      setosa
#> 5   virginica
#> 6   virginica
#> 7  versicolor
#> 8  versicolor
#> 9   virginica
#> 10 versicolor
```

## Programming with selections

Since `datawizard` 0.6.0, it is possible to pass function arguments and
loop indices in `select` and `exclude` arguments. This makes it easier
to program with `datawizard`.

For example, if we want to let the user decide the selection they want
to use:

``` r

my_function <- function(data, selection) {
  extract_column_names(data, select = selection)
}
my_function(iris, "Sepal.Length")
#> [1] "Sepal.Length"
my_function(iris, starts_with("Sep"))
#> [1] "Sepal.Length" "Sepal.Width"

my_function_2 <- function(data, pattern) {
  extract_column_names(data, select = starts_with(pattern))
}
my_function_2(iris, "Sep")
#> [1] "Sepal.Length" "Sepal.Width"
```

It is also possible to pass these values in loops, for example if we
have a list of patterns and we want to relocate columns based on these
patterns, one by one:

``` r

new_iris <- iris
for (i in c("Sep", "Pet")) {
  new_iris <- new_iris %>%
    data_relocate(select = starts_with(i), after = -1)
}
new_iris
#>       Species Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1      setosa          4.3         3.0          1.1         0.1
#> 2      setosa          5.0         3.3          1.4         0.2
#> 3   virginica          7.7         3.8          6.7         2.2
#> 4      setosa          4.4         3.2          1.3         0.2
#> 5   virginica          5.9         3.0          5.1         1.8
#> 6   virginica          6.5         3.0          5.2         2.0
#> 7  versicolor          5.5         2.5          4.0         1.3
#> 8  versicolor          5.5         2.6          4.4         1.2
#> 9   virginica          5.8         2.7          5.1         1.9
#> 10 versicolor          6.1         3.0          4.6         1.4
```

In the loop above, all columns starting with `"Sep"` are moved at the
end of the data frame, and the same thing was made with all columns
starting with `"Pet"`.

## Useful to know

### Ignore the case

In every selection that uses variable names, we can ignore the case in
the selection by applying `ignore_case = TRUE`.

``` r

data_select(iris, c("sepal.length", "petal.width"), ignore_case = TRUE)
#>    Sepal.Length Petal.Width
#> 1           4.3         0.1
#> 2           5.0         0.2
#> 3           7.7         2.2
#> 4           4.4         0.2
#> 5           5.9         1.8
#> 6           6.5         2.0
#> 7           5.5         1.3
#> 8           5.5         1.2
#> 9           5.8         1.9
#> 10          6.1         1.4

data_select(iris, ~ Sepal.length + petal.Width, ignore_case = TRUE)
#>    Sepal.Length Petal.Width
#> 1           4.3         0.1
#> 2           5.0         0.2
#> 3           7.7         2.2
#> 4           4.4         0.2
#> 5           5.9         1.8
#> 6           6.5         2.0
#> 7           5.5         1.3
#> 8           5.5         1.2
#> 9           5.8         1.9
#> 10          6.1         1.4

data_select(iris, starts_with("sep", "peta"), ignore_case = TRUE)
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width
#> 1           4.3         3.0          1.1         0.1
#> 2           5.0         3.3          1.4         0.2
#> 3           7.7         3.8          6.7         2.2
#> 4           4.4         3.2          1.3         0.2
#> 5           5.9         3.0          5.1         1.8
#> 6           6.5         3.0          5.2         2.0
#> 7           5.5         2.5          4.0         1.3
#> 8           5.5         2.6          4.4         1.2
#> 9           5.8         2.7          5.1         1.9
#> 10          6.1         3.0          4.6         1.4
```

### Formulas

It is also possible to use formulas to select variables:

``` r

data_select(iris, ~ Sepal.Length + Petal.Width)
#>    Sepal.Length Petal.Width
#> 1           4.3         0.1
#> 2           5.0         0.2
#> 3           7.7         2.2
#> 4           4.4         0.2
#> 5           5.9         1.8
#> 6           6.5         2.0
#> 7           5.5         1.3
#> 8           5.5         1.2
#> 9           5.8         1.9
#> 10          6.1         1.4
```

This made it easier to use selection in custom functions before
`datawizard` 0.6.0, and is kept available for backward compatibility.
