# Partition data

Creates data partitions (for instance, a training and a test set) based
on a data frame that can also be stratified (i.e., evenly spread a given
factor) using the `by` argument.

## Usage

``` r
data_partition(
  data,
  proportion = 0.7,
  by = NULL,
  seed = NULL,
  row_id = ".row_id",
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame.

- proportion:

  Scalar (between 0 and 1) or numeric vector, indicating the
  proportion(s) of the training set(s). The sum of `proportion` must not
  be greater than 1. The remaining part will be used for the test set.

- by:

  A character vector indicating the name(s) of the column(s) used for
  stratified partitioning.

- seed:

  A random number generator seed. Enter an integer (e.g. 123) so that
  the random sampling will be the same each time you run the function.

- row_id:

  Character string, indicating the name of the column that contains the
  row-id's.

- verbose:

  Toggle messages and warnings.

- ...:

  Other arguments passed to or from other functions.

## Value

A list of data frames. The list includes one training set per given
proportion and the remaining data as test set. List elements of training
sets are named after the given proportions (e.g., `$p_0.7`), the test
set is named `$test`.

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
  [`data_rotate()`](https://easystats.github.io/datawizard/reference/data_rotate.md)

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

- Split and merge data frames: `data_partition()`,
  [`data_merge()`](https://easystats.github.io/datawizard/reference/data_merge.md)

- Functions to find or select columns:
  [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md),
  [`extract_column_names()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)

- Functions to filter rows:
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md),
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)

## Examples

``` r
data(iris)
out <- data_partition(iris, proportion = 0.9)
out$test
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species .row_id
#> 1           4.8         3.4          1.6         0.2     setosa      12
#> 2           5.8         4.0          1.2         0.2     setosa      15
#> 3           4.8         3.1          1.6         0.2     setosa      31
#> 4           5.0         3.5          1.3         0.3     setosa      41
#> 5           6.0         2.2          4.0         1.0 versicolor      63
#> 6           5.6         2.9          3.6         1.3 versicolor      65
#> 7           6.7         3.1          4.4         1.4 versicolor      66
#> 8           6.3         2.5          4.9         1.5 versicolor      73
#> 9           5.5         2.4          3.8         1.1 versicolor      81
#> 10          5.7         2.9          4.2         1.3 versicolor      97
#> 11          6.3         3.3          6.0         2.5  virginica     101
#> 12          6.3         2.9          5.6         1.8  virginica     104
#> 13          6.5         3.0          5.8         2.2  virginica     105
#> 14          5.7         2.5          5.0         2.0  virginica     114
#> 15          6.9         3.1          5.1         2.3  virginica     142
nrow(out$p_0.9)
#> [1] 135

# Stratify by group (equal proportions of each species)
out <- data_partition(iris, proportion = 0.9, by = "Species")
out$test
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species .row_id
#> 1           5.8         4.0          1.2         0.2     setosa      15
#> 2           5.7         4.4          1.5         0.4     setosa      16
#> 3           5.7         3.8          1.7         0.3     setosa      19
#> 4           5.1         3.7          1.5         0.4     setosa      22
#> 5           4.4         3.0          1.3         0.2     setosa      39
#> 6           7.0         3.2          4.7         1.4 versicolor      51
#> 7           6.6         2.9          4.6         1.3 versicolor      59
#> 8           5.6         2.9          3.6         1.3 versicolor      65
#> 9           6.2         2.2          4.5         1.5 versicolor      69
#> 10          6.6         3.0          4.4         1.4 versicolor      76
#> 11          6.3         3.3          6.0         2.5  virginica     101
#> 12          6.5         3.0          5.8         2.2  virginica     105
#> 13          6.3         2.7          4.9         1.8  virginica     124
#> 14          7.2         3.2          6.0         1.8  virginica     126
#> 15          6.7         3.0          5.2         2.3  virginica     146

# Create multiple partitions
out <- data_partition(iris, proportion = c(0.3, 0.3))
lapply(out, head)
#> $p_0.3
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species .row_id
#> 1          4.7         3.2          1.3         0.2  setosa       3
#> 2          5.0         3.4          1.5         0.2  setosa       8
#> 3          4.4         2.9          1.4         0.2  setosa       9
#> 4          4.9         3.1          1.5         0.1  setosa      10
#> 5          5.4         3.7          1.5         0.2  setosa      11
#> 6          4.8         3.4          1.6         0.2  setosa      12
#> 
#> $p_0.3
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species .row_id
#> 1          5.0         3.6          1.4         0.2  setosa       5
#> 2          5.4         3.9          1.7         0.4  setosa       6
#> 3          4.6         3.4          1.4         0.3  setosa       7
#> 4          4.3         3.0          1.1         0.1  setosa      14
#> 5          5.4         3.9          1.3         0.4  setosa      17
#> 6          5.7         3.8          1.7         0.3  setosa      19
#> 
#> $test
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species .row_id
#> 1          5.1         3.5          1.4         0.2  setosa       1
#> 2          4.9         3.0          1.4         0.2  setosa       2
#> 3          4.6         3.1          1.5         0.2  setosa       4
#> 4          4.8         3.0          1.4         0.1  setosa      13
#> 5          5.8         4.0          1.2         0.2  setosa      15
#> 6          5.1         3.8          1.5         0.3  setosa      20
#> 

# Create multiple partitions, stratified by group - 30% equally sampled
# from species in first training set, 50% in second training set and
# remaining 20% equally sampled from each species in test set.
out <- data_partition(iris, proportion = c(0.3, 0.5), by = "Species")
lapply(out, function(i) table(i$Species))
#> $p_0.3
#> 
#>     setosa versicolor  virginica 
#>         15         15         15 
#> 
#> $p_0.5
#> 
#>     setosa versicolor  virginica 
#>         25         25         25 
#> 
#> $test
#> 
#>     setosa versicolor  virginica 
#>         10         10         10 
#> 
```
