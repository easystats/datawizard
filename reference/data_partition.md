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
#> 1           4.7         3.2          1.3         0.2     setosa       3
#> 2           4.6         3.1          1.5         0.2     setosa       4
#> 3           4.8         3.4          1.6         0.2     setosa      12
#> 4           5.8         4.0          1.2         0.2     setosa      15
#> 5           5.2         3.4          1.4         0.2     setosa      29
#> 6           5.5         4.2          1.4         0.2     setosa      34
#> 7           5.1         3.8          1.6         0.2     setosa      47
#> 8           4.9         2.4          3.3         1.0 versicolor      58
#> 9           6.7         3.0          5.0         1.7 versicolor      78
#> 10          5.5         2.4          3.7         1.0 versicolor      82
#> 11          7.1         3.0          5.9         2.1  virginica     103
#> 12          6.7         3.3          5.7         2.1  virginica     125
#> 13          6.1         3.0          4.9         1.8  virginica     128
#> 14          7.2         3.0          5.8         1.6  virginica     130
#> 15          6.0         3.0          4.8         1.8  virginica     139
nrow(out$p_0.9)
#> [1] 135

# Stratify by group (equal proportions of each species)
out <- data_partition(iris, proportion = 0.9, by = "Species")
out$test
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width    Species .row_id
#> 1           5.4         3.7          1.5         0.2     setosa      11
#> 2           5.7         4.4          1.5         0.4     setosa      16
#> 3           5.2         4.1          1.5         0.1     setosa      33
#> 4           5.5         3.5          1.3         0.2     setosa      37
#> 5           5.1         3.4          1.5         0.2     setosa      40
#> 6           4.9         2.4          3.3         1.0 versicolor      58
#> 7           5.2         2.7          3.9         1.4 versicolor      60
#> 8           6.7         3.0          5.0         1.7 versicolor      78
#> 9           6.0         3.4          4.5         1.6 versicolor      86
#> 10          6.2         2.9          4.3         1.3 versicolor      98
#> 11          6.9         3.2          5.7         2.3  virginica     121
#> 12          7.7         2.8          6.7         2.0  virginica     123
#> 13          6.3         2.7          4.9         1.8  virginica     124
#> 14          6.1         3.0          4.9         1.8  virginica     128
#> 15          6.0         3.0          4.8         1.8  virginica     139

# Create multiple partitions
out <- data_partition(iris, proportion = c(0.3, 0.3))
lapply(out, head)
#> $p_0.3
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species .row_id
#> 1          4.7         3.2          1.3         0.2  setosa       3
#> 2          5.0         3.6          1.4         0.2  setosa       5
#> 3          5.4         3.9          1.7         0.4  setosa       6
#> 4          5.0         3.4          1.5         0.2  setosa       8
#> 5          4.9         3.1          1.5         0.1  setosa      10
#> 6          5.4         3.7          1.5         0.2  setosa      11
#> 
#> $p_0.3
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species .row_id
#> 1          4.6         3.4          1.4         0.3  setosa       7
#> 2          4.8         3.0          1.4         0.1  setosa      13
#> 3          5.7         4.4          1.5         0.4  setosa      16
#> 4          5.4         3.9          1.3         0.4  setosa      17
#> 5          5.1         3.5          1.4         0.3  setosa      18
#> 6          4.6         3.6          1.0         0.2  setosa      23
#> 
#> $test
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species .row_id
#> 1          5.1         3.5          1.4         0.2  setosa       1
#> 2          4.9         3.0          1.4         0.2  setosa       2
#> 3          4.6         3.1          1.5         0.2  setosa       4
#> 4          4.4         2.9          1.4         0.2  setosa       9
#> 5          4.3         3.0          1.1         0.1  setosa      14
#> 6          5.8         4.0          1.2         0.2  setosa      15
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
