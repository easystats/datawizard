# Rename columns and variable names

Safe and intuitive functions to rename variables or rows in data frames.
`data_rename()` will rename column names, i.e. it facilitates renaming
variables. `data_rename_rows()` is a convenient shortcut to add or
rename row names of a data frame, but unlike
[`row.names()`](https://rdrr.io/r/base/row.names.html), its input and
output is a data frame, thus, integrating smoothly into a possible
pipe-workflow.

## Usage

``` r
data_rename(data, select = NULL, replacement = NULL, ...)

data_rename_rows(data, rows = NULL)
```

## Arguments

- data:

  A data frame.

- select:

  Variables that will be included when performing the required tasks.
  Can be either

  - a variable specified as a literal variable name (e.g.,
    `column_name`),

  - a string with the variable name (e.g., `"column_name"`), a character
    vector of variable names (e.g., `c("col1", "col2", "col3")`), or a
    character vector of variable names including ranges specified via
    `:` (e.g., `c("col1:col3", "col5")`),

  - for some functions, like
    [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
    or `data_rename()`, `select` can be a named character vector. In
    this case, the names are used to rename the columns in the output
    data frame. See 'Details' in the related functions to see where this
    option applies.

  - a formula with variable names (e.g., `~column_1 + column_2`),

  - a vector of positive integers, giving the positions counting from
    the left (e.g. `1` or `c(1, 3, 5)`),

  - a vector of negative integers, giving the positions counting from
    the right (e.g., `-1` or `-1:-3`),

  - one of the following select-helpers:
    [`starts_with()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
    [`ends_with()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
    [`contains()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
    a range using `:`, or `regex()`.
    [`starts_with()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
    [`ends_with()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html),
    and
    [`contains()`](https://nathaneastwood.github.io/poorman/reference/select_helpers.html)
    accept several patterns, e.g `starts_with("Sep", "Petal")`.
    `regex()` can be used to define regular expression patterns.

  - a function testing for logical conditions, e.g.
    [`is.numeric()`](https://rdrr.io/r/base/numeric.html) (or
    `is.numeric`), or any user-defined function that selects the
    variables for which the function returns `TRUE` (like:
    `foo <- function(x) mean(x) > 3`),

  - ranges specified via literal variable names, select-helpers (except
    `regex()`) and (user-defined) functions can be negated, i.e. return
    non-matching elements, when prefixed with a `-`, e.g.
    `-ends_with()`, `-is.numeric` or `-(Sepal.Width:Petal.Length)`.
    **Note:** Negation means that matches are *excluded*, and thus, the
    `exclude` argument can be used alternatively. For instance,
    `select=-ends_with("Length")` (with `-`) is equivalent to
    `exclude=ends_with("Length")` (no `-`). In case negation should not
    work as expected, use the `exclude` argument instead.

  If `NULL`, selects all columns. Patterns that found no matches are
  silently ignored, e.g.
  `extract_column_names(iris, select = c("Species", "Test"))` will just
  return `"Species"`.

- replacement:

  Character vector. Can be one of the following:

  - A character vector that indicates the new names of the columns
    selected in `select`. `select` and `replacement` must be of the same
    length.

  - A string (i.e. character vector of length 1) with a "glue" styled
    pattern. Currently supported tokens are:

    - `{col}` which will be replaced by the column name, i.e. the
      corresponding value in `select`.

    - `{n}` will be replaced by the number of the variable that is
      replaced.

    - `{letter}` will be replaced by alphabetical letters in sequential
      order. If more than 26 letters are required, letters are repeated,
      but have sequential numeric indices (e.g., `a1` to `z1`, followed
      by `a2` to `z2`).

    - Finally, the name of a user-defined object that is available in
      the environment can be used. Note that the object's name is not
      allowed to be one of the pre-defined tokens, `"col"`, `"n"` and
      `"letter"`.

    An example for the use of tokens is...

        data_rename(
          mtcars,
          select = c("am", "vs"),
          replacement = "new_name_from_{col}"
        )

    ... which would return new column names `new_name_from_am` and
    `new_name_from_vs`. See 'Examples'.

  If `select` is a named vector, `replacement` is ignored.

- ...:

  Other arguments passed to or from other functions.

- rows:

  Vector of row names.

## Value

A modified data frame.

## Details

`select` can also be a named character vector. In this case, the names
are used to rename the columns in the output data frame. If you have a
named list, use [`unlist()`](https://rdrr.io/r/base/unlist.html) to
convert it to a named vector. See 'Examples'.

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

- Split and merge data frames:
  [`data_partition()`](https://easystats.github.io/datawizard/reference/data_partition.md),
  [`data_merge()`](https://easystats.github.io/datawizard/reference/data_merge.md)

- Functions to find or select columns:
  [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md),
  [`extract_column_names()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)

- Functions to filter rows:
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md),
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)

## Examples

``` r
# Rename columns
head(data_rename(iris, "Sepal.Length", "length"))
#>   length Sepal.Width Petal.Length Petal.Width Species
#> 1    5.1         3.5          1.4         0.2  setosa
#> 2    4.9         3.0          1.4         0.2  setosa
#> 3    4.7         3.2          1.3         0.2  setosa
#> 4    4.6         3.1          1.5         0.2  setosa
#> 5    5.0         3.6          1.4         0.2  setosa
#> 6    5.4         3.9          1.7         0.4  setosa

# Use named vector to rename
head(data_rename(iris, c(length = "Sepal.Length", width = "Sepal.Width")))
#>   length width Petal.Length Petal.Width Species
#> 1    5.1   3.5          1.4         0.2  setosa
#> 2    4.9   3.0          1.4         0.2  setosa
#> 3    4.7   3.2          1.3         0.2  setosa
#> 4    4.6   3.1          1.5         0.2  setosa
#> 5    5.0   3.6          1.4         0.2  setosa
#> 6    5.4   3.9          1.7         0.4  setosa

# Change all
head(data_rename(iris, replacement = paste0("Var", 1:5)))
#>   Var1 Var2 Var3 Var4   Var5
#> 1  5.1  3.5  1.4  0.2 setosa
#> 2  4.9  3.0  1.4  0.2 setosa
#> 3  4.7  3.2  1.3  0.2 setosa
#> 4  4.6  3.1  1.5  0.2 setosa
#> 5  5.0  3.6  1.4  0.2 setosa
#> 6  5.4  3.9  1.7  0.4 setosa

# Use glue-styled patterns
head(data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "formerly_{col}"))
#>                   formerly_mpg formerly_cyl formerly_disp
#> Mazda RX4                 21.0            6           160
#> Mazda RX4 Wag             21.0            6           160
#> Datsun 710                22.8            4           108
#> Hornet 4 Drive            21.4            6           258
#> Hornet Sportabout         18.7            8           360
#> Valiant                   18.1            6           225
head(data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "{col}_is_column_{n}"))
#>                   mpg_is_column_1 cyl_is_column_2 disp_is_column_3
#> Mazda RX4                    21.0               6              160
#> Mazda RX4 Wag                21.0               6              160
#> Datsun 710                   22.8               4              108
#> Hornet 4 Drive               21.4               6              258
#> Hornet Sportabout            18.7               8              360
#> Valiant                      18.1               6              225
head(data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "new_{letter}"))
#>                   new_a new_b new_c
#> Mazda RX4          21.0     6   160
#> Mazda RX4 Wag      21.0     6   160
#> Datsun 710         22.8     4   108
#> Hornet 4 Drive     21.4     6   258
#> Hornet Sportabout  18.7     8   360
#> Valiant            18.1     6   225

# User-defined glue-styled patterns from objects in environment
x <- c("hi", "there", "!")
head(data_rename(mtcars[1:3], c("mpg", "cyl", "disp"), "col_{x}"))
#>                   col_hi col_there col_!
#> Mazda RX4           21.0         6   160
#> Mazda RX4 Wag       21.0         6   160
#> Datsun 710          22.8         4   108
#> Hornet 4 Drive      21.4         6   258
#> Hornet Sportabout   18.7         8   360
#> Valiant             18.1         6   225
```
