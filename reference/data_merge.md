# Merge (join) two data frames, or a list of data frames

Merge (join) two data frames, or a list of data frames. However, unlike
base R's [`merge()`](https://rdrr.io/r/base/merge.html), `data_merge()`
offers a few more methods to join data frames, and it does not drop data
frame nor column attributes.

## Usage

``` r
data_merge(x, ...)

data_join(x, ...)

# S3 method for class 'data.frame'
data_merge(x, y, join = "left", by = NULL, id = NULL, verbose = TRUE, ...)

# S3 method for class 'list'
data_merge(x, join = "left", by = NULL, id = NULL, verbose = TRUE, ...)
```

## Arguments

- x, y:

  A data frame to merge. `x` may also be a list of data frames that will
  be merged. Note that the list-method has no `y` argument.

- ...:

  Not used.

- join:

  Character vector, indicating the method of joining the data frames.
  Can be `"full"`, `"left"` (default), `"right"`, `"inner"`, `"anti"`,
  `"semi"` or `"bind"`. See details below.

- by:

  Specifications of the columns used for merging.

- id:

  Optional name for ID column that will be created to indicate the
  source data frames for appended rows. Only applies if `join = "bind"`.

- verbose:

  Toggle warnings.

## Value

A merged data frame.

## Merging data frames

Merging data frames is performed by adding rows (cases), columns
(variables) or both from the source data frame (`y`) to the target data
frame (`x`). This usually requires one or more variables which are
included in both data frames and that are used for merging, typically
indicated with the `by` argument. When `by` contains a variable present
in both data frames, cases are matched and filtered by identical values
of `by` in `x` and `y`.

## Left- and right-joins

Left- and right joins usually don't add new rows (cases), but only new
columns (variables) for existing cases in `x`. For `join = "left"` or
`join = "right"` to work, `by` *must* indicate one or more columns that
are included in both data frames. For `join = "left"`, if `by` is an
identifier variable, which is included in both `x` and `y`, all
variables from `y` are copied to `x`, but only those cases from `y` that
have matching values in their identifier variable in `x` (i.e. all cases
in `x` that are also found in `y` get the related values from the new
columns in `y`). If there is no match between identifiers in `x` and
`y`, the copied variable from `y` will get a `NA` value for this
particular case. Other variables that occur both in `x` and `y`, but are
not used as identifiers (with `by`), will be renamed to avoid multiple
identical variable names. Cases in `y` where values from the identifier
have no match in `x`'s identifier are removed. `join = "right"` works in
a similar way as `join = "left"`, just that only cases from `x` that
have matching values in their identifier variable in `y` are chosen.

In base R, these are equivalent to `merge(x, y, all.x = TRUE)` and
`merge(x, y, all.y = TRUE)`.

## Full joins

Full joins copy all cases from `y` to `x`. For matching cases in both
data frames, values for new variables are copied from `y` to `x`. For
cases in `y` not present in `x`, these will be added as new rows to `x`.
Thus, full joins not only add new columns (variables), but also might
add new rows (cases).

In base R, this is equivalent to `merge(x, y, all = TRUE)`.

## Inner joins

Inner joins merge two data frames, however, only those rows (cases) are
kept that are present in both data frames. Thus, inner joins usually add
new columns (variables), but also remove rows (cases) that only occur in
one data frame.

In base R, this is equivalent to `merge(x, y)`.

## Binds

`join = "bind"` row-binds the complete second data frame `y` to `x`.
Unlike simple [`rbind()`](https://rdrr.io/r/base/cbind.html), which
requires the same columns for both data frames, `join = "bind"` will
bind shared columns from `y` to `x`, and add new columns from `y` to
`x`.

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
  `data_merge()`

- Functions to find or select columns:
  [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md),
  [`extract_column_names()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)

- Functions to filter rows:
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md),
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)

## Examples

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

# "by" will default to all shared columns, i.e. "c" and "id". new columns
# "d" and "e" will be copied from "y" to "x", but there are only two cases
# in "x" that have the same values for "c" and "id" in "y". only those cases
# have values in the copied columns, the other case gets "NA".
data_merge(x, y, join = "left")
#>   a b c id    d   e
#> 3 1 a 5  1 <NA>  NA
#> 1 2 b 6  2    f 100
#> 2 3 c 7  3    g 101

# we change the id-value here
x <- data.frame(a = 1:3, b = c("a", "b", "c"), c = 5:7, id = 1:3)
y <- data.frame(c = 6:8, d = c("f", "g", "h"), e = 100:102, id = 3:5)

x
#>   a b c id
#> 1 1 a 5  1
#> 2 2 b 6  2
#> 3 3 c 7  3
y
#>   c d   e id
#> 1 6 f 100  3
#> 2 7 g 101  4
#> 3 8 h 102  5

# no cases in "y" have the same matching "c" and "id" as in "x", thus
# copied variables from "y" to "x" copy no values, all get NA.
data_merge(x, y, join = "left")
#>   a b c id    d  e
#> 1 1 a 5  1 <NA> NA
#> 2 2 b 6  2 <NA> NA
#> 3 3 c 7  3 <NA> NA

# one case in "y" has a match in "id" with "x", thus values for this
# case from the remaining variables in "y" are copied to "x", all other
# values (cases) in those remaining variables get NA
data_merge(x, y, join = "left", by = "id")
#>   a b id    d   e c.x c.y
#> 2 1 a  1 <NA>  NA   5  NA
#> 3 2 b  2 <NA>  NA   6  NA
#> 1 3 c  3    f 100   7   6

data(mtcars)
x <- mtcars[1:5, 1:3]
y <- mtcars[28:32, 4:6]

# add ID common column
x$id <- 1:5
y$id <- 3:7

# left-join, add new variables and copy values from y to x,
# where "id" values match
data_merge(x, y)
#>    mpg cyl disp id  hp drat    wt
#> 4 21.0   6  160  1  NA   NA    NA
#> 5 21.0   6  160  2  NA   NA    NA
#> 1 22.8   4  108  3 113 3.77 1.513
#> 2 21.4   6  258  4 264 4.22 3.170
#> 3 18.7   8  360  5 175 3.62 2.770

# right-join, add new variables and copy values from x to y,
# where "id" values match
data_merge(x, y, join = "right")
#>    mpg cyl disp id  hp drat    wt
#> 1 22.8   4  108  3 113 3.77 1.513
#> 2 21.4   6  258  4 264 4.22 3.170
#> 3 18.7   8  360  5 175 3.62 2.770
#> 4   NA  NA   NA  6 335 3.54 3.570
#> 5   NA  NA   NA  7 109 4.11 2.780

# full-join
data_merge(x, y, join = "full")
#>    mpg cyl disp id  hp drat    wt
#> 4 21.0   6  160  1  NA   NA    NA
#> 5 21.0   6  160  2  NA   NA    NA
#> 1 22.8   4  108  3 113 3.77 1.513
#> 2 21.4   6  258  4 264 4.22 3.170
#> 3 18.7   8  360  5 175 3.62 2.770
#> 6   NA  NA   NA  6 335 3.54 3.570
#> 7   NA  NA   NA  7 109 4.11 2.780


data(mtcars)
x <- mtcars[1:5, 1:3]
y <- mtcars[28:32, c(1, 4:5)]

# add ID common column
x$id <- 1:5
y$id <- 3:7

# left-join, no matching rows (because columns "id" and "disp" are used)
# new variables get all NA values
data_merge(x, y)
#>    mpg cyl disp id hp drat
#> 1 21.0   6  160  1 NA   NA
#> 2 21.0   6  160  2 NA   NA
#> 3 22.8   4  108  3 NA   NA
#> 4 21.4   6  258  4 NA   NA
#> 5 18.7   8  360  5 NA   NA

# one common value in "mpg", so one row from y is copied to x
data_merge(x, y, by = "mpg")
#>    mpg cyl disp  hp drat id.x id.y
#> 2 21.0   6  160  NA   NA    1   NA
#> 3 21.0   6  160  NA   NA    2   NA
#> 4 22.8   4  108  NA   NA    3   NA
#> 1 21.4   6  258 109 4.11    4    7
#> 5 18.7   8  360  NA   NA    5   NA

# only keep rows with matching values in by-column
data_merge(x, y, join = "semi", by = "mpg")
#>                 mpg cyl disp id
#> Hornet 4 Drive 21.4   6  258  4

# only keep rows with non-matching values in by-column
data_merge(x, y, join = "anti", by = "mpg")
#>                    mpg cyl disp id
#> Mazda RX4         21.0   6  160  1
#> Mazda RX4 Wag     21.0   6  160  2
#> Datsun 710        22.8   4  108  3
#> Hornet Sportabout 18.7   8  360  5

# merge list of data frames. can be of different rows
x <- mtcars[1:5, 1:3]
y <- mtcars[28:31, 3:5]
z <- mtcars[11:18, c(1, 3:4, 6:8)]
x$id <- 1:5
y$id <- 4:7
z$id <- 3:10
data_merge(list(x, y, z), join = "bind", by = "id", id = "source")
#>     mpg cyl  disp id  hp drat    wt  qsec vs source
#> 1  21.0   6 160.0  1  NA   NA    NA    NA NA      1
#> 2  21.0   6 160.0  2  NA   NA    NA    NA NA      1
#> 3  22.8   4 108.0  3  NA   NA    NA    NA NA      1
#> 4  21.4   6 258.0  4  NA   NA    NA    NA NA      1
#> 5  18.7   8 360.0  5  NA   NA    NA    NA NA      1
#> 6    NA  NA  95.1  4 113 3.77    NA    NA NA      2
#> 7    NA  NA 351.0  5 264 4.22    NA    NA NA      2
#> 8    NA  NA 145.0  6 175 3.62    NA    NA NA      2
#> 9    NA  NA 301.0  7 335 3.54    NA    NA NA      2
#> 10 17.8  NA 167.6  3 123   NA 3.440 18.90  1      3
#> 11 16.4  NA 275.8  4 180   NA 4.070 17.40  0      3
#> 12 17.3  NA 275.8  5 180   NA 3.730 17.60  0      3
#> 13 15.2  NA 275.8  6 180   NA 3.780 18.00  0      3
#> 14 10.4  NA 472.0  7 205   NA 5.250 17.98  0      3
#> 15 10.4  NA 460.0  8 215   NA 5.424 17.82  0      3
#> 16 14.7  NA 440.0  9 230   NA 5.345 17.42  0      3
#> 17 32.4  NA  78.7 10  66   NA 2.200 19.47  1      3
```
