# Assign variable and value labels

Assign variable and values labels to a variable or variables in a data
frame. Labels are stored as attributes (`"label"` for variable labels
and `"labels"`) for value labels.

## Usage

``` r
assign_labels(x, ...)

# S3 method for class 'numeric'
assign_labels(x, variable = NULL, values = NULL, ...)

# S3 method for class 'data.frame'
assign_labels(
  x,
  select = NULL,
  exclude = NULL,
  values = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- x:

  A data frame, factor or vector.

- ...:

  Currently not used.

- variable:

  The variable label as string.

- values:

  The value labels as (named) character vector. If `values` is *not* a
  named vector, the length of labels must be equal to the length of
  unique values. For a named vector, the left-hand side (LHS) is the
  value in `x`, the right-hand side (RHS) the associated value label.
  Non-matching labels are omitted.

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
    or
    [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md),
    `select` can be a named character vector. In this case, the names
    are used to rename the columns in the output data frame. See
    'Details' in the related functions to see where this option applies.

  - a formula with variable names (e.g., `~column_1 + column_2`),

  - a vector of positive integers, giving the positions counting from
    the left (e.g. `1` or `c(1, 3, 5)`),

  - a vector of negative integers, giving the positions counting from
    the right (e.g., `-1` or `-1:-3`),

  - one of the following select-helpers: `starts_with()`, `ends_with()`,
    `contains()`, a range using `:`, or `regex()`. `starts_with()`,
    `ends_with()`, and `contains()` accept several patterns, e.g
    `starts_with("Sep", "Petal")`. `regex()` can be used to define
    regular expression patterns.

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

- exclude:

  See `select`, however, column names matched by the pattern from
  `exclude` will be excluded instead of selected. If `NULL` (the
  default), excludes no columns.

- ignore_case:

  Logical, if `TRUE` and when one of the select-helpers or a regular
  expression is used in `select`, ignores lower/upper case in the search
  pattern when matching against variable names.

- regex:

  Logical, if `TRUE`, the search pattern from `select` will be treated
  as regular expression. When `regex = TRUE`, select *must* be a
  character string (or a variable containing a character string) and is
  not allowed to be one of the supported select-helpers or a character
  vector of length \> 1. `regex = TRUE` is comparable to using one of
  the two select-helpers, `select = contains()` or `select = regex()`,
  however, since the select-helpers may not work when called from inside
  other functions (see 'Details'), this argument may be used as
  workaround.

- verbose:

  Toggle warnings.

## Value

A labelled variable, or a data frame of labelled variables.

## Selection of variables - the `select` argument

For most functions that have a `select` argument (including this
function), the complete input data frame is returned, even when `select`
only selects a range of variables. That is, the function is only applied
to those variables that have a match in `select`, while all other
variables remain unchanged. In other words: for this function, `select`
will not omit any non-included variables, so that the returned data
frame will include all variables from the input data frame.

## Examples

``` r
x <- 1:3
# labelling by providing required number of labels
assign_labels(
  x,
  variable = "My x",
  values = c("one", "two", "three")
)
#> [1] 1 2 3
#> attr(,"label")
#> [1] "My x"
#> attr(,"labels")
#>   one   two three 
#>     1     2     3 

# labelling using named vectors
data(iris)
out <- assign_labels(
  iris$Species,
  variable = "Labelled Species",
  values = c(`setosa` = "Spec1", `versicolor` = "Spec2", `virginica` = "Spec3")
)
str(out)
#>  Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  - attr(*, "label")= chr "Labelled Species"
#>  - attr(*, "labels")= Named chr [1:3] "setosa" "versicolor" "virginica"
#>   ..- attr(*, "names")= chr [1:3] "Spec1" "Spec2" "Spec3"

# data frame example
out <- assign_labels(
  iris,
  select = "Species",
  variable = "Labelled Species",
  values = c(`setosa` = "Spec1", `versicolor` = "Spec2", `virginica` = "Spec3")
)
str(out$Species)
#>  Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...
#>  - attr(*, "label")= chr "Labelled Species"
#>  - attr(*, "labels")= Named chr [1:3] "setosa" "versicolor" "virginica"
#>   ..- attr(*, "names")= chr [1:3] "Spec1" "Spec2" "Spec3"

# Partial labelling
x <- 1:5
assign_labels(
  x,
  variable = "My x",
  values = c(`1` = "lowest", `5` = "highest")
)
#> [1] 1 2 3 4 5
#> attr(,"label")
#> [1] "My x"
#> attr(,"labels")
#>  lowest highest 
#>       1       5 
```
