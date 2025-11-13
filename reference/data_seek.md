# Find variables by their names, variable or value labels

This functions seeks variables in a data frame, based on patterns that
either match the variable name (column name), variable labels, value
labels or factor levels. Matching variable and value labels only works
for "labelled" data, i.e. when the variables either have a `label`
attribute or `labels` attribute.

`data_seek()` is particular useful for larger data frames with labelled
data - finding the correct variable name can be a challenge. This
function helps to find the required variables, when only certain
patterns of variable names or labels are known.

## Usage

``` r
data_seek(data, pattern, seek = c("names", "labels"), fuzzy = FALSE)
```

## Arguments

- data:

  A data frame.

- pattern:

  Character string (regular expression) to be matched in `data`. May
  also be a character vector of length \> 1. `pattern` is searched for
  in column names, variable label and value labels attributes, or factor
  levels of variables in `data`.

- seek:

  Character vector, indicating where `pattern` is sought. Use one or
  more of the following options:

  - `"names"`: Searches in column names. `"column_names"` and
    `"columns"` are aliases for `"names"`.

  - `"labels"`: Searches in variable labels. Only applies when a `label`
    attribute is set for a variable.

  - `"values"`: Searches in value labels or factor levels. Only applies
    when a `labels` attribute is set for a variable, or if a variable is
    a factor. `"levels"` is an alias for `"values"`.

  - `"all"`: Searches in all of the above.

- fuzzy:

  Logical. If `TRUE`, "fuzzy matching" (partial and close distance
  matching) will be used to find `pattern`.

## Value

A data frame with three columns: the column index, the column name and -
if available - the variable label of all matched variables in `data`.

## Examples

``` r
# seek variables with "Length" in variable name or labels
data_seek(iris, "Length")
#> index |       column |       labels
#> -----------------------------------
#>     1 | Sepal.Length | Sepal.Length
#>     3 | Petal.Length | Petal.Length

# seek variables with "dependency" in names or labels
# column "e42dep" has a label-attribute "elder's dependency"
data(efc)
data_seek(efc, "dependency")
#> index | column |             labels
#> -----------------------------------
#>     3 | e42dep | elder's dependency

# "female" only appears as value label attribute - default search is in
# variable names and labels only, so no match
data_seek(efc, "female")
#> No matches found.
# when we seek in all sources, we find the variable "e16sex"
data_seek(efc, "female", seek = "all")
#> index | column |         labels
#> -------------------------------
#>     2 | e16sex | elder's gender

# typo, no match
data_seek(iris, "Lenght")
#> No matches found.
# typo, fuzzy match
data_seek(iris, "Lenght", fuzzy = TRUE)
#> index |       column |       labels
#> -----------------------------------
#>     1 | Sepal.Length | Sepal.Length
#>     3 | Petal.Length | Petal.Length
```
