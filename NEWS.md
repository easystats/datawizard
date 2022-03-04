# datawizard 0.3.0.9000

MAJOR CHANGES

The following functions are now re-exported from `{insight}` package:
`object_has_names()`, `object_has_rownames()`, `is_empty_object()`,
`compact_list()`, `compact_character()`

NEW FUNCTIONS

  * To convert rownames to a column, and vice-versa: `rownames_as_column()` and `column_as_rownames()` (@etiennebacher, #80).

CHANGES

  * `data_match()` gains a `match` argument, to indicate with which logical
    operation matching results should be combined.

  * `data_match()` gains a `as_data_frame` argument, to return the filtered
    data frame instead of the indices of filtered rows.

  * Improved support for *labelled data* for many functions, i.e. returned
    data frame will preserve value and variable label attributes, where
    possible and applicable.

BUG FIXES

  * `data_to_numeric()` produced wrong results for factors when 
    `dummy_factors = TRUE` and factor contained missing values.

  * `data_match()` produced wrong results when data contained missing values.

  * Fixed CRAN check issues in `data_extract()` when more than one variable
    was extracted from a data frame.

# datawizard 0.3.0

NEW FUNCTIONS

  * To find or remove empty rows and columns in a data frame: `empty_rows()`,
    `empty_columns()`, `remove_empty_rows()`, `remove_empty_columns()`, and
    `remove_empty`.

  * To check for names: `object_has_names()` and `object_has_rownames()`.

  * To rotate data frames: `data_rotate()`.
  
  * To reverse score variables: `data_reverse()`.
  
  * To merge/join multiple data frames: `data_merge()` (or its alias
    `data_join()`).

  * To cut (recode) data into groups: `data_cut()`.
  
  * To replace specific values with `NA`s: `convert_to_na()`.
  
  * To replace `Inf` and `NaN` values with `NA`s: `replace_nan_inf()`.

- Arguments `cols`, `before` and `after` in `data_relocate()` can now also be
  numeric values, indicating the position of the destination column.

# datawizard 0.2.3

- New functions:

  * to work with lists: `is_empty_object()` and `compact_list()`

  * to work with strings: `compact_character()`

# datawizard 0.2.2

- New function `data_extract()` (or its alias `extract()`) to pull single
  variables from a data frame, possibly naming each value by the row names of
  that data frame.

- `reshape_ci()` gains a `ci_type` argument, to reshape data frames where
  CI-columns have prefixes other than `"CI"`.

- `standardize()` and `center()` gain arguments `center` and `scale`, to define
  references for centrality and deviation that are used when centering or
  standardizing variables.

- `center()` gains the arguments `force` and `reference`, similar to
  `standardize()`.

- The functionality of the `append` argument in `center()` and `standardize()`
  was revised. This made the `suffix` argument redundant, and thus it was
  removed.

- Fixed issue in `standardize()`.

- Fixed issue in `data_findcols()`.

# datawizard 0.2.1

- Exports `plot` method for `visualisation_recipe()` objects from `{see}`
  package.

- `centre()`, `standardise()`, `unstandardise()` are exported as aliases for
  `center()`, `standardize()`, `unstandardize()`, respectively.

# datawizard 0.2.0.1

- This is mainly a maintenance release that addresses some issues with
  conflicting namespaces.

# datawizard 0.2.0

- New function: `visualisation_recipe()`.

- The following function has now moved to *performance* package:
  `check_multimodal()`.

- Minor updates to documentation, including a new vignette about `demean()`.

# datawizard 0.1.0

* First release.

