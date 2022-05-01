# datawizard 0.4.1

BREAKING

* Added the `standardize.default()` method (moved from package **effectsize**),
  to be consistent in that the default-method now is in the same package as the
  generic. `standardize.default()` behaves exactly like in **effectsize** and
  particularly works for regression model objects. **effectsize** now re-exports
  `standardize()` from **datawizard**.

NEW FUNCTIONS

* `data_shift()` to shift the value range of numeric variables.

* `data_recode()` to recode old into new values.

* `data_to_factor()` as counterpart to `data_to_numeric()`.

* `data_tabulate()` to create frequency tables of variables.

* `data_read()` to read (import) data files (from text, or foreign statistical
  packages).

* `unnormalize()` as counterpart to `normalize()`. This function only works for 
  variables that have been normalized with `normalize()`.

* `data_group()` and `data_ungroup()` to create grouped data frames, or to remove
  the grouping information from grouped data frames.

CHANGES

* `data_find()` was added as alias to `find_colums()`, to have consistent
  name patterns for the **datawizard** functions. `data_findcols()` will be
  removed in a future update and usage is discouraged.

* The `select` argument (and thus, also the `exclude` argument) now also 
  accepts functions testing for logical conditions, e.g. `is.numeric()` (or
  `is.numeric`), or any user-defined function that selects the variables for 
  which the function returns `TRUE` (like: `foo <- function(x) mean(x) > 3`).

* Arguments `select` and `exclude` now allow the negation of select-helpers, 
  like `-ends_with("")`, `-is.numeric` or `-Sepal.Width:Petal.Length`.

* Many functions now get a `.default` method, to capture unsupported classes. 
  This now yields a message and returns the original input, and hence, the 
  `.data.frame` methods won't stop due to an error.

* The `filter` argument in `data_filter()` can also be a numeric vector, to
  indicate row indices of those rows that should be returned.
  
* `convert_to_na()` gets methods for variables of class `logical` and `Date`.

* `convert_to_na()` for factors (and data frames) gains a `drop_levels` argument,
  to drop unused levels that have been replaced by `NA`.

* `data_to_numeric()` gains two more arguments, `preserve_levels` and `lowest`,
  to give better control of conversion of factors.

BUG FIXES

* When logicals were passed to `center()` or `standardize()` and `force = TRUE`,
  these were not properly converted to numeric variables.

# datawizard 0.4.0

MAJOR CHANGES

* `data_match()` now returns filtered data by default. Old behavior (returning 
  rows indices) can be set by setting `return_indices = TRUE`.

* The following functions are now re-exported from `{insight}` package:
  `object_has_names()`, `object_has_rownames()`, `is_empty_object()`,
  `compact_list()`, `compact_character()`

* `data_findcols()` will become deprecated in future updates. Please use the
  new replacements `find_columns()` and `get_columns()`.

* The vignette *Analysing Longitudinal or Panel Data* has now moved to 
  [parameters package](https://easystats.github.io/parameters/articles/demean.html).

NEW FUNCTIONS

* To convert rownames to a column, and *vice versa*: `rownames_as_column()` 
  and `column_as_rownames()` (@etiennebacher, #80).

* `find_columns()` and `get_columns()` to find column names or retrieve
  subsets of data frames, based on various select-methods (including
  select-helpers). These function will supersede `data_findcols()` in the
  future.

* `data_filter()` as complement for `data_match()`, which works with logical 
  expressions for filtering rows of data frames.

* For computing weighted centrality measures and dispersion: `weighted_mean()`,
  `weighted_median()`, `weighted_sd()` and `weighted_mad()`.
  
* To replace `NA` in vectors and dataframes: `convert_na_to()` (@etiennebacher, #111).

MINOR CHANGES

* The `select` argument in several functions (like `data_remove()`,
  `reshape_longer()`, or  `data_extract()`) now allows the use of select-helpers
  for selecting variables based on specific patterns.

* `data_extract()` gains new arguments to allow type-safe return values,
   i.e. *always* return a vector *or* a data frame. Thus, `data_extract()`
   can now be used to select multiple variables or pull a single variable
   from data frames.

* `data_match()` gains a `match` argument, to indicate with which logical
  operation matching results should be combined.

* Improved support for *labelled data* for many functions, i.e. returned
  data frame will preserve value and variable label attributes, where
  possible and applicable.

* `describe_distribution()` now works with lists (@etiennebacher, #105).

* `data_rename()` doesn't use `pattern` anymore to rename the columns if
  `replacement` is not provided (@etiennebacher, #103).

* `data_rename()` now adds a suffix to duplicated names in `replacement`
  (@etiennebacher, #103).

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

