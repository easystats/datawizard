# datawizard 0.6.5

MAJOR CHANGES

* Etienne Bacher is the new maintainer.

MINOR CHANGES

* `standardize()`, `center()`, `normalize()` and `rescale()` can be used in
  model formulas, similar to `base::scale()`.

* `data_codebook()` now includes the proportion for each category/value, in
  addition to the counts. Furthermore, if data contains tagged `NA` values,
  these are included in the frequency table.

BUG FIXES

* `center(x)` now works correctly when `x` is a single value and either
  `reference` or `center` is specified (#324).

* Fixed issue in `data_codebook()`, which failed for labelled vectors when
  values of labels were not in sorted order.

# datawizard 0.6.4

NEW FUNCTIONS

* `data_codebook()`: to generate codebooks of data frames.

* New functions to deal with duplicates: `data_duplicated()` (keep all duplicates,
  including the first occurrence) and `data_unique()` (returns the data, excluding 
  all duplicates except one instance of each, based on the selected method).

MINOR CHANGES

* `.data.frame` methods should now preserve custom attributes.

* The `include_bounds` argument in `normalize()` can now also be a numeric
  value, defining the limit to the upper and lower bound (i.e. the distance
  to 1 and 0).
  
* `data_filter()` now works with grouped data. 

BUG FIXES

* `data_read()` no longer prints message for empty columns when the data
  actually had no empty columns.
  
 * `data_to_wide()` now drops columns that are not in `id_cols` (if specified), 
  `names_from`, or `values_from`. This is the behaviour observed in `tidyr::pivot_wider()`.

# datawizard 0.6.3

MAJOR CHANGES

* There is a new publication about the `{datawizard}` package:
  <https://joss.theoj.org/papers/10.21105/joss.04684>

* Fixes failing tests due to changes in `R-devel`.

* `data_to_long()` and `data_to_wide()` have had significant performance
  improvements, sometimes as high as a ten-fold speedup.

MINOR CHANGES

* When column names are misspelled, most functions now suggest which existing
  columns possibly could be meant.

* Miscellaneous performance gains.

* `convert_to_na()` now requires argument `na` to be of class 'Date' to convert
  specific dates to `NA`. For example, `convert_to_na(x, na = "2022-10-17")`
  must be changed to `convert_to_na(x, na = as.Date("2022-10-17"))`.

BUG FIXES

* `data_to_long()` and `data_to_wide()` now correctly keep the `date` format.

# datawizard 0.6.2

BREAKING CHANGES

* Methods for grouped data frames (`.grouped_df`) no longer support
  `dplyr::group_by()` for `{dplyr}` before version `0.8.0`.

* `empty_columns()` and `remove_empty_columns()` now also remove columns that
  contain only empty characters. Likewise, `empty_rows()` and
  `remove_empty_rows()` remove observations that completely have missing or
  empty character values.

MINOR CHANGES

* `data_read()` gains a `convert_factors` argument, to turn off automatic
  conversion from numeric variables into factors.

BUG FIXES

* `data_arrange()` now works with data frames that were grouped using
  `data_group()` (#274).

# datawizard 0.6.1

* Updates tests for upcoming changes in the `{tidyselect}` package (#267).

# datawizard 0.6.0

BREAKING CHANGES

* The minimum needed R version has been bumped to `3.6`.

* Following deprecated functions have been removed:

`data_cut()`, `data_recode()`, `data_shift()`, `data_reverse()`,
`data_rescale()`, `data_to_factor()`, `data_to_numeric()`

* New `text_format()` alias is introduced for `format_text()`, latter of which
  will be removed in the next release.

* New `recode_values()` alias is introduced for `change_code()`, latter of which
  will be removed in the next release.

* `data_merge()` now errors if columns specified in `by` are not in both
  datasets.

* Using negative values in arguments `select` and `exclude` now removes the
  columns from the selection/exclusion. The previous behavior was to start the
  selection/exclusion from the end of the dataset, which was inconsistent with
  the use of "-" with other selecting possibilities.

NEW FUNCTIONS

* `data_peek()`: to peek at values and type of variables in a data frame.

* `coef_var()`: to compute the coefficient of variation.

CHANGES

* `data_filter()` will give more informative messages on malformed syntax of the
  `filter` argument.

* It is now possible to use curly brackets to pass variable names to
  `data_filter()`, like the following example. See examples section in the
  documentation of `data_filter()`.

* The `regex` argument was added to functions that use select-helpers and did
  not already have this argument.

* Select helpers `starts_with()`, `ends_with()`, and `contains()` now accept
  several patterns, e.g `starts_with("Sep", "Petal")`.

* Arguments `select` and `exclude` that are present in most functions have been
  improved to work in loops and in custom functions. For example, the following
  code now works:

```r
foo <- function(data) {
  i <- "Sep"
  find_columns(data, select = starts_with(i))
}
foo(iris)

for (i in c("Sepal", "Sp")) {
  head(iris) |>
    find_columns(select = starts_with(i)) |>
    print()
}
```

* There is now a vignette summarizing the various ways to select or exclude
  variables in most `{datawizard}` functions.

# datawizard 0.5.1

* Fixes failing tests due to `{poorman}` update.

# datawizard 0.5.0

MAJOR CHANGES

* Following statistical transformation functions have been renamed to not have
  `data_*()` prefix, since they do not work exclusively with data frames, but
  are typically first of all used with vectors, and therefore had misleading
  names:

  - `data_cut()` -> `categorize()`

  - `data_recode()` -> `change_code()`

  - `data_shift()` -> `slide()`

  - `data_reverse()` -> `reverse()`

  - `data_rescale()` -> `rescale()`

  - `data_to_factor()` -> `to_factor()`

  - `data_to_numeric()` -> `to_numeric()`

Note that these functions also have `.data.frame()` methods and still work for
data frames as well. Former function names are still available as aliases, but
will be deprecated and removed in a future release.

* Bumps the needed minimum R version to `3.5`.

* Removed deprecated function `data_findcols()`. Please use its replacement,
  `data_find()`.

* Removed alias `extract()` for `data_extract()` function since it collided with
  `tidyr::extract()`.

* Argument `training_proportion` in `data_partition()` is deprecated. Please use
  `proportion` now.

* Given his continued and significant contributions to the package, Etienne
  Bacher (@etiennebacher) is now included as an author.

* `unstandardise()` now works for `center(x)`

* `unnormalize()` now works for `change_scale(x)`

* `reshape_wider()` now follows more consistently `tidyr::pivot_wider()` syntax.
  Arguments `colnames_from`, `sep`, and `rows_from` are deprecated and should be
  replaced by `names_from`, `names_sep`, and `id_cols` respectively.
  `reshape_wider()` also gains an argument `names_glue` (#182, #198).

* Similarly, `reshape_longer()` now follows more consistently
  `tidyr::pivot_longer()` syntax. Argument `colnames_to` is deprecated and
  should be replaced by `names_to`. `reshape_longer()` also gains new arguments:
  `names_prefix`, `names_sep`, `names_pattern`, and `values_drop_na` (#189).

CHANGES

* Some of the text formatting helpers (like `text_concatenate()`) gain an
  `enclose` argument, to wrap text elements with surrounding characters.

* `winsorize` now accepts "raw" and "zscore" methods (in addition to
  "percentile"). Additionally, when `robust` is set to `TRUE` together with
  `method = "zscore"`, winsorizes via the median and median absolute deviation
  (MAD); else via the mean and standard deviation. (@rempsyc, #177, #49, #47).

* `convert_na_to` now accepts numeric replacements on character vectors and
  single replacement for multiple vector classes. (@rempsyc, #214).

* `data_partition()` now allows to create multiple partitions from the data,
  returning multiple training and a remaining test set.

* Functions like `center()`, `normalize()` or `standardize()` no longer fail
  when data contains infinite values (`Inf`).

NEW FUNCTIONS

* `row_to_colnames()` and `colnames_to_row()` to move a row to column names, and
  column names to row (@etiennebacher, #169).

* `data_arrange()` to sort the rows of a dataframe according to the values of
  the selected columns.

BUG FIXES

* Fixed wrong column names in `data_to_wide()` (#173).

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

* `data_group()` and `data_ungroup()` to create grouped data frames, or to
  remove the grouping information from grouped data frames.

CHANGES

* `data_find()` was added as alias to `find_colums()`, to have consistent name
  patterns for the **datawizard** functions. `data_findcols()` will be removed
  in a future update and usage is discouraged.

* The `select` argument (and thus, also the `exclude` argument) now also accepts
  functions testing for logical conditions, e.g. `is.numeric()` (or
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

* `convert_to_na()` for factors (and data frames) gains a `drop_levels`
  argument, to drop unused levels that have been replaced by `NA`.

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

* `data_findcols()` will become deprecated in future updates. Please use the new
  replacements `find_columns()` and `get_columns()`.

* The vignette *Analysing Longitudinal or Panel Data* has now moved to
  [parameters
  package](https://easystats.github.io/parameters/articles/demean.html).

NEW FUNCTIONS

* To convert rownames to a column, and *vice versa*: `rownames_as_column()` and
  `column_as_rownames()` (@etiennebacher, #80).

* `find_columns()` and `get_columns()` to find column names or retrieve subsets
  of data frames, based on various select-methods (including select-helpers).
  These function will supersede `data_findcols()` in the future.

* `data_filter()` as complement for `data_match()`, which works with logical
  expressions for filtering rows of data frames.

* For computing weighted centrality measures and dispersion: `weighted_mean()`,
  `weighted_median()`, `weighted_sd()` and `weighted_mad()`.

* To replace `NA` in vectors and dataframes: `convert_na_to()` (@etiennebacher,
  #111).

MINOR CHANGES

* The `select` argument in several functions (like `data_remove()`,
  `reshape_longer()`, or `data_extract()`) now allows the use of select-helpers
  for selecting variables based on specific patterns.

* `data_extract()` gains new arguments to allow type-safe return values,

i.e. *always* return a vector *or* a data frame. Thus, `data_extract()` can now
be used to select multiple variables or pull a single variable from data
frames.

* `data_match()` gains a `match` argument, to indicate with which logical
  operation matching results should be combined.

* Improved support for *labelled data* for many functions, i.e. returned data
  frame will preserve value and variable label attributes, where possible and
  applicable.

* `describe_distribution()` now works with lists (@etiennebacher, #105).

* `data_rename()` doesn't use `pattern` anymore to rename the columns if
  `replacement` is not provided (@etiennebacher, #103).

* `data_rename()` now adds a suffix to duplicated names in `replacement`
  (@etiennebacher, #103).

BUG FIXES

* `data_to_numeric()` produced wrong results for factors when `dummy_factors =
  TRUE` and factor contained missing values.

* `data_match()` produced wrong results when data contained missing values.

* Fixed CRAN check issues in `data_extract()` when more than one variable was
  extracted from a data frame.

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

