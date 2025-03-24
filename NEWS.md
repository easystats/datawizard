# datawizard 1.0.2

BUG FIXES

* Fixed failing R CMD check on ATLAS, noLD, and OpenBLAS due to small numerical
  differences (#592).

# datawizard 1.0.1

BUG FIXES

* Fixed issue in `data_arrange()` for data frames that only had one column.
  Formerly, the data frame was coerced into a vector, now the data frame class
  is preserved.

* Fixed issue in R-devel (4.5.0) due to a change in how `grep()` handles logical
  arguments with missing values (#588).

# datawizard 1.0.0

BREAKING CHANGES AND DEPRECATIONS

* *datawizard* now requires R >= 4.0 (#515).

* Argument `drop_na` in `data_match()` is deprecated now. Please use
  `remove_na` instead (#556).

* In `data_rename()` (#567):
  - argument `pattern` is deprecated. Use `select` instead.
  - argument `safe` is deprecated. The function now errors when `select`
    contains unknown column names.
  - when `replacement` is `NULL`, an error is now thrown (previously, column
    indices were used as new names).
  - if `select` (previously `pattern`) is a named vector, then all elements
    must be named, e.g. `c(length = "Sepal.Length", "Sepal.Width")` errors.

* Order of arguments `by` and `probability_weights` in `rescale_weights()` has
  changed, because for `method = "kish"`, the `by` argument is optional (#575).

* The name of the rescaled weights variables in `rescale_weights()` have been
  renamed. `pweights_a` and `pweights_b` are now named `rescaled_weights_a`
  and `rescaled_weights_b` (#575).

* `print()` methods for `data_tabulate()` with multiple sub-tables (i.e. when
  length of `by` was > 1) were revised. Now, an integrated table instead of
  multiple tables is returned. Furthermore, `print_html()` did not work, which
  was also fixed now (#577).

* `demean()` (and `degroup()`) gets an `append` argument that defaults to `TRUE`,
  to append the centered variables to the original data frame, instead of
  returning the de- and group-meaned variables only. Use `append = FALSE` to
  for the previous default behaviour (i.e. only returning the newly created
  variables) (#579).

CHANGES

* `rescale_weights()` gets a `method` argument, to choose method to rescale
  weights. Options are `"carle"` (the default) and `"kish"` (#575).

* The `select` argument, which is available in different functions to select
  variables, can now also be a character vector with quoted variable names,
  including a colon to indicate a range of several variables (e.g. `"cyl:gear"`)
  (#551).

* New function `row_sums()`, to calculate row sums (optionally with minimum
  amount of valid values), as complement to `row_means()` (#552).

* New function `row_count()`, to count specific values row-wise (#553).

* `data_read()` no longer shows warning about forthcoming breaking changes
  in upstream packages when reading `.RData` files (#557).

* `data_modify()` now recognizes `n()`, for example to create an index for data
  groups with `1:n()` (#535).

* The `replacement` argument in `data_rename()` now supports glue-styled
  tokens  (#563).

* `data_summary()` also accepts the results of `bayestestR::ci()` as summary
  function (#483).

* `ranktransform()` has a new argument `zeros` to determine how zeros should be
  handled when `sign = TRUE` (#573).

BUG FIXES

* `describe_distribution()` no longer errors if the sample was too sparse to compute
  CIs. Instead, it warns the user and returns `NA` (#550).

* `data_read()` preserves variable types when importing files from `rds` or
  `rdata` format (#558).

# datawizard 0.13.0

BREAKING CHANGES

* `data_rename()` now errors when the `replacement` argument contains `NA` values
  or empty strings (#539).

* Removed deprecated functions `get_columns()`, `data_find()`, `format_text()` (#546).

* Removed deprecated arguments `group` and `na.rm` in multiple functions. Use `by` and `remove_na` instead (#546).

* The default value for the argument `dummy_factors` in `to_numeric()` has
  changed from `TRUE` to `FALSE` (#544).

CHANGES

* The `pattern` argument in `data_rename()` can also be a named vector. In this
  case, names are used as values for the `replacement` argument (i.e. `pattern`
  can be a character vector using `<new name> = "<old name>"`).

* `categorize()` gains a new `breaks` argument, to decide whether breaks are
  inclusive or exclusive (#548).

* The `labels` argument in `categorize()` gets two new options, `"range"` and
  `"observed"`, to use the range of categorized values as labels (i.e. factor
  levels) (#548).

* Minor additions to `reshape_ci()` to work with forthcoming changes in the
  `{bayestestR}` package.

# datawizard 0.12.3

CHANGES

* `demean()` (and `degroup()`) now also work for nested designs, if argument
  `nested = TRUE` and  `by` specifies more than one variable (#533).

* Vignettes are no longer provided in the package, they are now only available
  on the website. There is only one "Overview" vignette available in the package,
  it contains links to the other vignettes on the website. This is because there
  are CRAN errors occurring when building vignettes on macOS and we couldn't
  determine the cause after multiple patch releases (#534).

# datawizard 0.12.2

* Remove `htmltools` from `Suggests` in an attempt of fixing an error in CRAN
  checks due to failures to build a vignette (#528).

# datawizard 0.12.1

This is a patch release to fix one error on CRAN checks occurring because of a
missing package namespace in one of the vignettes.

# datawizard 0.12.0

BREAKING CHANGES

* The argument `include_na` in `data_tabulate()` and `data_summary()` has been
  renamed into `remove_na`. Consequently, to mimic former behaviour, `FALSE` and
  `TRUE` need to be switched (i.e. `remove_na = TRUE` is equivalent to the former
  `include_na = FALSE`).

* Class names for objects returned by `data_tabulate()` have been changed to
  `datawizard_table` and `datawizard_crosstable` (resp. the plural forms,
  `*_tables`), to provide a clearer and more consistent naming scheme.

CHANGES

* `data_select()` can directly rename selected variables when a named vector
  is provided in `select`, e.g. `data_select(mtcars, c(new1 = "mpg", new2 = "cyl"))`.

* `data_tabulate()` gains an `as.data.frame()` method, to return the frequency
  table as a data frame. The structure of the returned object is a nested data
  frame, where the first column contains name of the variable for which
  frequencies were calculated, and the second column contains the frequency table.

* `demean()` (and `degroup()`) now also work for cross-classified designs, or
  more generally, for data with multiple grouping or cluster variables (i.e.
  `by` can now specify more than one variable).

# datawizard 0.11.0

BREAKING CHANGES

* Arguments named `group` or `group_by` are deprecated and will be removed
  in a future release. Please use `by` instead. This affects the following
  functions in *datawizard* (#502).

  * `data_partition()`
  * `demean()` and `degroup()`
  * `means_by_group()`
  * `rescale_weights()`

* Following aliases are deprecated and will be removed in a future release (#504):

  * `get_columns()`, use `data_select()` instead.
  * `data_find()` and `find_columns()`, use `extract_column_names()` instead.
  * `format_text()`, use `text_format()` instead.

CHANGES

* `recode_into()` is more relaxed regarding checking the type of `NA` values.
  If you recode into a numeric variable, and one of the recode values is `NA`,
  you no longer need to use `NA_real_` for numeric `NA` values.

* Improved documentation for some functions.

BUG FIXES

* `data_to_long()` did not work for data frame where columns had attributes
  (like labelled data).

# datawizard 0.10.0

BREAKING CHANGES

* The following arguments were deprecated in 0.5.0 and are now removed:

  * in `data_to_wide()`: `colnames_from`, `rows_from`, `sep`
  * in `data_to_long()`: `colnames_to`
  * in `data_partition()`: `training_proportion`

NEW FUNCTIONS

* `data_summary()`, to compute summary statistics of (grouped) data frames.

* `data_replicate()`, to expand a data frame by replicating rows based on another
  variable that contains the counts of replications per row.

CHANGES

* `data_modify()` gets three new arguments, `.at`, `.if` and `.modify`, to modify
  variables at specific positions or based on logical conditions.

* `data_tabulate()` was revised and gets several new arguments: a `weights`
  argument, to compute weighted frequency tables. `include_na` allows to include
  or omit missing values from the table. Furthermore, a `by` argument was added,
  to compute crosstables (#479, #481).

# datawizard 0.9.1

CHANGES

* `rescale()` gains `multiply` and `add` arguments, to expand ranges by a given
  factor or value.

* `to_factor()` and `to_numeric()` now support class `haven_labelled`.

BUG FIXES

* `to_numeric()` now correctly deals with inversed factor levels when
  `preserve_levels = TRUE`.

* `to_numeric()` inversed order of value labels when `dummy_factors = FALSE`.

* `convert_to_na()` now preserves attributes for factors when `drop_levels = TRUE`.

# datawizard 0.9.0

NEW FUNCTIONS

* `row_means()`, to compute row means, optionally only for the rows with at
  least `min_valid` non-missing values.

* `contr.deviation()` for sum-deviation contrast coding of factors.

* `means_by_group()`, to compute mean values of variables, grouped by levels
  of specified factors.

* `data_seek()`, to seek for variables in a data frame, based on their
  column names, variables labels, value labels or factor levels. Searching for
  labels only works for "labelled" data, i.e. when variables have a `label` or
  `labels` attribute.

CHANGES

* `recode_into()` gains an `overwrite` argument to skip overwriting already
  recoded cases when multiple recode patterns apply to the same case.

* `recode_into()` gains an `preserve_na` argument to preserve `NA` values
  when recoding.

* `data_read()` now passes the `encoding` argument to `data.table::fread()`.
  This allows to read files with non-ASCII characters.

* `datawizard` moves from the GPL-3 license to the MIT license.

* `unnormalize()` and `unstandardize()` now work with grouped data (#415).

* `unnormalize()` now errors instead of emitting a warning if it doesn't have the
  necessary info (#415).

BUG FIXES

* Fixed issue in `labels_to_levels()` when values of labels were not in sorted
  order and values were not sequentially numbered.

* Fixed issues in `data_write()` when writing labelled data into SPSS format
  and vectors were of different type as value labels.

* Fixed issues in `data_write()` when writing labelled data into SPSS format
  for character vectors with missing value labels, but existing variable
  labels.

* Fixed issue in `recode_into()` with probably wrong case number printed in the
  warning when several recode patterns match to one case.

* Fixed issue in `recode_into()` when original data contained `NA` values and
  `NA` was not included in the recode pattern.

* Fixed issue in `data_filter()` where functions containing a `=` (e.g. when
  naming arguments, like `grepl(pattern, x = a)`) were mistakenly seen as
  faulty syntax.

* Fixed issue in `empty_column()` for strings with invalid multibyte strings.
  For such data frames or files, `empty_column()` or `data_read()` no longer
  fails.

# datawizard 0.8.0

BREAKING CHANGES

* The following re-exported functions from `{insight}` have now been removed:
  `object_has_names()`, `object_has_rownames()`, `is_empty_object()`,
  `compact_list()`, `compact_character()`.

* Argument `na.rm` was renamed to `remove_na` throughout `{datawizard}` functions.
  `na.rm` is kept for backward compatibility, but will be deprecated and later
  removed in future updates.

* The way expressions are defined in `data_filter()` was revised. The `filter`
  argument was replaced by `...`, allowing to separate multiple expression with
  a comma (which are then combined with `&`). Furthermore, expressions can now also be
  defined as strings, or be provided as character vectors, to allow string-friendly
  programming.

CHANGES

* Weighted-functions (`weighted_sd()`, `weighted_mean()`, ...) gain a `remove_na`
  argument, to remove or keep missing and infinite values. By default,
  `remove_na = TRUE`, i.e. missing and infinite values are removed by default.

* `reverse_scale()`, `normalize()` and `rescale()` gain an `append` argument
  (similar to other data frame methods of transformation functions), to append
  recoded variables to the input data frame instead of overwriting existing
  variables.

NEW FUNCTIONS

* `rowid_as_column()` to complement `rownames_as_column()` (and to mimic
  `tibble::rowid_to_column()`). Note that its behavior is different from
  `tibble::rowid_to_column()` for grouped data. See the Details section in the
  docs.

* `data_unite()`, to merge values of multiple variables into one new variable.

* `data_separate()`, as counterpart to `data_unite()`, to separate a single
  variable into multiple new variables.

* `data_modify()`, to create new variables, or modify or remove existing
  variables in a data frame.

MINOR CHANGES

* `to_numeric()` for variables of type `Date`, `POSIXct` and `POSIXlt` now
  includes the class name in the warning message.

* Added a `print()` method for `center()`, `standardize()`, `normalize()` and
  `rescale()`.

BUG FIXES

* `standardize_parameters()` now works when the package namespace is in the model
  formula (#401).

* `data_merge()` no longer yields a warning for `tibbles` when `join = "bind"`.

* `center()` and `standardize()` did not work for grouped data frames (of class
  `grouped_df`) when `force = TRUE`.

* The `data.frame` method of `describe_distribution()` returns `NULL` instead of
  an error if no valid variable were passed (for example a factor variable with
  `include_factors = FALSE`) (#421).

# datawizard 0.7.1

BREAKING CHANGES

* `add_labs()` was renamed into `assign_labels()`. Since `add_labs()` existed
  only for a few days, there will be no alias for backwards compatibility.

NEW FUNCTIONS

* `labels_to_levels()`, to use value labels of factors as their levels.

MINOR CHANGES

* `data_read()` now checks if the imported object actually is a data frame (or
  coercible to a data frame), and if not, no longer errors, but gives an
  informative warning of the type of object that was imported.

BUG FIXES

* Fix test for CRAN check on Mac OS arm64

# datawizard 0.7.0

BREAKING CHANGES

* In selection patterns, expressions like `-var1:var3` to exclude all variables
  between `var1` and `var3` are no longer accepted. The correct expression is
  `-(var1:var3)`. This is for 2 reasons:

  * to be consistent with the behavior for numerics (`-1:2` is not accepted but
    `-(1:2)` is);
  * to be consistent with `dplyr::select()`, which throws a warning and only
    uses the first variable in the first expression.

NEW FUNCTIONS

* `recode_into()`, similar to `dplyr::case_when()`, to recode values from one
  or more variables into a new variable.

* `mean_sd()` and `median_mad()` for summarizing vectors to their mean (or
  median) and a range of one SD (or MAD) above and below.

* `data_write()` as counterpart to `data_read()`, to write data frames into
  CSV, SPSS, SAS, Stata files and many other file types. One advantage over
  existing functions to write data in other packages is that labelled (numeric)
  data can be converted into factors (with values labels used as factor levels)
  even for text formats like CSV and similar. This allows exporting "labelled"
  data into those file formats, too.

* `add_labs()`, to manually add value and variable labels as attributes to
  variables. These attributes are stored as `"label"` and `"labels"` attributes,
  similar to the `labelled` class from the _haven_ package.

MINOR CHANGES

* `data_rename()` gets a `verbose` argument.
* `winsorize()` now errors if the threshold is incorrect (previously, it provided
  a warning and returned the unchanged data). The argument `verbose` is now
  useless but is kept for backward compatibility. The documentation now contains
  details about the valid values for `threshold` (#357).
* In all functions that have arguments `select` and/or `exclude`, there is now
  one warning per misspelled variable. The previous behavior was to have only one
  warning.
* Fixed inconsistent behaviour in `standardize()` when only one of the arguments
  `center` or `scale` were provided (#365).
* `unstandardize()` and `replace_nan_inf()` now work with select helpers (#376).
* Added informative warning and error messages to `reverse()`. Furthermore, the
  docs now describe the `range` argument more clearly (#380).
* `unnormalize()` errors with unexpected inputs (#383).

BUG FIXES

* `empty_columns()` (and therefore `remove_empty_columns()`) now correctly detects
  columns containing only `NA_character_` (#349).
* Select helpers now work in custom functions when argument is called `select`
  (#356).
* Fix unexpected warning in `convert_na_to()` when `select` is a list (#352).
* Fixed issue with correct labelling of numeric variables with more than nine
  unique values and associated value labels.


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
