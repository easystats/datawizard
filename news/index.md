# Changelog

## datawizard 1.3.0

CRAN release: 2025-10-11

BREAKING CHANGES

- Argument `values_fill` in
  [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  is now defunct, because it did not work as intended
  ([\#645](https://github.com/easystats/datawizard/issues/645)).

- [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  no longer removes empty columns that were created after widening data
  frames, to behave similarly to
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  ([\#645](https://github.com/easystats/datawizard/issues/645)).

CHANGES

- [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  now saves the table of proportions for crosstables as attribute,
  accessible via the new
  [`as.prop.table()`](https://easystats.github.io/datawizard/reference/as.prop.table.md)
  method ([\#656](https://github.com/easystats/datawizard/issues/656)).

- Due to changes in the package `insight`,
  [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  no longer prints decimals when all values in a column are integers
  ([\#641](https://github.com/easystats/datawizard/issues/641)).

- Argument `values_from` in
  [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  now supports select-helpers like the `select` argument in other
  [datawizard](https://easystats.github.io/datawizard/) functions
  ([\#645](https://github.com/easystats/datawizard/issues/645)).

- Added a
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  method for
  [`data_codebook()`](https://easystats.github.io/datawizard/reference/data_codebook.md)
  ([\#646](https://github.com/easystats/datawizard/issues/646)).

- [`display()`](https://easystats.github.io/insight/reference/display.html)
  methods now support the
  [tinytable](https://vincentarelbundock.github.io/tinytable/) package.
  Use `format = "tt"` to export tables as `tinytable` objects
  ([\#646](https://github.com/easystats/datawizard/issues/646)).

- Improved performance for several functions that process grouped data
  frames when the input is a grouped `tibble`
  ([\#651](https://github.com/easystats/datawizard/issues/651)).

BUG FIXES

- Fixed an issue when
  [`demean()`](https://easystats.github.io/datawizard/reference/demean.md)ing
  nested structures with more than 2 grouping variables
  ([\#635](https://github.com/easystats/datawizard/issues/635)).

- Fixed an issue when
  [`demean()`](https://easystats.github.io/datawizard/reference/demean.md)ing
  crossed structures with more than 2 grouping variables
  ([\#638](https://github.com/easystats/datawizard/issues/638)).

- Fixed issue in
  [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  with multiple variables assigned in `values_from` when IDs were not
  balanced (equally spread across observations)
  ([\#644](https://github.com/easystats/datawizard/issues/644)).

- Fixed issue in
  [`data_replicate()`](https://easystats.github.io/datawizard/reference/data_replicate.md)
  when data frame had only one column to replicate
  ([\#654](https://github.com/easystats/datawizard/issues/654)).

## datawizard 1.2.0

CRAN release: 2025-07-17

BREAKING CHANGES

- The following deprecated arguments have been removed
  ([\#603](https://github.com/easystats/datawizard/issues/603)):
  - `drop_na` in
    [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md)
  - `safe`, `pattern`, and `verbose` in
    [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)

CHANGES

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  and
  [`data_write()`](https://easystats.github.io/datawizard/reference/data_read.md)
  now support the `.parquet` file format, via the *nanoparquet* package
  ([\#625](https://github.com/easystats/datawizard/issues/625)).

- [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  gets a
  [`display()`](https://easystats.github.io/insight/reference/display.html)
  method ([\#627](https://github.com/easystats/datawizard/issues/627)).

- [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  gets an [`as.table()`](https://rdrr.io/r/base/table.html) method to
  coerce the frequency or contingency table into a (list of)
  [`table()`](https://rdrr.io/r/base/table.html) object(s). This can be
  useful for further statistical analysis, e.g. in combination with
  [`chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)
  ([\#629](https://github.com/easystats/datawizard/issues/629)).

- The [`print()`](https://rdrr.io/r/base/print.html) method for
  [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  now appears in the documentation, making the `big_mark` argument
  visible ([\#627](https://github.com/easystats/datawizard/issues/627)).

BUG FIXES

- Fixed an issue when printing cross tables using
  `data_tabulate(by = ...)`, which was caused by the recent changes in
  [`insight::export_table()`](https://easystats.github.io/insight/reference/export_table.html).

- Fixed another issue when printing cross tables using
  `data_tabulate(by = ...)`, when more than one variable was selected
  for `select`
  ([\#630](https://github.com/easystats/datawizard/issues/630)).

- Fixed typo in the documentation of
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md).

## datawizard 1.1.0

CRAN release: 2025-05-09

BREAKING CHANGES

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  now also returns Bayesian models from packages *brms* and *rstanarm*
  as original model objects, and no longer coerces them into data frames
  ([\#606](https://github.com/easystats/datawizard/issues/606)).

- The output format of
  [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  on grouped data has changed. Before, it printed one table per group
  combination. Now, it prints a single table with group columns at the
  start ([\#610](https://github.com/easystats/datawizard/issues/610)).

- The output format of
  [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  when confidence intervals are requested has changed. Now, for each
  centrality measure a confidence interval is calculated
  ([\#617](https://github.com/easystats/datawizard/issues/617)).

- [`data_modify()`](https://easystats.github.io/datawizard/reference/data_modify.md)
  now always uses values of a vector for a modified or newly created
  variable, and no longer tries to detect whether a character value
  possibly contains an expression. To allow expression provided as
  string (or character vectors), use the helper-function `as_expr()`.
  Only literal expressions or strings wrapped in `as_expr()` will be
  evaluated as expressions, everything else will be treated as vector
  with values for new variables
  ([\#605](https://github.com/easystats/datawizard/issues/605)).

CHANGES

- [`display()`](https://easystats.github.io/insight/reference/display.html)
  is now re-exported from package *insight*.

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  and
  [`data_write()`](https://easystats.github.io/datawizard/reference/data_read.md)
  now rely on base-R functions for files of type `.rds`, `.rda` or
  `.rdata`. Thus, package *rio* is no longer required to be installed
  for these file types
  ([\#607](https://github.com/easystats/datawizard/issues/607)).

- [`data_codebook()`](https://easystats.github.io/datawizard/reference/data_codebook.md)
  gives an informative warning when no column names matched the
  selection pattern
  ([\#601](https://github.com/easystats/datawizard/issues/601)).

- [`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md)
  now errors when columns selected to reshape do not exist in the data,
  to avoid nonsensical results that could be missed
  ([\#602](https://github.com/easystats/datawizard/issues/602)).

- New argument `by` in
  [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  ([\#604](https://github.com/easystats/datawizard/issues/604)).

- [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  now gives informative errors when column names in the input data frame
  conflict with column from the output table
  ([\#612](https://github.com/easystats/datawizard/issues/612)).

- The methods for `parameters_distribution` objects are now defined in
  `datawizard` (they were previously in `parameters`)
  ([\#613](https://github.com/easystats/datawizard/issues/613)).

BUG FIXES

- Fixed bug in
  [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md),
  where new column names in `names_from` were ignored when that column
  only contained one unique value.

- Fixed bug in
  [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  when some group combinations didn’t appear in the data
  ([\#609](https://github.com/easystats/datawizard/issues/609)).

- Fixed bug in
  [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  when more than one value for the `centrality` argument were specified
  ([\#617](https://github.com/easystats/datawizard/issues/617)).

- Fixed bug in
  [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  where setting `verbose = FALSE` didn’t hide some warnings
  ([\#617](https://github.com/easystats/datawizard/issues/617)).

- Fixed warning in
  [`data_summary()`](https://easystats.github.io/datawizard/reference/data_summary.md)
  when a variable had the same name as another object in the global
  environment
  ([\#585](https://github.com/easystats/datawizard/issues/585)).

## datawizard 1.0.2

CRAN release: 2025-03-24

BUG FIXES

- Fixed failing R CMD check on ATLAS, noLD, and OpenBLAS due to small
  numerical differences
  ([\#592](https://github.com/easystats/datawizard/issues/592)).

## datawizard 1.0.1

CRAN release: 2025-03-07

BUG FIXES

- Fixed issue in
  [`data_arrange()`](https://easystats.github.io/datawizard/reference/data_arrange.md)
  for data frames that only had one column. Formerly, the data frame was
  coerced into a vector, now the data frame class is preserved.

- Fixed issue in R-devel (4.5.0) due to a change in how
  [`grep()`](https://rdrr.io/r/base/grep.html) handles logical arguments
  with missing values
  ([\#588](https://github.com/easystats/datawizard/issues/588)).

## datawizard 1.0.0

CRAN release: 2025-01-10

BREAKING CHANGES AND DEPRECATIONS

- *datawizard* now requires R \>= 4.0
  ([\#515](https://github.com/easystats/datawizard/issues/515)).

- Argument `drop_na` in
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md)
  is deprecated now. Please use `remove_na` instead
  ([\#556](https://github.com/easystats/datawizard/issues/556)).

- In
  [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
  ([\#567](https://github.com/easystats/datawizard/issues/567)):

  - argument `pattern` is deprecated. Use `select` instead.
  - argument `safe` is deprecated. The function now errors when `select`
    contains unknown column names.
  - when `replacement` is `NULL`, an error is now thrown (previously,
    column indices were used as new names).
  - if `select` (previously `pattern`) is a named vector, then all
    elements must be named,
    e.g. `c(length = "Sepal.Length", "Sepal.Width")` errors.

- Order of arguments `by` and `probability_weights` in
  [`rescale_weights()`](https://easystats.github.io/datawizard/reference/rescale_weights.md)
  has changed, because for `method = "kish"`, the `by` argument is
  optional
  ([\#575](https://github.com/easystats/datawizard/issues/575)).

- The name of the rescaled weights variables in
  [`rescale_weights()`](https://easystats.github.io/datawizard/reference/rescale_weights.md)
  have been renamed. `pweights_a` and `pweights_b` are now named
  `rescaled_weights_a` and `rescaled_weights_b`
  ([\#575](https://github.com/easystats/datawizard/issues/575)).

- [`print()`](https://rdrr.io/r/base/print.html) methods for
  [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  with multiple sub-tables (i.e. when length of `by` was \> 1) were
  revised. Now, an integrated table instead of multiple tables is
  returned. Furthermore,
  [`print_html()`](https://easystats.github.io/insight/reference/display.html)
  did not work, which was also fixed now
  ([\#577](https://github.com/easystats/datawizard/issues/577)).

- [`demean()`](https://easystats.github.io/datawizard/reference/demean.md)
  (and
  [`degroup()`](https://easystats.github.io/datawizard/reference/demean.md))
  gets an `append` argument that defaults to `TRUE`, to append the
  centered variables to the original data frame, instead of returning
  the de- and group-meaned variables only. Use `append = FALSE` to for
  the previous default behaviour (i.e. only returning the newly created
  variables)
  ([\#579](https://github.com/easystats/datawizard/issues/579)).

CHANGES

- [`rescale_weights()`](https://easystats.github.io/datawizard/reference/rescale_weights.md)
  gets a `method` argument, to choose method to rescale weights. Options
  are `"carle"` (the default) and `"kish"`
  ([\#575](https://github.com/easystats/datawizard/issues/575)).

- The `select` argument, which is available in different functions to
  select variables, can now also be a character vector with quoted
  variable names, including a colon to indicate a range of several
  variables (e.g. `"cyl:gear"`)
  ([\#551](https://github.com/easystats/datawizard/issues/551)).

- New function
  [`row_sums()`](https://easystats.github.io/datawizard/reference/row_means.md),
  to calculate row sums (optionally with minimum amount of valid
  values), as complement to
  [`row_means()`](https://easystats.github.io/datawizard/reference/row_means.md)
  ([\#552](https://github.com/easystats/datawizard/issues/552)).

- New function
  [`row_count()`](https://easystats.github.io/datawizard/reference/row_count.md),
  to count specific values row-wise
  ([\#553](https://github.com/easystats/datawizard/issues/553)).

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  no longer shows warning about forthcoming breaking changes in upstream
  packages when reading `.RData` files
  ([\#557](https://github.com/easystats/datawizard/issues/557)).

- [`data_modify()`](https://easystats.github.io/datawizard/reference/data_modify.md)
  now recognizes
  [`n()`](https://dplyr.tidyverse.org/reference/context.html), for
  example to create an index for data groups with `1:n()`
  ([\#535](https://github.com/easystats/datawizard/issues/535)).

- The `replacement` argument in
  [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
  now supports glue-styled tokens
  ([\#563](https://github.com/easystats/datawizard/issues/563)).

- [`data_summary()`](https://easystats.github.io/datawizard/reference/data_summary.md)
  also accepts the results of
  [`bayestestR::ci()`](https://easystats.github.io/bayestestR/reference/ci.html)
  as summary function
  ([\#483](https://github.com/easystats/datawizard/issues/483)).

- [`ranktransform()`](https://easystats.github.io/datawizard/reference/ranktransform.md)
  has a new argument `zeros` to determine how zeros should be handled
  when `sign = TRUE`
  ([\#573](https://github.com/easystats/datawizard/issues/573)).

BUG FIXES

- [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  no longer errors if the sample was too sparse to compute CIs. Instead,
  it warns the user and returns `NA`
  ([\#550](https://github.com/easystats/datawizard/issues/550)).

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  preserves variable types when importing files from `rds` or `rdata`
  format ([\#558](https://github.com/easystats/datawizard/issues/558)).

## datawizard 0.13.0

CRAN release: 2024-10-05

BREAKING CHANGES

- [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
  now errors when the `replacement` argument contains `NA` values or
  empty strings
  ([\#539](https://github.com/easystats/datawizard/issues/539)).

- Removed deprecated functions `get_columns()`, `data_find()`,
  `format_text()`
  ([\#546](https://github.com/easystats/datawizard/issues/546)).

- Removed deprecated arguments `group` and `na.rm` in multiple
  functions. Use `by` and `remove_na` instead
  ([\#546](https://github.com/easystats/datawizard/issues/546)).

- The default value for the argument `dummy_factors` in
  [`to_numeric()`](https://easystats.github.io/datawizard/reference/to_numeric.md)
  has changed from `TRUE` to `FALSE`
  ([\#544](https://github.com/easystats/datawizard/issues/544)).

CHANGES

- The `pattern` argument in
  [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
  can also be a named vector. In this case, names are used as values for
  the `replacement` argument (i.e. `pattern` can be a character vector
  using `<new name> = "<old name>"`).

- [`categorize()`](https://easystats.github.io/datawizard/reference/categorize.md)
  gains a new `breaks` argument, to decide whether breaks are inclusive
  or exclusive
  ([\#548](https://github.com/easystats/datawizard/issues/548)).

- The `labels` argument in
  [`categorize()`](https://easystats.github.io/datawizard/reference/categorize.md)
  gets two new options, `"range"` and `"observed"`, to use the range of
  categorized values as labels (i.e. factor levels)
  ([\#548](https://github.com/easystats/datawizard/issues/548)).

- Minor additions to
  [`reshape_ci()`](https://easystats.github.io/datawizard/reference/reshape_ci.md)
  to work with forthcoming changes in the
  [bayestestR](https://easystats.github.io/bayestestR/) package.

## datawizard 0.12.3

CRAN release: 2024-09-02

CHANGES

- [`demean()`](https://easystats.github.io/datawizard/reference/demean.md)
  (and
  [`degroup()`](https://easystats.github.io/datawizard/reference/demean.md))
  now also work for nested designs, if argument `nested = TRUE` and `by`
  specifies more than one variable
  ([\#533](https://github.com/easystats/datawizard/issues/533)).

- Vignettes are no longer provided in the package, they are now only
  available on the website. There is only one “Overview” vignette
  available in the package, it contains links to the other vignettes on
  the website. This is because there are CRAN errors occurring when
  building vignettes on macOS and we couldn’t determine the cause after
  multiple patch releases
  ([\#534](https://github.com/easystats/datawizard/issues/534)).

## datawizard 0.12.2

CRAN release: 2024-07-21

- Remove `htmltools` from `Suggests` in an attempt of fixing an error in
  CRAN checks due to failures to build a vignette
  ([\#528](https://github.com/easystats/datawizard/issues/528)).

## datawizard 0.12.1

CRAN release: 2024-07-15

This is a patch release to fix one error on CRAN checks occurring
because of a missing package namespace in one of the vignettes.

## datawizard 0.12.0

CRAN release: 2024-07-11

BREAKING CHANGES

- The argument `include_na` in
  [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  and
  [`data_summary()`](https://easystats.github.io/datawizard/reference/data_summary.md)
  has been renamed into `remove_na`. Consequently, to mimic former
  behaviour, `FALSE` and `TRUE` need to be switched
  (i.e. `remove_na = TRUE` is equivalent to the former
  `include_na = FALSE`).

- Class names for objects returned by
  [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  have been changed to `datawizard_table` and `datawizard_crosstable`
  (resp. the plural forms, `*_tables`), to provide a clearer and more
  consistent naming scheme.

CHANGES

- [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
  can directly rename selected variables when a named vector is provided
  in `select`,
  e.g. `data_select(mtcars, c(new1 = "mpg", new2 = "cyl"))`.

- [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  gains an
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method,
  to return the frequency table as a data frame. The structure of the
  returned object is a nested data frame, where the first column
  contains name of the variable for which frequencies were calculated,
  and the second column contains the frequency table.

- [`demean()`](https://easystats.github.io/datawizard/reference/demean.md)
  (and
  [`degroup()`](https://easystats.github.io/datawizard/reference/demean.md))
  now also work for cross-classified designs, or more generally, for
  data with multiple grouping or cluster variables (i.e. `by` can now
  specify more than one variable).

## datawizard 0.11.0

CRAN release: 2024-06-05

BREAKING CHANGES

- Arguments named `group` or `group_by` are deprecated and will be
  removed in a future release. Please use `by` instead. This affects the
  following functions in *datawizard*
  ([\#502](https://github.com/easystats/datawizard/issues/502)).

  - [`data_partition()`](https://easystats.github.io/datawizard/reference/data_partition.md)
  - [`demean()`](https://easystats.github.io/datawizard/reference/demean.md)
    and
    [`degroup()`](https://easystats.github.io/datawizard/reference/demean.md)
  - [`means_by_group()`](https://easystats.github.io/datawizard/reference/means_by_group.md)
  - [`rescale_weights()`](https://easystats.github.io/datawizard/reference/rescale_weights.md)

- Following aliases are deprecated and will be removed in a future
  release ([\#504](https://github.com/easystats/datawizard/issues/504)):

  - `get_columns()`, use
    [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
    instead.
  - `data_find()` and
    [`find_columns()`](https://easystats.github.io/datawizard/reference/extract_column_names.md),
    use
    [`extract_column_names()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
    instead.
  - `format_text()`, use
    [`text_format()`](https://easystats.github.io/datawizard/reference/text_format.md)
    instead.

CHANGES

- [`recode_into()`](https://easystats.github.io/datawizard/reference/recode_into.md)
  is more relaxed regarding checking the type of `NA` values. If you
  recode into a numeric variable, and one of the recode values is `NA`,
  you no longer need to use `NA_real_` for numeric `NA` values.

- Improved documentation for some functions.

BUG FIXES

- [`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md)
  did not work for data frame where columns had attributes (like
  labelled data).

## datawizard 0.10.0

CRAN release: 2024-03-26

BREAKING CHANGES

- The following arguments were deprecated in 0.5.0 and are now removed:

  - in
    [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md):
    `colnames_from`, `rows_from`, `sep`
  - in
    [`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md):
    `colnames_to`
  - in
    [`data_partition()`](https://easystats.github.io/datawizard/reference/data_partition.md):
    `training_proportion`

NEW FUNCTIONS

- [`data_summary()`](https://easystats.github.io/datawizard/reference/data_summary.md),
  to compute summary statistics of (grouped) data frames.

- [`data_replicate()`](https://easystats.github.io/datawizard/reference/data_replicate.md),
  to expand a data frame by replicating rows based on another variable
  that contains the counts of replications per row.

CHANGES

- [`data_modify()`](https://easystats.github.io/datawizard/reference/data_modify.md)
  gets three new arguments, `.at`, `.if` and `.modify`, to modify
  variables at specific positions or based on logical conditions.

- [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  was revised and gets several new arguments: a `weights` argument, to
  compute weighted frequency tables. `include_na` allows to include or
  omit missing values from the table. Furthermore, a `by` argument was
  added, to compute crosstables
  ([\#479](https://github.com/easystats/datawizard/issues/479),
  [\#481](https://github.com/easystats/datawizard/issues/481)).

## datawizard 0.9.1

CRAN release: 2023-12-21

CHANGES

- [`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md)
  gains `multiply` and `add` arguments, to expand ranges by a given
  factor or value.

- [`to_factor()`](https://easystats.github.io/datawizard/reference/to_factor.md)
  and
  [`to_numeric()`](https://easystats.github.io/datawizard/reference/to_numeric.md)
  now support class `haven_labelled`.

BUG FIXES

- [`to_numeric()`](https://easystats.github.io/datawizard/reference/to_numeric.md)
  now correctly deals with inversed factor levels when
  `preserve_levels = TRUE`.

- [`to_numeric()`](https://easystats.github.io/datawizard/reference/to_numeric.md)
  inversed order of value labels when `dummy_factors = FALSE`.

- [`convert_to_na()`](https://easystats.github.io/datawizard/reference/convert_to_na.md)
  now preserves attributes for factors when `drop_levels = TRUE`.

## datawizard 0.9.0

CRAN release: 2023-09-15

NEW FUNCTIONS

- [`row_means()`](https://easystats.github.io/datawizard/reference/row_means.md),
  to compute row means, optionally only for the rows with at least
  `min_valid` non-missing values.

- [`contr.deviation()`](https://easystats.github.io/datawizard/reference/contr.deviation.md)
  for sum-deviation contrast coding of factors.

- [`means_by_group()`](https://easystats.github.io/datawizard/reference/means_by_group.md),
  to compute mean values of variables, grouped by levels of specified
  factors.

- [`data_seek()`](https://easystats.github.io/datawizard/reference/data_seek.md),
  to seek for variables in a data frame, based on their column names,
  variables labels, value labels or factor levels. Searching for labels
  only works for “labelled” data, i.e. when variables have a `label` or
  `labels` attribute.

CHANGES

- [`recode_into()`](https://easystats.github.io/datawizard/reference/recode_into.md)
  gains an `overwrite` argument to skip overwriting already recoded
  cases when multiple recode patterns apply to the same case.

- [`recode_into()`](https://easystats.github.io/datawizard/reference/recode_into.md)
  gains an `preserve_na` argument to preserve `NA` values when recoding.

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  now passes the `encoding` argument to
  [`data.table::fread()`](https://rdrr.io/pkg/data.table/man/fread.html).
  This allows to read files with non-ASCII characters.

- `datawizard` moves from the GPL-3 license to the MIT license.

- [`unnormalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  and
  [`unstandardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  now work with grouped data
  ([\#415](https://github.com/easystats/datawizard/issues/415)).

- [`unnormalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  now errors instead of emitting a warning if it doesn’t have the
  necessary info
  ([\#415](https://github.com/easystats/datawizard/issues/415)).

BUG FIXES

- Fixed issue in
  [`labels_to_levels()`](https://easystats.github.io/datawizard/reference/labels_to_levels.md)
  when values of labels were not in sorted order and values were not
  sequentially numbered.

- Fixed issues in
  [`data_write()`](https://easystats.github.io/datawizard/reference/data_read.md)
  when writing labelled data into SPSS format and vectors were of
  different type as value labels.

- Fixed issues in
  [`data_write()`](https://easystats.github.io/datawizard/reference/data_read.md)
  when writing labelled data into SPSS format for character vectors with
  missing value labels, but existing variable labels.

- Fixed issue in
  [`recode_into()`](https://easystats.github.io/datawizard/reference/recode_into.md)
  with probably wrong case number printed in the warning when several
  recode patterns match to one case.

- Fixed issue in
  [`recode_into()`](https://easystats.github.io/datawizard/reference/recode_into.md)
  when original data contained `NA` values and `NA` was not included in
  the recode pattern.

- Fixed issue in
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)
  where functions containing a `=` (e.g. when naming arguments, like
  `grepl(pattern, x = a)`) were mistakenly seen as faulty syntax.

- Fixed issue in `empty_column()` for strings with invalid multibyte
  strings. For such data frames or files, `empty_column()` or
  [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  no longer fails.

## datawizard 0.8.0

CRAN release: 2023-06-16

BREAKING CHANGES

- The following re-exported functions from
  [insight](https://easystats.github.io/insight/) have now been removed:
  `object_has_names()`, `object_has_rownames()`, `is_empty_object()`,
  `compact_list()`, `compact_character()`.

- Argument `na.rm` was renamed to `remove_na` throughout
  [datawizard](https://easystats.github.io/datawizard/) functions.
  `na.rm` is kept for backward compatibility, but will be deprecated and
  later removed in future updates.

- The way expressions are defined in
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)
  was revised. The `filter` argument was replaced by `...`, allowing to
  separate multiple expression with a comma (which are then combined
  with `&`). Furthermore, expressions can now also be defined as
  strings, or be provided as character vectors, to allow string-friendly
  programming.

CHANGES

- Weighted-functions
  ([`weighted_sd()`](https://easystats.github.io/datawizard/reference/weighted_mean.md),
  [`weighted_mean()`](https://easystats.github.io/datawizard/reference/weighted_mean.md),
  …) gain a `remove_na` argument, to remove or keep missing and infinite
  values. By default, `remove_na = TRUE`, i.e. missing and infinite
  values are removed by default.

- [`reverse_scale()`](https://easystats.github.io/datawizard/reference/reverse.md),
  [`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  and
  [`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md)
  gain an `append` argument (similar to other data frame methods of
  transformation functions), to append recoded variables to the input
  data frame instead of overwriting existing variables.

NEW FUNCTIONS

- [`rowid_as_column()`](https://easystats.github.io/datawizard/reference/rownames.md)
  to complement
  [`rownames_as_column()`](https://easystats.github.io/datawizard/reference/rownames.md)
  (and to mimic
  [`tibble::rowid_to_column()`](https://tibble.tidyverse.org/reference/rownames.html)).
  Note that its behavior is different from
  [`tibble::rowid_to_column()`](https://tibble.tidyverse.org/reference/rownames.html)
  for grouped data. See the Details section in the docs.

- [`data_unite()`](https://easystats.github.io/datawizard/reference/data_unite.md),
  to merge values of multiple variables into one new variable.

- [`data_separate()`](https://easystats.github.io/datawizard/reference/data_separate.md),
  as counterpart to
  [`data_unite()`](https://easystats.github.io/datawizard/reference/data_unite.md),
  to separate a single variable into multiple new variables.

- [`data_modify()`](https://easystats.github.io/datawizard/reference/data_modify.md),
  to create new variables, or modify or remove existing variables in a
  data frame.

MINOR CHANGES

- [`to_numeric()`](https://easystats.github.io/datawizard/reference/to_numeric.md)
  for variables of type `Date`, `POSIXct` and `POSIXlt` now includes the
  class name in the warning message.

- Added a [`print()`](https://rdrr.io/r/base/print.html) method for
  [`center()`](https://easystats.github.io/datawizard/reference/center.md),
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md),
  [`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  and
  [`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md).

BUG FIXES

- [`standardize_parameters()`](https://easystats.github.io/parameters/reference/standardize_parameters.html)
  now works when the package namespace is in the model formula
  ([\#401](https://github.com/easystats/datawizard/issues/401)).

- [`data_merge()`](https://easystats.github.io/datawizard/reference/data_merge.md)
  no longer yields a warning for `tibbles` when `join = "bind"`.

- [`center()`](https://easystats.github.io/datawizard/reference/center.md)
  and
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  did not work for grouped data frames (of class `grouped_df`) when
  `force = TRUE`.

- The `data.frame` method of
  [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  returns `NULL` instead of an error if no valid variable were passed
  (for example a factor variable with `include_factors = FALSE`)
  ([\#421](https://github.com/easystats/datawizard/issues/421)).

## datawizard 0.7.1

CRAN release: 2023-04-03

BREAKING CHANGES

- `add_labs()` was renamed into
  [`assign_labels()`](https://easystats.github.io/datawizard/reference/assign_labels.md).
  Since `add_labs()` existed only for a few days, there will be no alias
  for backwards compatibility.

NEW FUNCTIONS

- [`labels_to_levels()`](https://easystats.github.io/datawizard/reference/labels_to_levels.md),
  to use value labels of factors as their levels.

MINOR CHANGES

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  now checks if the imported object actually is a data frame (or
  coercible to a data frame), and if not, no longer errors, but gives an
  informative warning of the type of object that was imported.

BUG FIXES

- Fix test for CRAN check on Mac OS arm64

## datawizard 0.7.0

CRAN release: 2023-03-22

BREAKING CHANGES

- In selection patterns, expressions like `-var1:var3` to exclude all
  variables between `var1` and `var3` are no longer accepted. The
  correct expression is `-(var1:var3)`. This is for 2 reasons:

  - to be consistent with the behavior for numerics (`-1:2` is not
    accepted but `-(1:2)` is);
  - to be consistent with
    [`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
    which throws a warning and only uses the first variable in the first
    expression.

NEW FUNCTIONS

- [`recode_into()`](https://easystats.github.io/datawizard/reference/recode_into.md),
  similar to
  [`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html),
  to recode values from one or more variables into a new variable.

- [`mean_sd()`](https://easystats.github.io/datawizard/reference/mean_sd.md)
  and
  [`median_mad()`](https://easystats.github.io/datawizard/reference/mean_sd.md)
  for summarizing vectors to their mean (or median) and a range of one
  SD (or MAD) above and below.

- [`data_write()`](https://easystats.github.io/datawizard/reference/data_read.md)
  as counterpart to
  [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md),
  to write data frames into CSV, SPSS, SAS, Stata files and many other
  file types. One advantage over existing functions to write data in
  other packages is that labelled (numeric) data can be converted into
  factors (with values labels used as factor levels) even for text
  formats like CSV and similar. This allows exporting “labelled” data
  into those file formats, too.

- `add_labs()`, to manually add value and variable labels as attributes
  to variables. These attributes are stored as `"label"` and `"labels"`
  attributes, similar to the `labelled` class from the *haven* package.

MINOR CHANGES

- [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
  gets a `verbose` argument.
- [`winsorize()`](https://easystats.github.io/datawizard/reference/winsorize.md)
  now errors if the threshold is incorrect (previously, it provided a
  warning and returned the unchanged data). The argument `verbose` is
  now useless but is kept for backward compatibility. The documentation
  now contains details about the valid values for `threshold`
  ([\#357](https://github.com/easystats/datawizard/issues/357)).
- In all functions that have arguments `select` and/or `exclude`, there
  is now one warning per misspelled variable. The previous behavior was
  to have only one warning.
- Fixed inconsistent behaviour in
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  when only one of the arguments `center` or `scale` were provided
  ([\#365](https://github.com/easystats/datawizard/issues/365)).
- [`unstandardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  and
  [`replace_nan_inf()`](https://easystats.github.io/datawizard/reference/replace_nan_inf.md)
  now work with select helpers
  ([\#376](https://github.com/easystats/datawizard/issues/376)).
- Added informative warning and error messages to
  [`reverse()`](https://easystats.github.io/datawizard/reference/reverse.md).
  Furthermore, the docs now describe the `range` argument more clearly
  ([\#380](https://github.com/easystats/datawizard/issues/380)).
- [`unnormalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  errors with unexpected inputs
  ([\#383](https://github.com/easystats/datawizard/issues/383)).

BUG FIXES

- [`empty_columns()`](https://easystats.github.io/datawizard/reference/remove_empty.md)
  (and therefore
  [`remove_empty_columns()`](https://easystats.github.io/datawizard/reference/remove_empty.md))
  now correctly detects columns containing only `NA_character_`
  ([\#349](https://github.com/easystats/datawizard/issues/349)).
- Select helpers now work in custom functions when argument is called
  `select`
  ([\#356](https://github.com/easystats/datawizard/issues/356)).
- Fix unexpected warning in
  [`convert_na_to()`](https://easystats.github.io/datawizard/reference/convert_na_to.md)
  when `select` is a list
  ([\#352](https://github.com/easystats/datawizard/issues/352)).
- Fixed issue with correct labelling of numeric variables with more than
  nine unique values and associated value labels.

## datawizard 0.6.5

CRAN release: 2022-12-14

MAJOR CHANGES

- Etienne Bacher is the new maintainer.

MINOR CHANGES

- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md),
  [`center()`](https://easystats.github.io/datawizard/reference/center.md),
  [`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  and
  [`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md)
  can be used in model formulas, similar to
  [`base::scale()`](https://rdrr.io/r/base/scale.html).

- [`data_codebook()`](https://easystats.github.io/datawizard/reference/data_codebook.md)
  now includes the proportion for each category/value, in addition to
  the counts. Furthermore, if data contains tagged `NA` values, these
  are included in the frequency table.

BUG FIXES

- `center(x)` now works correctly when `x` is a single value and either
  `reference` or `center` is specified
  ([\#324](https://github.com/easystats/datawizard/issues/324)).

- Fixed issue in
  [`data_codebook()`](https://easystats.github.io/datawizard/reference/data_codebook.md),
  which failed for labelled vectors when values of labels were not in
  sorted order.

## datawizard 0.6.4

CRAN release: 2022-11-19

NEW FUNCTIONS

- [`data_codebook()`](https://easystats.github.io/datawizard/reference/data_codebook.md):
  to generate codebooks of data frames.

- New functions to deal with duplicates:
  [`data_duplicated()`](https://easystats.github.io/datawizard/reference/data_duplicated.md)
  (keep all duplicates, including the first occurrence) and
  [`data_unique()`](https://easystats.github.io/datawizard/reference/data_unique.md)
  (returns the data, excluding all duplicates except one instance of
  each, based on the selected method).

MINOR CHANGES

- `.data.frame` methods should now preserve custom attributes.

- The `include_bounds` argument in
  [`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  can now also be a numeric value, defining the limit to the upper and
  lower bound (i.e. the distance to 1 and 0).

- [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)
  now works with grouped data.

BUG FIXES

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  no longer prints message for empty columns when the data actually had
  no empty columns.

- [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  now drops columns that are not in `id_cols` (if specified),
  `names_from`, or `values_from`. This is the behaviour observed in
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html).

## datawizard 0.6.3

CRAN release: 2022-10-22

MAJOR CHANGES

- There is a new publication about the
  [datawizard](https://easystats.github.io/datawizard/) package:
  <https://joss.theoj.org/papers/10.21105/joss.04684>

- Fixes failing tests due to changes in `R-devel`.

- [`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md)
  and
  [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  have had significant performance improvements, sometimes as high as a
  ten-fold speedup.

MINOR CHANGES

- When column names are misspelled, most functions now suggest which
  existing columns possibly could be meant.

- Miscellaneous performance gains.

- [`convert_to_na()`](https://easystats.github.io/datawizard/reference/convert_to_na.md)
  now requires argument `na` to be of class ‘Date’ to convert specific
  dates to `NA`. For example, `convert_to_na(x, na = "2022-10-17")` must
  be changed to `convert_to_na(x, na = as.Date("2022-10-17"))`.

BUG FIXES

- [`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md)
  and
  [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  now correctly keep the `date` format.

## datawizard 0.6.2

CRAN release: 2022-10-04

BREAKING CHANGES

- Methods for grouped data frames (`.grouped_df`) no longer support
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  for [dplyr](https://dplyr.tidyverse.org) before version `0.8.0`.

- [`empty_columns()`](https://easystats.github.io/datawizard/reference/remove_empty.md)
  and
  [`remove_empty_columns()`](https://easystats.github.io/datawizard/reference/remove_empty.md)
  now also remove columns that contain only empty characters. Likewise,
  [`empty_rows()`](https://easystats.github.io/datawizard/reference/remove_empty.md)
  and
  [`remove_empty_rows()`](https://easystats.github.io/datawizard/reference/remove_empty.md)
  remove observations that completely have missing or empty character
  values.

MINOR CHANGES

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  gains a `convert_factors` argument, to turn off automatic conversion
  from numeric variables into factors.

BUG FIXES

- [`data_arrange()`](https://easystats.github.io/datawizard/reference/data_arrange.md)
  now works with data frames that were grouped using
  [`data_group()`](https://easystats.github.io/datawizard/reference/data_group.md)
  ([\#274](https://github.com/easystats/datawizard/issues/274)).

## datawizard 0.6.1

CRAN release: 2022-09-25

- Updates tests for upcoming changes in the
  [tidyselect](https://tidyselect.r-lib.org) package
  ([\#267](https://github.com/easystats/datawizard/issues/267)).

## datawizard 0.6.0

CRAN release: 2022-09-15

BREAKING CHANGES

- The minimum needed R version has been bumped to `3.6`.

- Following deprecated functions have been removed:

`data_cut()`, `data_recode()`, `data_shift()`, `data_reverse()`,
`data_rescale()`, `data_to_factor()`, `data_to_numeric()`

- New
  [`text_format()`](https://easystats.github.io/datawizard/reference/text_format.md)
  alias is introduced for `format_text()`, latter of which will be
  removed in the next release.

- New
  [`recode_values()`](https://easystats.github.io/datawizard/reference/recode_values.md)
  alias is introduced for `change_code()`, latter of which will be
  removed in the next release.

- [`data_merge()`](https://easystats.github.io/datawizard/reference/data_merge.md)
  now errors if columns specified in `by` are not in both datasets.

- Using negative values in arguments `select` and `exclude` now removes
  the columns from the selection/exclusion. The previous behavior was to
  start the selection/exclusion from the end of the dataset, which was
  inconsistent with the use of “-” with other selecting possibilities.

NEW FUNCTIONS

- [`data_peek()`](https://easystats.github.io/datawizard/reference/data_peek.md):
  to peek at values and type of variables in a data frame.

- [`coef_var()`](https://easystats.github.io/datawizard/reference/coef_var.md):
  to compute the coefficient of variation.

CHANGES

- [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)
  will give more informative messages on malformed syntax of the
  `filter` argument.

- It is now possible to use curly brackets to pass variable names to
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md),
  like the following example. See examples section in the documentation
  of
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md).

- The `regex` argument was added to functions that use select-helpers
  and did not already have this argument.

- Select helpers
  [`starts_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  [`ends_with()`](https://tidyselect.r-lib.org/reference/starts_with.html),
  and
  [`contains()`](https://tidyselect.r-lib.org/reference/starts_with.html)
  now accept several patterns, e.g `starts_with("Sep", "Petal")`.

- Arguments `select` and `exclude` that are present in most functions
  have been improved to work in loops and in custom functions. For
  example, the following code now works:

``` r

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

- There is now a vignette summarizing the various ways to select or
  exclude variables in most
  [datawizard](https://easystats.github.io/datawizard/) functions.

## datawizard 0.5.1

CRAN release: 2022-08-17

- Fixes failing tests due to
  [poorman](https://nathaneastwood.github.io/poorman/) update.

## datawizard 0.5.0

CRAN release: 2022-08-07

MAJOR CHANGES

- Following statistical transformation functions have been renamed to
  not have `data_*()` prefix, since they do not work exclusively with
  data frames, but are typically first of all used with vectors, and
  therefore had misleading names:

  - `data_cut()` -\>
    [`categorize()`](https://easystats.github.io/datawizard/reference/categorize.md)

  - `data_recode()` -\> `change_code()`

  - `data_shift()` -\>
    [`slide()`](https://easystats.github.io/datawizard/reference/slide.md)

  - `data_reverse()` -\>
    [`reverse()`](https://easystats.github.io/datawizard/reference/reverse.md)

  - `data_rescale()` -\>
    [`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md)

  - `data_to_factor()` -\>
    [`to_factor()`](https://easystats.github.io/datawizard/reference/to_factor.md)

  - `data_to_numeric()` -\>
    [`to_numeric()`](https://easystats.github.io/datawizard/reference/to_numeric.md)

Note that these functions also have `.data.frame()` methods and still
work for data frames as well. Former function names are still available
as aliases, but will be deprecated and removed in a future release.

- Bumps the needed minimum R version to `3.5`.

- Removed deprecated function `data_findcols()`. Please use its
  replacement, `data_find()`.

- Removed alias
  [`extract()`](https://tidyr.tidyverse.org/reference/extract.html) for
  [`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md)
  function since it collided with
  [`tidyr::extract()`](https://tidyr.tidyverse.org/reference/extract.html).

- Argument `training_proportion` in
  [`data_partition()`](https://easystats.github.io/datawizard/reference/data_partition.md)
  is deprecated. Please use `proportion` now.

- Given his continued and significant contributions to the package,
  Etienne Bacher ([@etiennebacher](https://github.com/etiennebacher)) is
  now included as an author.

- [`unstandardise()`](https://easystats.github.io/datawizard/reference/standardize.md)
  now works for `center(x)`

- [`unnormalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  now works for `change_scale(x)`

- [`reshape_wider()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  now follows more consistently
  [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  syntax. Arguments `colnames_from`, `sep`, and `rows_from` are
  deprecated and should be replaced by `names_from`, `names_sep`, and
  `id_cols` respectively.
  [`reshape_wider()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  also gains an argument `names_glue`
  ([\#182](https://github.com/easystats/datawizard/issues/182),
  [\#198](https://github.com/easystats/datawizard/issues/198)).

- Similarly,
  [`reshape_longer()`](https://easystats.github.io/datawizard/reference/data_to_long.md)
  now follows more consistently
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  syntax. Argument `colnames_to` is deprecated and should be replaced by
  `names_to`.
  [`reshape_longer()`](https://easystats.github.io/datawizard/reference/data_to_long.md)
  also gains new arguments: `names_prefix`, `names_sep`,
  `names_pattern`, and `values_drop_na`
  ([\#189](https://github.com/easystats/datawizard/issues/189)).

CHANGES

- Some of the text formatting helpers (like
  [`text_concatenate()`](https://easystats.github.io/datawizard/reference/text_format.md))
  gain an `enclose` argument, to wrap text elements with surrounding
  characters.

- `winsorize` now accepts “raw” and “zscore” methods (in addition to
  “percentile”). Additionally, when `robust` is set to `TRUE` together
  with `method = "zscore"`, winsorizes via the median and median
  absolute deviation (MAD); else via the mean and standard deviation.
  ([@rempsyc](https://github.com/rempsyc),
  [\#177](https://github.com/easystats/datawizard/issues/177),
  [\#49](https://github.com/easystats/datawizard/issues/49),
  [\#47](https://github.com/easystats/datawizard/issues/47)).

- `convert_na_to` now accepts numeric replacements on character vectors
  and single replacement for multiple vector classes.
  ([@rempsyc](https://github.com/rempsyc),
  [\#214](https://github.com/easystats/datawizard/issues/214)).

- [`data_partition()`](https://easystats.github.io/datawizard/reference/data_partition.md)
  now allows to create multiple partitions from the data, returning
  multiple training and a remaining test set.

- Functions like
  [`center()`](https://easystats.github.io/datawizard/reference/center.md),
  [`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  or
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  no longer fail when data contains infinite values (`Inf`).

NEW FUNCTIONS

- [`row_to_colnames()`](https://easystats.github.io/datawizard/reference/colnames.md)
  and
  [`colnames_to_row()`](https://easystats.github.io/datawizard/reference/colnames.md)
  to move a row to column names, and column names to row
  ([@etiennebacher](https://github.com/etiennebacher),
  [\#169](https://github.com/easystats/datawizard/issues/169)).

- [`data_arrange()`](https://easystats.github.io/datawizard/reference/data_arrange.md)
  to sort the rows of a dataframe according to the values of the
  selected columns.

BUG FIXES

- Fixed wrong column names in
  [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  ([\#173](https://github.com/easystats/datawizard/issues/173)).

## datawizard 0.4.1

CRAN release: 2022-05-16

BREAKING

- Added the
  [`standardize.default()`](https://easystats.github.io/datawizard/reference/standardize.default.md)
  method (moved from package **effectsize**), to be consistent in that
  the default-method now is in the same package as the generic.
  [`standardize.default()`](https://easystats.github.io/datawizard/reference/standardize.default.md)
  behaves exactly like in **effectsize** and particularly works for
  regression model objects. **effectsize** now re-exports
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  from **datawizard**.

NEW FUNCTIONS

- `data_shift()` to shift the value range of numeric variables.

- `data_recode()` to recode old into new values.

- `data_to_factor()` as counterpart to `data_to_numeric()`.

- [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  to create frequency tables of variables.

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  to read (import) data files (from text, or foreign statistical
  packages).

- [`unnormalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  as counterpart to
  [`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md).
  This function only works for variables that have been normalized with
  [`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md).

- [`data_group()`](https://easystats.github.io/datawizard/reference/data_group.md)
  and
  [`data_ungroup()`](https://easystats.github.io/datawizard/reference/data_group.md)
  to create grouped data frames, or to remove the grouping information
  from grouped data frames.

CHANGES

- `data_find()` was added as alias to `find_colums()`, to have
  consistent name patterns for the **datawizard** functions.
  `data_findcols()` will be removed in a future update and usage is
  discouraged.

- The `select` argument (and thus, also the `exclude` argument) now also
  accepts functions testing for logical conditions,
  e.g. [`is.numeric()`](https://rdrr.io/r/base/numeric.html) (or
  `is.numeric`), or any user-defined function that selects the variables
  for which the function returns `TRUE` (like:
  `foo <- function(x) mean(x) > 3`).

- Arguments `select` and `exclude` now allow the negation of
  select-helpers, like `-ends_with("")`, `-is.numeric` or
  `-Sepal.Width:Petal.Length`.

- Many functions now get a `.default` method, to capture unsupported
  classes. This now yields a message and returns the original input, and
  hence, the `.data.frame` methods won’t stop due to an error.

- The `filter` argument in
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)
  can also be a numeric vector, to indicate row indices of those rows
  that should be returned.

- [`convert_to_na()`](https://easystats.github.io/datawizard/reference/convert_to_na.md)
  gets methods for variables of class `logical` and `Date`.

- [`convert_to_na()`](https://easystats.github.io/datawizard/reference/convert_to_na.md)
  for factors (and data frames) gains a `drop_levels` argument, to drop
  unused levels that have been replaced by `NA`.

- `data_to_numeric()` gains two more arguments, `preserve_levels` and
  `lowest`, to give better control of conversion of factors.

BUG FIXES

- When logicals were passed to
  [`center()`](https://easystats.github.io/datawizard/reference/center.md)
  or
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  and `force = TRUE`, these were not properly converted to numeric
  variables.

## datawizard 0.4.0

CRAN release: 2022-03-30

MAJOR CHANGES

- [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md)
  now returns filtered data by default. Old behavior (returning rows
  indices) can be set by setting `return_indices = TRUE`.

- The following functions are now re-exported from
  [insight](https://easystats.github.io/insight/) package:
  `object_has_names()`, `object_has_rownames()`, `is_empty_object()`,
  `compact_list()`, `compact_character()`

- `data_findcols()` will become deprecated in future updates. Please use
  the new replacements
  [`find_columns()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
  and `get_columns()`.

- The vignette *Analysing Longitudinal or Panel Data* has now moved to
  [parameters
  package](https://easystats.github.io/parameters/articles/demean.html).

NEW FUNCTIONS

- To convert rownames to a column, and *vice versa*:
  [`rownames_as_column()`](https://easystats.github.io/datawizard/reference/rownames.md)
  and
  [`column_as_rownames()`](https://easystats.github.io/datawizard/reference/rownames.md)
  ([@etiennebacher](https://github.com/etiennebacher),
  [\#80](https://github.com/easystats/datawizard/issues/80)).

- [`find_columns()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
  and `get_columns()` to find column names or retrieve subsets of data
  frames, based on various select-methods (including select-helpers).
  These function will supersede `data_findcols()` in the future.

- [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)
  as complement for
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md),
  which works with logical expressions for filtering rows of data
  frames.

- For computing weighted centrality measures and dispersion:
  [`weighted_mean()`](https://easystats.github.io/datawizard/reference/weighted_mean.md),
  [`weighted_median()`](https://easystats.github.io/datawizard/reference/weighted_mean.md),
  [`weighted_sd()`](https://easystats.github.io/datawizard/reference/weighted_mean.md)
  and
  [`weighted_mad()`](https://easystats.github.io/datawizard/reference/weighted_mean.md).

- To replace `NA` in vectors and dataframes:
  [`convert_na_to()`](https://easystats.github.io/datawizard/reference/convert_na_to.md)
  ([@etiennebacher](https://github.com/etiennebacher),
  [\#111](https://github.com/easystats/datawizard/issues/111)).

MINOR CHANGES

- The `select` argument in several functions (like
  [`data_remove()`](https://easystats.github.io/datawizard/reference/data_relocate.md),
  [`reshape_longer()`](https://easystats.github.io/datawizard/reference/data_to_long.md),
  or
  [`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md))
  now allows the use of select-helpers for selecting variables based on
  specific patterns.

- [`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md)
  gains new arguments to allow type-safe return values,

i.e. *always* return a vector *or* a data frame. Thus,
[`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md)
can now be used to select multiple variables or pull a single variable
from data frames.

- [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md)
  gains a `match` argument, to indicate with which logical operation
  matching results should be combined.

- Improved support for *labelled data* for many functions, i.e. returned
  data frame will preserve value and variable label attributes, where
  possible and applicable.

- [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  now works with lists
  ([@etiennebacher](https://github.com/etiennebacher),
  [\#105](https://github.com/easystats/datawizard/issues/105)).

- [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
  doesn’t use `pattern` anymore to rename the columns if `replacement`
  is not provided ([@etiennebacher](https://github.com/etiennebacher),
  [\#103](https://github.com/easystats/datawizard/issues/103)).

- [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
  now adds a suffix to duplicated names in `replacement`
  ([@etiennebacher](https://github.com/etiennebacher),
  [\#103](https://github.com/easystats/datawizard/issues/103)).

BUG FIXES

- `data_to_numeric()` produced wrong results for factors when
  `dummy_factors = TRUE` and factor contained missing values.

- [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md)
  produced wrong results when data contained missing values.

- Fixed CRAN check issues in
  [`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md)
  when more than one variable was extracted from a data frame.

## datawizard 0.3.0

CRAN release: 2022-03-02

NEW FUNCTIONS

- To find or remove empty rows and columns in a data frame:
  [`empty_rows()`](https://easystats.github.io/datawizard/reference/remove_empty.md),
  [`empty_columns()`](https://easystats.github.io/datawizard/reference/remove_empty.md),
  [`remove_empty_rows()`](https://easystats.github.io/datawizard/reference/remove_empty.md),
  [`remove_empty_columns()`](https://easystats.github.io/datawizard/reference/remove_empty.md),
  and `remove_empty`.

- To check for names: `object_has_names()` and `object_has_rownames()`.

- To rotate data frames:
  [`data_rotate()`](https://easystats.github.io/datawizard/reference/data_rotate.md).

- To reverse score variables: `data_reverse()`.

- To merge/join multiple data frames:
  [`data_merge()`](https://easystats.github.io/datawizard/reference/data_merge.md)
  (or its alias
  [`data_join()`](https://easystats.github.io/datawizard/reference/data_merge.md)).

- To cut (recode) data into groups: `data_cut()`.

- To replace specific values with `NA`s:
  [`convert_to_na()`](https://easystats.github.io/datawizard/reference/convert_to_na.md).

- To replace `Inf` and `NaN` values with `NA`s:
  [`replace_nan_inf()`](https://easystats.github.io/datawizard/reference/replace_nan_inf.md).

- Arguments `cols`, `before` and `after` in
  [`data_relocate()`](https://easystats.github.io/datawizard/reference/data_relocate.md)
  can now also be numeric values, indicating the position of the
  destination column.

## datawizard 0.2.3

CRAN release: 2022-01-26

- New functions:

  - to work with lists: `is_empty_object()` and `compact_list()`

  - to work with strings: `compact_character()`

## datawizard 0.2.2

CRAN release: 2022-01-04

- New function
  [`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md)
  (or its alias
  [`extract()`](https://tidyr.tidyverse.org/reference/extract.html)) to
  pull single variables from a data frame, possibly naming each value by
  the row names of that data frame.

- [`reshape_ci()`](https://easystats.github.io/datawizard/reference/reshape_ci.md)
  gains a `ci_type` argument, to reshape data frames where CI-columns
  have prefixes other than `"CI"`.

- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  and
  [`center()`](https://easystats.github.io/datawizard/reference/center.md)
  gain arguments `center` and `scale`, to define references for
  centrality and deviation that are used when centering or standardizing
  variables.

- [`center()`](https://easystats.github.io/datawizard/reference/center.md)
  gains the arguments `force` and `reference`, similar to
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md).

- The functionality of the `append` argument in
  [`center()`](https://easystats.github.io/datawizard/reference/center.md)
  and
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  was revised. This made the `suffix` argument redundant, and thus it
  was removed.

- Fixed issue in
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md).

- Fixed issue in `data_findcols()`.

## datawizard 0.2.1

CRAN release: 2021-10-04

- Exports `plot` method for
  [`visualisation_recipe()`](https://easystats.github.io/datawizard/reference/visualisation_recipe.md)
  objects from [see](https://easystats.github.io/see/) package.

- [`centre()`](https://easystats.github.io/datawizard/reference/center.md),
  [`standardise()`](https://easystats.github.io/datawizard/reference/standardize.md),
  [`unstandardise()`](https://easystats.github.io/datawizard/reference/standardize.md)
  are exported as aliases for
  [`center()`](https://easystats.github.io/datawizard/reference/center.md),
  [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md),
  [`unstandardize()`](https://easystats.github.io/datawizard/reference/standardize.md),
  respectively.

## datawizard 0.2.0.1

CRAN release: 2021-09-02

- This is mainly a maintenance release that addresses some issues with
  conflicting namespaces.

## datawizard 0.2.0

CRAN release: 2021-08-17

- New function:
  [`visualisation_recipe()`](https://easystats.github.io/datawizard/reference/visualisation_recipe.md).

- The following function has now moved to *performance* package:
  `check_multimodal()`.

- Minor updates to documentation, including a new vignette about
  [`demean()`](https://easystats.github.io/datawizard/reference/demean.md).

## datawizard 0.1.0

CRAN release: 2021-06-18

- First release.
