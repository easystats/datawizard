# Package index

## Data Preparation

Main functions for cleaning and preparing data

- [`data_to_long()`](https://easystats.github.io/datawizard/reference/data_to_long.md)
  [`reshape_longer()`](https://easystats.github.io/datawizard/reference/data_to_long.md)
  : Reshape (pivot) data from wide to long
- [`data_to_wide()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  [`reshape_wider()`](https://easystats.github.io/datawizard/reference/data_to_wide.md)
  : Reshape (pivot) data from long to wide
- [`data_extract()`](https://easystats.github.io/datawizard/reference/data_extract.md)
  : Extract one or more columns or elements from an object
- [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md)
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)
  : Return filtered or sliced data frame, or row indices
- [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
  [`extract_column_names()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
  [`find_columns()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
  : Find or get columns in a data frame based on search patterns
- [`data_relocate()`](https://easystats.github.io/datawizard/reference/data_relocate.md)
  [`data_reorder()`](https://easystats.github.io/datawizard/reference/data_relocate.md)
  [`data_remove()`](https://easystats.github.io/datawizard/reference/data_relocate.md)
  : Relocate (reorder) columns of a data frame
- [`data_arrange()`](https://easystats.github.io/datawizard/reference/data_arrange.md)
  : Arrange rows by column values
- [`data_merge()`](https://easystats.github.io/datawizard/reference/data_merge.md)
  [`data_join()`](https://easystats.github.io/datawizard/reference/data_merge.md)
  : Merge (join) two data frames, or a list of data frames
- [`data_partition()`](https://easystats.github.io/datawizard/reference/data_partition.md)
  : Partition data
- [`data_rotate()`](https://easystats.github.io/datawizard/reference/data_rotate.md)
  [`data_transpose()`](https://easystats.github.io/datawizard/reference/data_rotate.md)
  : Rotate a data frame
- [`data_group()`](https://easystats.github.io/datawizard/reference/data_group.md)
  [`data_ungroup()`](https://easystats.github.io/datawizard/reference/data_group.md)
  : Create a grouped data frame
- [`data_replicate()`](https://easystats.github.io/datawizard/reference/data_replicate.md)
  : Expand (i.e. replicate rows) a data frame
- [`data_duplicated()`](https://easystats.github.io/datawizard/reference/data_duplicated.md)
  : Extract all duplicates
- [`data_unique()`](https://easystats.github.io/datawizard/reference/data_unique.md)
  : Keep only one row from all with duplicated IDs

## Data and Variable Transformations

### Statistical Transformations

Functions for transforming variables

- [`data_modify()`](https://easystats.github.io/datawizard/reference/data_modify.md)
  : Create new variables in a data frame
- [`data_separate()`](https://easystats.github.io/datawizard/reference/data_separate.md)
  : Separate single variable into multiple variables
- [`data_unite()`](https://easystats.github.io/datawizard/reference/data_unite.md)
  : Unite ("merge") multiple variables
- [`categorize()`](https://easystats.github.io/datawizard/reference/categorize.md)
  : Recode (or "cut" / "bin") data into groups of values.
- [`recode_into()`](https://easystats.github.io/datawizard/reference/recode_into.md)
  : Recode values from one or more variables into a new variable
- [`recode_values()`](https://easystats.github.io/datawizard/reference/recode_values.md)
  : Recode old values of variables into new values
- [`adjust()`](https://easystats.github.io/datawizard/reference/adjust.md)
  [`data_adjust()`](https://easystats.github.io/datawizard/reference/adjust.md)
  : Adjust data for the effect of other variable(s)
- [`demean()`](https://easystats.github.io/datawizard/reference/demean.md)
  [`degroup()`](https://easystats.github.io/datawizard/reference/demean.md)
  [`detrend()`](https://easystats.github.io/datawizard/reference/demean.md)
  : Compute group-meaned and de-meaned variables
- [`ranktransform()`](https://easystats.github.io/datawizard/reference/ranktransform.md)
  : (Signed) rank transformation
- [`rescale_weights()`](https://easystats.github.io/datawizard/reference/rescale_weights.md)
  : Rescale design weights for multilevel analysis
- [`winsorize()`](https://easystats.github.io/datawizard/reference/winsorize.md)
  : Winsorize data

### Linear Transformers

Convenient functions for common linear transformations

- [`center()`](https://easystats.github.io/datawizard/reference/center.md)
  [`centre()`](https://easystats.github.io/datawizard/reference/center.md)
  : Centering (Grand-Mean Centering)

- [`slide()`](https://easystats.github.io/datawizard/reference/slide.md)
  : Shift numeric value range

- [`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  [`standardise()`](https://easystats.github.io/datawizard/reference/standardize.md)
  [`unstandardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
  [`unstandardise()`](https://easystats.github.io/datawizard/reference/standardize.md)
  : Standardization (Z-scoring)

- [`standardize(`*`<default>`*`)`](https://easystats.github.io/datawizard/reference/standardize.default.md)
  : Re-fit a model with standardized data

- [`reverse()`](https://easystats.github.io/datawizard/reference/reverse.md)
  [`reverse_scale()`](https://easystats.github.io/datawizard/reference/reverse.md)
  : Reverse-Score Variables

- [`rescale()`](https://easystats.github.io/datawizard/reference/rescale.md)
  [`change_scale()`](https://easystats.github.io/datawizard/reference/rescale.md)
  : Rescale Variables to a New Range

- [`normalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  [`unnormalize()`](https://easystats.github.io/datawizard/reference/normalize.md)
  : Normalize numeric variable to 0-1 range

- [`makepredictcall(`*`<dw_transformer>`*`)`](https://easystats.github.io/datawizard/reference/makepredictcall.dw_transformer.md)
  :

  Utility Function for Safe Prediction with `datawizard` transformers

### Others

- [`contr.deviation()`](https://easystats.github.io/datawizard/reference/contr.deviation.md)
  : Deviation Contrast Matrix

## Data Properties

Functions to compute statistical summaries of data properties and
distributions

- [`as.prop.table()`](https://easystats.github.io/datawizard/reference/as.prop.table.md)
  [`as.data.frame(`*`<datawizard_tables>`*`)`](https://easystats.github.io/datawizard/reference/as.prop.table.md)
  [`as.table(`*`<datawizard_table>`*`)`](https://easystats.github.io/datawizard/reference/as.prop.table.md)
  : Convert a crosstable to a frequency or a propensity table
- [`data_codebook()`](https://easystats.github.io/datawizard/reference/data_codebook.md)
  [`print_html(`*`<data_codebook>`*`)`](https://easystats.github.io/datawizard/reference/data_codebook.md)
  [`display(`*`<data_codebook>`*`)`](https://easystats.github.io/datawizard/reference/data_codebook.md)
  : Generate a codebook of a data frame.
- [`data_summary()`](https://easystats.github.io/datawizard/reference/data_summary.md)
  : Summarize data
- [`data_tabulate()`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  [`print(`*`<datawizard_table>`*`)`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  [`display(`*`<datawizard_table>`*`)`](https://easystats.github.io/datawizard/reference/data_tabulate.md)
  : Create frequency and crosstables of variables
- [`data_peek()`](https://easystats.github.io/datawizard/reference/data_peek.md)
  : Peek at values and type of variables in a data frame
- [`data_seek()`](https://easystats.github.io/datawizard/reference/data_seek.md)
  : Find variables by their names, variable or value labels
- [`means_by_group()`](https://easystats.github.io/datawizard/reference/means_by_group.md)
  : Summary of mean values by group
- [`coef_var()`](https://easystats.github.io/datawizard/reference/coef_var.md)
  [`distribution_coef_var()`](https://easystats.github.io/datawizard/reference/coef_var.md)
  : Compute the coefficient of variation
- [`describe_distribution()`](https://easystats.github.io/datawizard/reference/describe_distribution.md)
  : Describe a distribution
- [`distribution_mode()`](https://easystats.github.io/datawizard/reference/distribution_mode.md)
  : Compute mode for a statistical distribution
- [`skewness()`](https://easystats.github.io/datawizard/reference/skewness.md)
  [`kurtosis()`](https://easystats.github.io/datawizard/reference/skewness.md)
  [`print(`*`<parameters_kurtosis>`*`)`](https://easystats.github.io/datawizard/reference/skewness.md)
  [`print(`*`<parameters_skewness>`*`)`](https://easystats.github.io/datawizard/reference/skewness.md)
  [`summary(`*`<parameters_skewness>`*`)`](https://easystats.github.io/datawizard/reference/skewness.md)
  [`summary(`*`<parameters_kurtosis>`*`)`](https://easystats.github.io/datawizard/reference/skewness.md)
  : Compute Skewness and (Excess) Kurtosis
- [`smoothness()`](https://easystats.github.io/datawizard/reference/smoothness.md)
  : Quantify the smoothness of a vector
- [`row_count()`](https://easystats.github.io/datawizard/reference/row_count.md)
  : Count specific values row-wise
- [`row_means()`](https://easystats.github.io/datawizard/reference/row_means.md)
  [`row_sums()`](https://easystats.github.io/datawizard/reference/row_means.md)
  : Row means or sums (optionally with minimum amount of valid values)
- [`weighted_mean()`](https://easystats.github.io/datawizard/reference/weighted_mean.md)
  [`weighted_median()`](https://easystats.github.io/datawizard/reference/weighted_mean.md)
  [`weighted_sd()`](https://easystats.github.io/datawizard/reference/weighted_mean.md)
  [`weighted_mad()`](https://easystats.github.io/datawizard/reference/weighted_mean.md)
  : Weighted Mean, Median, SD, and MAD
- [`mean_sd()`](https://easystats.github.io/datawizard/reference/mean_sd.md)
  [`median_mad()`](https://easystats.github.io/datawizard/reference/mean_sd.md)
  : Summary Helpers

## Convert and Replace Data

Helpers for data replacements

- [`assign_labels()`](https://easystats.github.io/datawizard/reference/assign_labels.md)
  : Assign variable and value labels

- [`labels_to_levels()`](https://easystats.github.io/datawizard/reference/labels_to_levels.md)
  : Convert value labels into factor levels

- [`coerce_to_numeric()`](https://easystats.github.io/datawizard/reference/coerce_to_numeric.md)
  : Convert to Numeric (if possible)

- [`to_numeric()`](https://easystats.github.io/datawizard/reference/to_numeric.md)
  : Convert data to numeric

- [`to_factor()`](https://easystats.github.io/datawizard/reference/to_factor.md)
  : Convert data to factors

- [`replace_nan_inf()`](https://easystats.github.io/datawizard/reference/replace_nan_inf.md)
  :

  Convert infinite or `NaN` values into `NA`

- [`convert_na_to()`](https://easystats.github.io/datawizard/reference/convert_na_to.md)
  : Replace missing values in a variable or a data frame.

- [`convert_to_na()`](https://easystats.github.io/datawizard/reference/convert_to_na.md)
  : Convert non-missing values in a variable into missing values.

## Import data

Helpers for importing data

- [`data_read()`](https://easystats.github.io/datawizard/reference/data_read.md)
  [`data_write()`](https://easystats.github.io/datawizard/reference/data_read.md)
  : Read (import) data files from various sources

## Helpers for Data Preparation

Primarily useful in the context of other ‘easystats’ packages

- [`reshape_ci()`](https://easystats.github.io/datawizard/reference/reshape_ci.md)
  : Reshape CI between wide/long formats
- [`data_rename()`](https://easystats.github.io/datawizard/reference/data_rename.md)
  [`data_rename_rows()`](https://easystats.github.io/datawizard/reference/data_rename.md)
  : Rename columns and variable names
- [`data_addprefix()`](https://easystats.github.io/datawizard/reference/data_prefix_suffix.md)
  [`data_addsuffix()`](https://easystats.github.io/datawizard/reference/data_prefix_suffix.md)
  : Add a prefix or suffix to column names
- [`empty_columns()`](https://easystats.github.io/datawizard/reference/remove_empty.md)
  [`empty_rows()`](https://easystats.github.io/datawizard/reference/remove_empty.md)
  [`remove_empty_columns()`](https://easystats.github.io/datawizard/reference/remove_empty.md)
  [`remove_empty_rows()`](https://easystats.github.io/datawizard/reference/remove_empty.md)
  [`remove_empty()`](https://easystats.github.io/datawizard/reference/remove_empty.md)
  : Return or remove variables or observations that are completely
  missing
- [`rownames_as_column()`](https://easystats.github.io/datawizard/reference/rownames.md)
  [`column_as_rownames()`](https://easystats.github.io/datawizard/reference/rownames.md)
  [`rowid_as_column()`](https://easystats.github.io/datawizard/reference/rownames.md)
  : Tools for working with row names or row ids
- [`row_to_colnames()`](https://easystats.github.io/datawizard/reference/colnames.md)
  [`colnames_to_row()`](https://easystats.github.io/datawizard/reference/colnames.md)
  : Tools for working with column names
- [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
  [`extract_column_names()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
  [`find_columns()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)
  : Find or get columns in a data frame based on search patterns
- [`data_restoretype()`](https://easystats.github.io/datawizard/reference/data_restoretype.md)
  : Restore the type of columns according to a reference data frame

## Helpers for Text Formatting

Primarily useful for ‘report’ package

- [`text_format()`](https://easystats.github.io/datawizard/reference/text_format.md)
  [`text_fullstop()`](https://easystats.github.io/datawizard/reference/text_format.md)
  [`text_lastchar()`](https://easystats.github.io/datawizard/reference/text_format.md)
  [`text_concatenate()`](https://easystats.github.io/datawizard/reference/text_format.md)
  [`text_paste()`](https://easystats.github.io/datawizard/reference/text_format.md)
  [`text_remove()`](https://easystats.github.io/datawizard/reference/text_format.md)
  [`text_wrap()`](https://easystats.github.io/datawizard/reference/text_format.md)
  : Convenient text formatting functionalities

## Visualization helpers

Primarily useful in the context of other ‘easystats’ packages

- [`visualisation_recipe()`](https://easystats.github.io/datawizard/reference/visualisation_recipe.md)
  : Prepare objects for visualisation

## Data

Datasets useful for examples and tests

- [`efc`](https://easystats.github.io/datawizard/reference/efc.md) :
  Sample dataset from the EFC Survey
- [`nhanes_sample`](https://easystats.github.io/datawizard/reference/nhanes_sample.md)
  : Sample dataset from the National Health and Nutrition Examination
  Survey
