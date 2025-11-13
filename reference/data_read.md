# Read (import) data files from various sources

This functions imports data from various file types. It is a small
wrapper around
[`haven::read_spss()`](https://haven.tidyverse.org/reference/read_spss.html),
[`haven::read_stata()`](https://haven.tidyverse.org/reference/read_dta.html),
[`haven::read_sas()`](https://haven.tidyverse.org/reference/read_sas.html),
[`readxl::read_excel()`](https://readxl.tidyverse.org/reference/read_excel.html)
and
[`data.table::fread()`](https://rdatatable.gitlab.io/data.table/reference/fread.html)
resp.
[`readr::read_delim()`](https://readr.tidyverse.org/reference/read_delim.html)
(the latter if package **data.table** is not installed). Thus, supported
file types for importing data are data files from SPSS, SAS or Stata,
Excel files or text files (like '.csv' files). All other file types are
passed to
[`rio::import()`](http://gesistsa.github.io/rio/reference/import.md).
`data_write()` works in a similar way.

## Usage

``` r
data_read(
  path,
  path_catalog = NULL,
  encoding = NULL,
  convert_factors = TRUE,
  verbose = TRUE,
  ...
)

data_write(
  data,
  path,
  delimiter = ",",
  convert_factors = FALSE,
  save_labels = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- path:

  Character string, the file path to the data file.

- path_catalog:

  Character string, path to the catalog file. Only relevant for SAS data
  files.

- encoding:

  The character encoding used for the file. Usually not needed.

- convert_factors:

  If `TRUE` (default), numeric variables, where all values have a value
  label, are assumed to be categorical and converted into factors. If
  `FALSE`, no variable types are guessed and no conversion of numeric
  variables into factors will be performed. For `data_read()`, this
  argument only applies to file types with *labelled data*, e.g. files
  from SPSS, SAS or Stata. See also section 'Differences to other
  packages'. For `data_write()`, this argument only applies to the text
  (e.g. `.txt` or `.csv`) or spreadsheet file formats (like `.xlsx`).
  Converting to factors might be useful for these formats because
  labelled numeric variables are then converted into factors and
  exported as character columns - else, value labels would be lost and
  only numeric values are written to the file.

- verbose:

  Toggle warnings and messages.

- ...:

  Arguments passed to the related `read_*()` or `write_*()` functions.

- data:

  The data frame that should be written to a file.

- delimiter:

  For CSV-files, specifies the delimiter. Defaults to `","`, but in
  particular in European regions, `";"` might be a useful alternative,
  especially when exported CSV-files should be opened in Excel.

- save_labels:

  Only applies to CSV files. If `TRUE`, value and variable labels (if
  any) will be saved as additional CSV file. This file has the same file
  name as the exported CSV file, but includes a `"_labels"` suffix (i.e.
  when the file name is `"mydat.csv"`, the additional file with value
  and variable labels is named `"mydat_labels.csv"`).

## Value

A data frame.

## Supported file types

- `data_read()` is a wrapper around the **haven**, **data.table**,
  **readr** **readxl**, **nanoparquet** and **rio** packages. Currently
  supported file types are `.txt`, `.csv`, `.xls`, `.xlsx`, `.sav`,
  `.por`, `.dta`, `.sas`, `.rda`, `.parquet`, `.rdata`, and `.rds` (and
  related files). All other file types are passed to
  [`rio::import()`](http://gesistsa.github.io/rio/reference/import.md).

- `data_write()` is a wrapper around **haven**, **readr**,
  **nanoparquet**, and **rio** packages, and supports writing files into
  all formats supported by these packages.

## Compressed files (zip) and URLs

`data_read()` can also read the above mentioned files from URLs or from
inside zip-compressed files. Thus, `path` can also be a URL to a file
like `"http://www.url.com/file.csv"`. When `path` points to a
zip-compressed file, and there are multiple files inside the
zip-archive, then the first supported file is extracted and loaded.

## General behaviour

`data_read()` detects the appropriate `read_*()` function based on the
file-extension of the data file. Thus, in most cases it should be enough
to only specify the `path` argument. However, if more control is needed,
all arguments in `...` are passed down to the related `read_*()`
function. The same applies to `data_write()`, i.e. based on the file
extension provided in `path`, the appropriate `write_*()` function is
used automatically.

## SPSS specific behaviour

`data_read()` does *not* import user-defined ("tagged") `NA` values from
SPSS, i.e. argument `user_na` is always set to `FALSE` when importing
SPSS data with the **haven** package. Use
[`convert_to_na()`](https://easystats.github.io/datawizard/reference/convert_to_na.md)
to define missing values in the imported data, if necessary.
Furthermore, `data_write()` compresses SPSS files by default. If this
causes problems with (older) SPSS versions, use `compress = "none"`, for
example `data_write(data, "myfile.sav", compress = "none")`.

## Differences to other packages that read foreign data formats

`data_read()` is most comparable to
[`rio::import()`](http://gesistsa.github.io/rio/reference/import.md).
For data files from SPSS, SAS or Stata, which support labelled data,
variables are converted into their most appropriate type. The major
difference to
[`rio::import()`](http://gesistsa.github.io/rio/reference/import.md) is
for data files from SPSS, SAS, or Stata, i.e. file types that support
*labelled data*. `data_read()` automatically converts fully labelled
numeric variables into factors, where imported value labels will be set
as factor levels. If a numeric variable has *no* value labels or less
value labels than values, it is not converted to factor. In this case,
value labels are preserved as `"labels"` attribute. Character vectors
are preserved. Use `convert_factors = FALSE` to remove the automatic
conversion of numeric variables to factors.
