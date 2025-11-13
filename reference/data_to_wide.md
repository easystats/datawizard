# Reshape (pivot) data from long to wide

This function "widens" data, increasing the number of columns and
decreasing the number of rows. This is a dependency-free base-R
equivalent of
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html).

## Usage

``` r
data_to_wide(
  data,
  id_cols = NULL,
  values_from = "Value",
  names_from = "Name",
  names_sep = "_",
  names_prefix = "",
  names_glue = NULL,
  values_fill = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)

reshape_wider(
  data,
  id_cols = NULL,
  values_from = "Value",
  names_from = "Name",
  names_sep = "_",
  names_prefix = "",
  names_glue = NULL,
  values_fill = NULL,
  ignore_case = FALSE,
  regex = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  A data frame to convert to wide format, so that it has more columns
  and fewer rows post-widening than pre-widening.

- id_cols:

  The name of the column that identifies the rows in the data by which
  observations are grouped and the gathered data is spread into new
  columns. Usually, this is a variable containing an ID for observations
  that have been repeatedly measured. If `NULL`, it will use all
  remaining columns that are not in `names_from` or `values_from` as ID
  columns. `id_cols` can also be a character vector with more than one
  name of identifier columns. See also 'Details' and 'Examples'.

- values_from:

  The name of the columns in the original data that contains the values
  used to fill the new columns created in the widened data. Can also be
  one of the selection helpers (see argument `select` in
  [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)).

- names_from:

  The name of the column in the original data whose values will be used
  for naming the new columns created in the widened data. Each unique
  value in this column will become the name of one of these new columns.
  In case `names_prefix` is provided, column names will be concatenated
  with the string given in `names_prefix`. If `values_from` specifies
  more than one variable that should be widened, the new column names
  are a combination of the old column names in `values_from` and the
  *values* from `names_from`, to avoid duplicate column names.

- names_sep:

  If `names_from` or `values_from` contains multiple variables, this
  will be used to join their values together into a single string to use
  as a column name.

- names_prefix:

  String added to the start of every variable name. This is particularly
  useful if `names_from` is a numeric vector and you want to create
  syntactic variable names.

- names_glue:

  Instead of `names_sep` and `names_prefix`, you can supply a [glue
  specification](https://glue.tidyverse.org/index.html) that uses the
  `names_from` columns to create custom column names. Note that the only
  delimiters supported by `names_glue` are curly brackets, `{` and `}`.

- values_fill:

  Defunct argument, which has no function anymore. Will be removed in
  future versions.

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

- ...:

  Not used for now.

## Value

If a tibble was provided as input, `data_to_wide()` also returns a
tibble. Otherwise, it returns a data frame.

## Details

Reshaping data into wide format usually means that the input data frame
is in *long* format, where multiple measurements taken on the same
subject are stored in multiple rows. The wide format stores the same
information in a single row, with each measurement stored in a separate
column. Thus, the necessary information for `data_to_wide()` is:

- The name of the column(s) that identify the groups or repeated
  measurements (`id_cols`).

- The name of the column whose *values* will become the new column names
  (`names_from`). Since these values may not necessarily reflect
  appropriate column names, you can use `names_prefix` to add a prefix
  to each newly created column name.

- The name of the column(s) that contain the values (`values_from`) for
  the new columns that are created by `names_from`.

In other words: repeated measurements, as indicated by `id_cols`, that
are saved into the column `values_from` will be spread into new columns,
which will be named after the values in `names_from`. See also
'Examples'.

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
  `data_to_wide()`,
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
  [`data_merge()`](https://easystats.github.io/datawizard/reference/data_merge.md)

- Functions to find or select columns:
  [`data_select()`](https://easystats.github.io/datawizard/reference/extract_column_names.md),
  [`extract_column_names()`](https://easystats.github.io/datawizard/reference/extract_column_names.md)

- Functions to filter rows:
  [`data_match()`](https://easystats.github.io/datawizard/reference/data_match.md),
  [`data_filter()`](https://easystats.github.io/datawizard/reference/data_match.md)

## Examples

``` r
data_long <- read.table(header = TRUE, text = "
 subject sex condition measurement
       1   M   control         7.9
       1   M     cond1        12.3
       1   M     cond2        10.7
       2   F   control         6.3
       2   F     cond1        10.6
       2   F     cond2        11.1
       3   F   control         9.5
       3   F     cond1        13.1
       3   F     cond2        13.8
       4   M   control        11.5
       4   M     cond1        13.4
       4   M     cond2        12.9")

# converting long data into wide format
data_to_wide(
  data_long,
  id_cols = "subject",
  names_from = "condition",
  values_from = "measurement"
)
#>   subject control cond1 cond2
#> 1       1     7.9  12.3  10.7
#> 2       2     6.3  10.6  11.1
#> 3       3     9.5  13.1  13.8
#> 4       4    11.5  13.4  12.9

# converting long data into wide format with custom column names
data_to_wide(
  data_long,
  id_cols = "subject",
  names_from = "condition",
  values_from = "measurement",
  names_prefix = "Var.",
  names_sep = "."
)
#>   subject Var.control Var.cond1 Var.cond2
#> 1       1         7.9      12.3      10.7
#> 2       2         6.3      10.6      11.1
#> 3       3         9.5      13.1      13.8
#> 4       4        11.5      13.4      12.9

# converting long data into wide format, combining multiple columns
production <- expand.grid(
  product = c("A", "B"),
  country = c("AI", "EI"),
  year = 2000:2014
)
production <- data_filter(production, (product == "A" & country == "AI") | product == "B")
production$production <- rnorm(nrow(production))

data_to_wide(
  production,
  names_from = c("product", "country"),
  values_from = "production",
  names_glue = "prod_{product}_{country}"
)
#>    year   prod_A_AI  prod_B_AI   prod_B_EI
#> 1  2000 -0.21586539 -0.3349128 -1.08569914
#> 2  2001 -0.08542326  1.0706105 -0.14539355
#> 3  2002 -1.16554485 -0.8185157  0.68493608
#> 4  2003 -0.32005642 -1.3115224 -0.59960833
#> 5  2004 -0.12941069  0.8867361 -0.15139596
#> 6  2005  0.32979120 -3.2273228 -0.77179177
#> 7  2006  0.28654857 -1.2205120  0.43455038
#> 8  2007  0.80017687 -0.1639310  1.24291877
#> 9  2008 -0.93438506  0.3937087  0.40363146
#> 10 2009 -0.88643672 -1.3189376  0.02884391
#> 11 2010 -0.43212979  1.6898725  1.22839278
#> 12 2011  0.27602348 -1.0489755 -0.52086934
#> 13 2012  1.62320252 -1.0700682  1.68588724
#> 14 2013 -0.24168977 -0.4682005 -0.77297823
#> 15 2014  2.14991934 -1.3343536  0.49587048

# reshaping multiple long columns into wide format. to avoid duplicate
# column names, new names are a combination of the old column names in
# `values_from` and the values from `names_from`
data_long <- read.table(header = TRUE, text = "
subject_id time score anxiety test
         1    1    10       5   NA
         1    2    NA       7   NA
         2    1    15       6   NA
         2    2    12      NA   NA
         3    1    18       8   NA
         5    2    11       4   NA
         4    1    NA       5   NA
         4    2    14      NA   NA")

data_to_wide(
  data_long,
  id_cols = "subject_id",
  names_from = "time",
  values_from = c("score", "anxiety", "test")
)
#>   subject_id score_1 score_2 anxiety_1 anxiety_2 test_1 test_2
#> 1          1      10      NA         5         7     NA     NA
#> 2          2      15      12         6        NA     NA     NA
#> 3          3      18      NA         8        NA     NA     NA
#> 4          5      NA      11        NA         4     NA     NA
#> 5          4      NA      14         5        NA     NA     NA

# using the "sleepstudy" dataset
data(sleepstudy, package = "lme4")

# the sleepstudy data contains repeated measurements of average reaction
# times for each subjects over multiple days, in a sleep deprivation study.
# It is in long-format, i.e. each row corresponds to a single measurement.
# The variable "Days" contains the timepoint of the measurement, and
# "Reaction" contains the measurement itself. Converting this data to wide
# format will create a new column for each day, with the reaction time as the
# value.
head(sleepstudy)
#>   Reaction Days Subject
#> 1 249.5600    0     308
#> 2 258.7047    1     308
#> 3 250.8006    2     308
#> 4 321.4398    3     308
#> 5 356.8519    4     308
#> 6 414.6901    5     308

data_to_wide(
  sleepstudy,
  id_cols = "Subject",
  names_from = "Days",
  values_from = "Reaction"
)
#>    Subject        0        1        2        3        4        5        6
#> 1      308 249.5600 258.7047 250.8006 321.4398 356.8519 414.6901 382.2038
#> 2      309 222.7339 205.2658 202.9778 204.7070 207.7161 215.9618 213.6303
#> 3      310 199.0539 194.3322 234.3200 232.8416 229.3074 220.4579 235.4208
#> 4      330 321.5426 300.4002 283.8565 285.1330 285.7973 297.5855 280.2396
#> 5      331 287.6079 285.0000 301.8206 320.1153 316.2773 293.3187 290.0750
#> 6      332 234.8606 242.8118 272.9613 309.7688 317.4629 309.9976 454.1619
#> 7      333 283.8424 289.5550 276.7693 299.8097 297.1710 338.1665 332.0265
#> 8      334 265.4731 276.2012 243.3647 254.6723 279.0244 284.1912 305.5248
#> 9      335 241.6083 273.9472 254.4907 270.8021 251.4519 254.6362 245.4523
#> 10     337 312.3666 313.8058 291.6112 346.1222 365.7324 391.8385 404.2601
#> 11     349 236.1032 230.3167 238.9256 254.9220 250.7103 269.7744 281.5648
#> 12     350 256.2968 243.4543 256.2046 255.5271 268.9165 329.7247 379.4445
#> 13     351 250.5265 300.0576 269.8939 280.5891 271.8274 304.6336 287.7466
#> 14     352 221.6771 298.1939 326.8785 346.8555 348.7402 352.8287 354.4266
#> 15     369 271.9235 268.4369 257.2424 277.6566 314.8222 317.2135 298.1353
#> 16     370 225.2640 234.5235 238.9008 240.4730 267.5373 344.1937 281.1481
#> 17     371 269.8804 272.4428 277.8989 281.7895 279.1705 284.5120 259.2658
#> 18     372 269.4117 273.4740 297.5968 310.6316 287.1726 329.6076 334.4818
#>           7        8        9
#> 1  290.1486 430.5853 466.3535
#> 2  217.7272 224.2957 237.3142
#> 3  255.7511 261.0125 247.5153
#> 4  318.2613 305.3495 354.0487
#> 5  334.8177 293.7469 371.5811
#> 6  346.8311 330.3003 253.8644
#> 7  348.8399 333.3600 362.0428
#> 8  331.5229 335.7469 377.2990
#> 9  235.3110 235.7541 237.2466
#> 10 416.6923 455.8643 458.9167
#> 11 308.1020 336.2806 351.6451
#> 12 362.9184 394.4872 389.0527
#> 13 266.5955 321.5418 347.5655
#> 14 360.4326 375.6406 388.5417
#> 15 348.1229 340.2800 366.5131
#> 16 347.5855 365.1630 372.2288
#> 17 304.6306 350.7807 369.4692
#> 18 343.2199 369.1417 364.1236

# clearer column names
data_to_wide(
  sleepstudy,
  id_cols = "Subject",
  names_from = "Days",
  values_from = "Reaction",
  names_prefix = "Reaction_Day_"
)
#>    Subject Reaction_Day_0 Reaction_Day_1 Reaction_Day_2 Reaction_Day_3
#> 1      308       249.5600       258.7047       250.8006       321.4398
#> 2      309       222.7339       205.2658       202.9778       204.7070
#> 3      310       199.0539       194.3322       234.3200       232.8416
#> 4      330       321.5426       300.4002       283.8565       285.1330
#> 5      331       287.6079       285.0000       301.8206       320.1153
#> 6      332       234.8606       242.8118       272.9613       309.7688
#> 7      333       283.8424       289.5550       276.7693       299.8097
#> 8      334       265.4731       276.2012       243.3647       254.6723
#> 9      335       241.6083       273.9472       254.4907       270.8021
#> 10     337       312.3666       313.8058       291.6112       346.1222
#> 11     349       236.1032       230.3167       238.9256       254.9220
#> 12     350       256.2968       243.4543       256.2046       255.5271
#> 13     351       250.5265       300.0576       269.8939       280.5891
#> 14     352       221.6771       298.1939       326.8785       346.8555
#> 15     369       271.9235       268.4369       257.2424       277.6566
#> 16     370       225.2640       234.5235       238.9008       240.4730
#> 17     371       269.8804       272.4428       277.8989       281.7895
#> 18     372       269.4117       273.4740       297.5968       310.6316
#>    Reaction_Day_4 Reaction_Day_5 Reaction_Day_6 Reaction_Day_7 Reaction_Day_8
#> 1        356.8519       414.6901       382.2038       290.1486       430.5853
#> 2        207.7161       215.9618       213.6303       217.7272       224.2957
#> 3        229.3074       220.4579       235.4208       255.7511       261.0125
#> 4        285.7973       297.5855       280.2396       318.2613       305.3495
#> 5        316.2773       293.3187       290.0750       334.8177       293.7469
#> 6        317.4629       309.9976       454.1619       346.8311       330.3003
#> 7        297.1710       338.1665       332.0265       348.8399       333.3600
#> 8        279.0244       284.1912       305.5248       331.5229       335.7469
#> 9        251.4519       254.6362       245.4523       235.3110       235.7541
#> 10       365.7324       391.8385       404.2601       416.6923       455.8643
#> 11       250.7103       269.7744       281.5648       308.1020       336.2806
#> 12       268.9165       329.7247       379.4445       362.9184       394.4872
#> 13       271.8274       304.6336       287.7466       266.5955       321.5418
#> 14       348.7402       352.8287       354.4266       360.4326       375.6406
#> 15       314.8222       317.2135       298.1353       348.1229       340.2800
#> 16       267.5373       344.1937       281.1481       347.5855       365.1630
#> 17       279.1705       284.5120       259.2658       304.6306       350.7807
#> 18       287.1726       329.6076       334.4818       343.2199       369.1417
#>    Reaction_Day_9
#> 1        466.3535
#> 2        237.3142
#> 3        247.5153
#> 4        354.0487
#> 5        371.5811
#> 6        253.8644
#> 7        362.0428
#> 8        377.2990
#> 9        237.2466
#> 10       458.9167
#> 11       351.6451
#> 12       389.0527
#> 13       347.5655
#> 14       388.5417
#> 15       366.5131
#> 16       372.2288
#> 17       369.4692
#> 18       364.1236

# For unequal group sizes, missing information is filled with NA
d <- subset(sleepstudy, Days %in% c(0, 1, 2, 3, 4))[c(1:9, 11:13, 16:17, 21), ]

# long format, different number of "Subjects"
d
#>    Reaction Days Subject
#> 1  249.5600    0     308
#> 2  258.7047    1     308
#> 3  250.8006    2     308
#> 4  321.4398    3     308
#> 5  356.8519    4     308
#> 11 222.7339    0     309
#> 12 205.2658    1     309
#> 13 202.9778    2     309
#> 14 204.7070    3     309
#> 21 199.0539    0     310
#> 22 194.3322    1     310
#> 23 234.3200    2     310
#> 31 321.5426    0     330
#> 32 300.4002    1     330
#> 41 287.6079    0     331

data_to_wide(
  d,
  id_cols = "Subject",
  names_from = "Days",
  values_from = "Reaction",
  names_prefix = "Reaction_Day_"
)
#>   Subject Reaction_Day_0 Reaction_Day_1 Reaction_Day_2 Reaction_Day_3
#> 1     308       249.5600       258.7047       250.8006       321.4398
#> 2     309       222.7339       205.2658       202.9778       204.7070
#> 3     310       199.0539       194.3322       234.3200             NA
#> 4     330       321.5426       300.4002             NA             NA
#> 5     331       287.6079             NA             NA             NA
#>   Reaction_Day_4
#> 1       356.8519
#> 2             NA
#> 3             NA
#> 4             NA
#> 5             NA
```
