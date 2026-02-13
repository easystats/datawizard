# Recode values from one or more variables into a new variable

This functions recodes values from one or more variables into a new
variable. It is a convenient function to avoid nested
[`ifelse()`](https://rdrr.io/r/base/ifelse.html) statements, which is
similar to
[`dplyr::case_when()`](https://dplyr.tidyverse.org/reference/case-and-replace-when.html).

## Usage

``` r
recode_into(
  ...,
  data = NULL,
  default = NA,
  overwrite = TRUE,
  preserve_na = FALSE,
  verbose = TRUE
)
```

## Arguments

- ...:

  A sequence of two-sided formulas, where the left hand side (LHS) is a
  logical matching condition that determines which values match this
  case. The LHS of this formula is also called "recode pattern" (e.g.,
  in messages). The right hand side (RHS) indicates the replacement
  value.

- data:

  Optional, name of a data frame. This can be used to avoid writing the
  data name multiple times in `...`. See 'Examples'.

- default:

  Indicates the default value that is chosen when no match in the
  formulas in `...` is found. If not provided, `NA` is used as default
  value.

- overwrite:

  Logical, if `TRUE` (default) and more than one recode pattern apply to
  the same case, already recoded values will be overwritten by
  subsequent recode patterns. If `FALSE`, former recoded cases will not
  be altered by later recode patterns that would apply to those cases
  again. A warning message is printed to alert such situations and to
  avoid unintentional recodings.

- preserve_na:

  Logical, if `TRUE` and `default` is not `NA`, missing values in the
  original variable will be set back to `NA` in the recoded variable
  (unless overwritten by other recode patterns). If `FALSE`, missing
  values in the original variable will be recoded to `default`. Setting
  `preserve_na = TRUE` prevents unintentional overwriting of missing
  values with `default`, which means that you won't find valid values
  where the original data only had missing values. See 'Examples'.

- verbose:

  Toggle warnings.

## Value

A vector with recoded values.

## Examples

``` r
x <- 1:30
recode_into(
  x > 15 ~ "a",
  x > 10 & x <= 15 ~ "b",
  default = "c"
)
#>  [1] "c" "c" "c" "c" "c" "c" "c" "c" "c" "c" "b" "b" "b" "b" "b" "a" "a" "a" "a"
#> [20] "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a"

x <- 1:10
# default behaviour: second recode pattern "x > 5" overwrites
# some of the formerly recoded cases from pattern "x >= 3 & x <= 7"
recode_into(
  x >= 3 & x <= 7 ~ 1,
  x > 5 ~ 2,
  default = 0,
  verbose = FALSE
)
#>  [1] 0 0 1 1 1 2 2 2 2 2

# setting "overwrite = FALSE" will not alter formerly recoded cases
recode_into(
  x >= 3 & x <= 7 ~ 1,
  x > 5 ~ 2,
  default = 0,
  overwrite = FALSE,
  verbose = FALSE
)
#>  [1] 0 0 1 1 1 1 1 2 2 2

set.seed(123)
d <- data.frame(
  x = sample(1:5, 30, TRUE),
  y = sample(letters[1:5], 30, TRUE),
  stringsAsFactors = FALSE
)

# from different variables into new vector
recode_into(
  d$x %in% 1:3 & d$y %in% c("a", "b") ~ 1,
  d$x > 3 ~ 2,
  default = 0
)
#>  [1] 1 1 1 0 0 2 2 0 1 1 2 0 0 0 2 1 1 2 1 0 1 1 0 2 0 1 2 2 1 2

# no need to write name of data frame each time
recode_into(
  x %in% 1:3 & y %in% c("a", "b") ~ 1,
  x > 3 ~ 2,
  data = d,
  default = 0
)
#>  [1] 1 1 1 0 0 2 2 0 1 1 2 0 0 0 2 1 1 2 1 0 1 1 0 2 0 1 2 2 1 2

# handling of missing values
d <- data.frame(
  x = c(1, NA, 2, NA, 3, 4),
  y = c(1, 11, 3, NA, 5, 6)
)
# first NA in x is overwritten by valid value from y
# we have no known value for second NA in x and y,
# thus we get one NA in the result
recode_into(
  x <= 3 ~ 1,
  y > 5 ~ 2,
  data = d,
  default = 0,
  preserve_na = TRUE
)
#> [1]  1  2  1 NA  1  2
# first NA in x is overwritten by valid value from y
# default value is used for second NA
recode_into(
  x <= 3 ~ 1,
  y > 5 ~ 2,
  data = d,
  default = 0,
  preserve_na = FALSE
)
#> Missing values in original variable are overwritten by default value. If
#>   you want to preserve missing values, set `preserve_na = TRUE`.
#> [1] 1 2 1 0 1 2
```
