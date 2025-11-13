# Compute group-meaned and de-meaned variables

`demean()` computes group- and de-meaned versions of a variable that can
be used in regression analysis to model the between- and within-subject
effect (person-mean centering or centering within clusters). `degroup()`
is more generic in terms of the centering-operation. While `demean()`
always uses mean-centering, `degroup()` can also use the mode or median
for centering.

## Usage

``` r
demean(
  x,
  select,
  by,
  nested = FALSE,
  suffix_demean = "_within",
  suffix_groupmean = "_between",
  append = TRUE,
  add_attributes = TRUE,
  verbose = TRUE
)

degroup(
  x,
  select,
  by,
  nested = FALSE,
  center = "mean",
  suffix_demean = "_within",
  suffix_groupmean = "_between",
  append = TRUE,
  add_attributes = TRUE,
  verbose = TRUE
)

detrend(
  x,
  select,
  by,
  nested = FALSE,
  center = "mean",
  suffix_demean = "_within",
  suffix_groupmean = "_between",
  append = TRUE,
  add_attributes = TRUE,
  verbose = TRUE
)
```

## Arguments

- x:

  A data frame.

- select:

  Character vector (or formula) with names of variables to select that
  should be group- and de-meaned.

- by:

  Character vector (or formula) with the name of the variable that
  indicates the group- or cluster-ID. For cross-classified or nested
  designs, `by` can also identify two or more variables as group- or
  cluster-IDs. If the data is nested and should be treated as such, set
  `nested = TRUE`. Else, if `by` defines two or more variables and
  `nested = FALSE`, a cross-classified design is assumed. Note that
  `demean()` and `degroup()` can't handle a mix of nested and
  cross-classified designs in one model.

  For nested designs, `by` can be:

  - a character vector with the name of the variable that indicates the
    levels, ordered from *highest* level to *lowest* (e.g.
    `by = c("L4", "L3", "L2")`.

  - a character vector with variable names in the format
    `by = "L4/L3/L2"`, where the levels are separated by `/`.

  See also section *De-meaning for cross-classified designs* and
  *De-meaning for nested designs* below.

- nested:

  Logical, if `TRUE`, the data is treated as nested. If `FALSE`, the
  data is treated as cross-classified. Only applies if `by` contains
  more than one variable.

- suffix_demean, suffix_groupmean:

  String value, will be appended to the names of the group-meaned and
  de-meaned variables of `x`. By default, de-meaned variables will be
  suffixed with `"_within"` and grouped-meaned variables with
  `"_between"`.

- append:

  Logical, if `TRUE` (default), the group- and de-meaned variables will
  be appended (column bind) to the original data `x`, thus returning
  both the original and the de-/group-meaned variables.

- add_attributes:

  Logical, if `TRUE`, the returned variables gain attributes to indicate
  the within- and between-effects. This is only relevant when printing
  `model_parameters()` - in such cases, the within- and between-effects
  are printed in separated blocks.

- verbose:

  Toggle warnings and messages.

- center:

  Method for centering. `demean()` always performs mean-centering, while
  `degroup()` can use `center = "median"` or `center = "mode"` for
  median- or mode-centering, and also `"min"` or `"max"`.

## Value

A data frame with the group-/de-meaned variables, which get the suffix
`"_between"` (for the group-meaned variable) and `"_within"` (for the
de-meaned variable) by default. For cross-classified or nested designs,
the name pattern of the group-meaned variables is the name of the
centered variable followed by the name of the variable that indicates
the related grouping level, e.g. `predictor_L3_between` and
`predictor_L2_between`.

## Heterogeneity Bias

Mixed models include different levels of sources of variability, i.e.
error terms at each level. When macro-indicators (or level-2 predictors,
or higher-level units, or more general: *group-level predictors that
**vary** within and across groups*) are included as fixed effects (i.e.
treated as covariate at level-1), the variance that is left unaccounted
for this covariate will be absorbed into the error terms of level-1 and
level-2 (*Bafumi and Gelman 2006; Gelman and Hill 2007, Chapter 12.6.*):
"Such covariates contain two parts: one that is specific to the
higher-level entity that does not vary between occasions, and one that
represents the difference between occasions, within higher-level
entities" (*Bell et al. 2015*). Hence, the error terms will be
correlated with the covariate, which violates one of the assumptions of
mixed models (iid, independent and identically distributed error terms).
This bias is also called the *heterogeneity bias* (*Bell et al. 2015*).
To resolve this problem, level-2 predictors used as (level-1) covariates
should be separated into their "within" and "between" effects by
"de-meaning" and "group-meaning": After demeaning time-varying
predictors, "at the higher level, the mean term is no longer constrained
by Level 1 effects, so it is free to account for all the higher-level
variance associated with that variable" (*Bell et al. 2015*).

## Panel data and correlating fixed and group effects

`demean()` is intended to create group- and de-meaned variables for
panel regression models (fixed effects models), or for complex
random-effect-within-between models (see *Bell et al. 2015, 2018*),
where group-effects (random effects) and fixed effects correlate (see
*Bafumi and Gelman 2006*). This can happen, for instance, when analyzing
panel data, which can lead to *Heterogeneity Bias*. To control for
correlating predictors and group effects, it is recommended to include
the group-meaned and de-meaned version of *time-varying covariates* (and
group-meaned version of *time-invariant covariates* that are on a higher
level, e.g. level-2 predictors) in the model. By this, one can fit
complex multilevel models for panel data, including time-varying
predictors, time-invariant predictors and random effects.

## Why mixed models are preferred over fixed effects models

A mixed models approach can model the causes of endogeneity explicitly
by including the (separated) within- and between-effects of time-varying
fixed effects and including time-constant fixed effects. Furthermore,
mixed models also include random effects, thus a mixed models approach
is superior to classic fixed-effects models, which lack information of
variation in the group-effects or between-subject effects. Furthermore,
fixed effects regression cannot include random slopes, which means that
fixed effects regressions are neglecting "cross-cluster differences in
the effects of lower-level controls (which) reduces the precision of
estimated context effects, resulting in unnecessarily wide confidence
intervals and low statistical power" (*Heisig et al. 2017*).

## Terminology

The group-meaned variable is simply the mean of an independent variable
within each group (or id-level or cluster) represented by `by`. It
represents the cluster-mean of an independent variable. The regression
coefficient of a group-meaned variable is the *between-subject-effect*.
The de-meaned variable is then the centered version of the group-meaned
variable. De-meaning is sometimes also called person-mean centering or
centering within clusters. The regression coefficient of a de-meaned
variable represents the *within-subject-effect*.

## De-meaning with continuous predictors

For continuous time-varying predictors, the recommendation is to include
both their de-meaned and group-meaned versions as fixed effects, but not
the raw (untransformed) time-varying predictors themselves. The
de-meaned predictor should also be included as random effect (random
slope). In regression models, the coefficient of the de-meaned
predictors indicates the within-subject effect, while the coefficient of
the group-meaned predictor indicates the between-subject effect.

## De-meaning with binary predictors

For binary time-varying predictors, there are two recommendations. First
is to include the raw (untransformed) binary predictor as fixed effect
only and the *de-meaned* variable as random effect (random slope). The
alternative would be to add the de-meaned version(s) of binary
time-varying covariates as additional fixed effect as well (instead of
adding it as random slope). Centering time-varying binary variables to
obtain within-effects (level 1) isn't necessary. They have a sensible
interpretation when left in the typical 0/1 format (*Hoffmann 2015,
chapter 8-2.I*). `demean()` will thus coerce categorical time-varying
predictors to numeric to compute the de- and group-meaned versions for
these variables, where the raw (untransformed) binary predictor and the
de-meaned version should be added to the model.

## De-meaning of factors with more than 2 levels

Factors with more than two levels are demeaned in two ways: first, these
are also converted to numeric and de-meaned; second, dummy variables are
created (binary, with 0/1 coding for each level) and these binary
dummy-variables are de-meaned in the same way (as described above).
Packages like **panelr** internally convert factors to dummies before
demeaning, so this behaviour can be mimicked here.

## De-meaning interaction terms

There are multiple ways to deal with interaction terms of within- and
between-effects.

- A classical approach is to simply use the product term of the
  de-meaned variables (i.e. introducing the de-meaned variables as
  interaction term in the model formula, e.g.
  `y ~ x_within * time_within`). This approach, however, might be
  subject to bias (see *Giesselmann & Schmidt-Catran 2020*).

- Another option is to first calculate the product term and then apply
  the de-meaning to it. This approach produces an estimator "that
  reflects unit-level differences of interacted variables whose
  moderators vary within units", which is desirable if *no* within
  interaction of two time-dependent variables is required. This is what
  `demean()` does internally when `select` contains interaction terms.

- A third option, when the interaction should result in a genuine within
  estimator, is to "double de-mean" the interaction terms (*Giesselmann
  & Schmidt-Catran 2018*), however, this is currently not supported by
  `demean()`. If this is required, the `wmb()` function from the
  **panelr** package should be used.

To de-mean interaction terms for within-between models, simply specify
the term as interaction for the `select`-argument, e.g. `select = "a*b"`
(see 'Examples').

## De-meaning for cross-classified designs

`demean()` can handle cross-classified designs, where the data has two
or more groups at the higher (i.e. second) level. In such cases, the
`by`-argument can identify two or more variables that represent the
cross-classified group- or cluster-IDs. The de-meaned variables for
cross-classified designs are simply subtracting all group means from
each individual value, i.e. *fully cluster-mean-centering* (see *Guo et
al. 2024* for details). Note that de-meaning for cross-classified
designs is *not* equivalent to de-meaning of nested data structures from
models with three or more levels. Set `nested = TRUE` to explicitly
assume a nested design. For cross-classified designs, de-meaning is
supposed to work for models like `y ~ x + (1|level3) + (1|level2)`, but
*not* for models like `y ~ x + (1|level3/level2)`. Note that `demean()`
and `degroup()` can't handle a mix of nested and cross-classified
designs in one model.

## De-meaning for nested designs

*Brincks et al. (2017)* have suggested an algorithm to center variables
for nested designs, which is implemented in `demean()`. For nested
designs, set `nested = TRUE` *and* specify the variables that indicate
the different levels in descending order in the `by` argument. E.g.,
`by = c("level4", "level3, "level2")` assumes a model like
`y ~ x + (1|level4/level3/level2)`. An alternative notation for the
`by`-argument would be `by = "level4/level3/level2"`, similar to the
formula notation.

## Analysing panel data with mixed models using lme4

A description of how to translate the formulas described in *Bell et al.
2018* into R using `lmer()` from **lme4** can be found in [this
vignette](https://easystats.github.io/parameters/articles/demean.html).

## References

- Bafumi J, Gelman A. 2006. Fitting Multilevel Models When Predictors
  and Group Effects Correlate. In. Philadelphia, PA: Annual meeting of
  the American Political Science Association.

- Bell A, Fairbrother M, Jones K. 2019. Fixed and Random Effects Models:
  Making an Informed Choice. Quality & Quantity (53); 1051-1074

- Bell A, Jones K. 2015. Explaining Fixed Effects: Random Effects
  Modeling of Time-Series Cross-Sectional and Panel Data. Political
  Science Research and Methods, 3(1), 133–153.

- Brincks, A. M., Enders, C. K., Llabre, M. M., Bulotsky-Shearer, R. J.,
  Prado, G., and Feaster, D. J. (2017). Centering Predictor Variables in
  Three-Level Contextual Models. Multivariate Behavioral Research,
  52(2), 149–163. https://doi.org/10.1080/00273171.2016.1256753

- Gelman A, Hill J. 2007. Data Analysis Using Regression and
  Multilevel/Hierarchical Models. Analytical Methods for Social
  Research. Cambridge, New York: Cambridge University Press

- Giesselmann M, Schmidt-Catran, AW. 2020. Interactions in fixed effects
  regression models. Sociological Methods & Research, 1–28.
  https://doi.org/10.1177/0049124120914934

- Guo Y, Dhaliwal J, Rights JD. 2024. Disaggregating level-specific
  effects in cross-classified multilevel models. Behavior Research
  Methods, 56(4), 3023–3057.

- Heisig JP, Schaeffer M, Giesecke J. 2017. The Costs of Simplicity: Why
  Multilevel Models May Benefit from Accounting for Cross-Cluster
  Differences in the Effects of Controls. American Sociological Review
  82 (4): 796–827.

- Hoffman L. 2015. Longitudinal analysis: modeling within-person
  fluctuation and change. New York: Routledge

## See also

If grand-mean centering (instead of centering within-clusters) is
required, see
[`center()`](https://easystats.github.io/datawizard/reference/center.md).
See
[`performance::check_group_variation()`](https://easystats.github.io/performance/reference/check_group_variation.html)
to check for heterogeneity bias.

## Examples

``` r

data(iris)
iris$ID <- sample(1:4, nrow(iris), replace = TRUE) # fake-ID
iris$binary <- as.factor(rbinom(150, 1, .35)) # binary variable

x <- demean(iris, select = c("Sepal.Length", "Petal.Length"), by = "ID")
head(x)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species ID binary
#> 1          5.1         3.5          1.4         0.2  setosa  2      0
#> 2          4.9         3.0          1.4         0.2  setosa  2      0
#> 3          4.7         3.2          1.3         0.2  setosa  3      1
#> 4          4.6         3.1          1.5         0.2  setosa  4      0
#> 5          5.0         3.6          1.4         0.2  setosa  4      0
#> 6          5.4         3.9          1.7         0.4  setosa  2      0
#>   Sepal.Length_between Petal.Length_between Sepal.Length_within
#> 1             5.681579             3.434211          -0.5815789
#> 2             5.681579             3.434211          -0.7815789
#> 3             5.789189             3.451351          -1.0891892
#> 4             5.748718             3.684615          -1.1487179
#> 5             5.748718             3.684615          -0.7487179
#> 6             5.681579             3.434211          -0.2815789
#>   Petal.Length_within
#> 1           -2.034211
#> 2           -2.034211
#> 3           -2.151351
#> 4           -2.184615
#> 5           -2.284615
#> 6           -1.734211

x <- demean(iris, select = c("Sepal.Length", "binary", "Species"), by = "ID")
#> Categorical predictors (binary, Species) have been coerced to numeric
#>   values to compute de- and group-meaned variables.
head(x)
#>   Sepal.Length Sepal.Width Petal.Length Petal.Width Species ID binary
#> 1          5.1         3.5          1.4         0.2  setosa  2      0
#> 2          4.9         3.0          1.4         0.2  setosa  2      0
#> 3          4.7         3.2          1.3         0.2  setosa  3      1
#> 4          4.6         3.1          1.5         0.2  setosa  4      0
#> 5          5.0         3.6          1.4         0.2  setosa  4      0
#> 6          5.4         3.9          1.7         0.4  setosa  2      0
#>   Sepal.Length_between binary_between Species_between Species_setosa_between
#> 1             5.681579      0.4473684       0.8947368              0.4210526
#> 2             5.681579      0.4473684       0.8947368              0.4210526
#> 3             5.789189      0.2432432       0.8648649              0.4324324
#> 4             5.748718      0.2307692       0.9487179              0.2820513
#> 5             5.748718      0.2307692       0.9487179              0.2820513
#> 6             5.681579      0.4473684       0.8947368              0.4210526
#>   Species_versicolor_between Species_virginica_between Sepal.Length_within
#> 1                  0.2631579                 0.3157895          -0.5815789
#> 2                  0.2631579                 0.3157895          -0.7815789
#> 3                  0.2702703                 0.2972973          -1.0891892
#> 4                  0.4871795                 0.2307692          -1.1487179
#> 5                  0.4871795                 0.2307692          -0.7487179
#> 6                  0.2631579                 0.3157895          -0.2815789
#>   binary_within Species_within Species_setosa_within Species_versicolor_within
#> 1    -0.4473684     -0.8947368             0.5789474                -0.2631579
#> 2    -0.4473684     -0.8947368             0.5789474                -0.2631579
#> 3     0.7567568     -0.8648649             0.5675676                -0.2702703
#> 4    -0.2307692     -0.9487179             0.7179487                -0.4871795
#> 5    -0.2307692     -0.9487179             0.7179487                -0.4871795
#> 6    -0.4473684     -0.8947368             0.5789474                -0.2631579
#>   Species_virginica_within
#> 1               -0.3157895
#> 2               -0.3157895
#> 3               -0.2972973
#> 4               -0.2307692
#> 5               -0.2307692
#> 6               -0.3157895


# demean interaction term x*y
dat <- data.frame(
  a = c(1, 2, 3, 4, 1, 2, 3, 4),
  x = c(4, 3, 3, 4, 1, 2, 1, 2),
  y = c(1, 2, 1, 2, 4, 3, 2, 1),
  ID = c(1, 2, 3, 1, 2, 3, 1, 2)
)
demean(dat, select = c("a", "x*y"), by = "ID")
#>   a x y ID a_between x_y_between   a_within x_y_within
#> 1 1 4 1  1  2.666667    4.666667 -1.6666667 -0.6666667
#> 2 2 3 2  2  2.333333    4.000000 -0.3333333  2.0000000
#> 3 3 3 1  3  2.500000    4.500000  0.5000000 -1.5000000
#> 4 4 4 2  1  2.666667    4.666667  1.3333333  3.3333333
#> 5 1 1 4  2  2.333333    4.000000 -1.3333333  0.0000000
#> 6 2 2 3  3  2.500000    4.500000 -0.5000000  1.5000000
#> 7 3 1 2  1  2.666667    4.666667  0.3333333 -2.6666667
#> 8 4 2 1  2  2.333333    4.000000  1.6666667 -2.0000000

# or in formula-notation
demean(dat, select = ~ a + x * y, by = ~ID)
#>   a x y ID a_between x_y_between   a_within x_y_within
#> 1 1 4 1  1  2.666667    4.666667 -1.6666667 -0.6666667
#> 2 2 3 2  2  2.333333    4.000000 -0.3333333  2.0000000
#> 3 3 3 1  3  2.500000    4.500000  0.5000000 -1.5000000
#> 4 4 4 2  1  2.666667    4.666667  1.3333333  3.3333333
#> 5 1 1 4  2  2.333333    4.000000 -1.3333333  0.0000000
#> 6 2 2 3  3  2.500000    4.500000 -0.5000000  1.5000000
#> 7 3 1 2  1  2.666667    4.666667  0.3333333 -2.6666667
#> 8 4 2 1  2  2.333333    4.000000  1.6666667 -2.0000000
```
