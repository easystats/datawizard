# Rescale design weights for multilevel analysis

Most functions to fit multilevel and mixed effects models only allow the
user to specify frequency weights, but not design (i.e., sampling or
probability) weights, which should be used when analyzing complex
samples (e.g., probability samples). `rescale_weights()` implements two
algorithms, one proposed by Asparouhov (2006) and Carle (2009), to
rescale design weights in survey data to account for the grouping
structure of multilevel models, and one based on the design effect
proposed by Kish (1965), to rescale weights by the design effect to
account for additional sampling error introduced by weighting.

## Usage

``` r
rescale_weights(
  data,
  probability_weights = NULL,
  by = NULL,
  nest = FALSE,
  method = "carle"
)
```

## Arguments

- data:

  A data frame.

- probability_weights:

  Variable indicating the probability (design or sampling) weights of
  the survey data (level-1-weight), provided as character string or
  formula.

- by:

  Variable names (as character vector, or as formula), indicating the
  grouping structure (strata) of the survey data (level-2-cluster
  variable). It is also possible to create weights for multiple group
  variables; in such cases, each created weighting variable will be
  suffixed by the name of the group variable. This argument is required
  for `method = "carle"`, but optional for `method = "kish"`.

- nest:

  Logical, if `TRUE` and `by` indicates at least two group variables,
  then groups are "nested", i.e. groups are now a combination from each
  group level of the variables in `by`. This argument is not used when
  `method = "kish"`.

- method:

  String, indicating which rescale-method is used for rescaling weights.
  Can be either `"carle"` (default) or `"kish"`. See 'Details'. If
  `method = "carle"`, the `by` argument is required.

## Value

`data`, including the new weighting variable(s). For `method = "carle"`,
new columns `rescaled_weights_a` and `rescaled_weights_b` are returned,
and for `method = "kish"`, the returned data contains a column
`rescaled_weights`. These represent the rescaled design weights to use
in multilevel models (use these variables for the `weights` argument).

## Details

- `method = "carle"`

  Rescaling is based on two methods: For `rescaled_weights_a`, the
  sample weights `probability_weights` are adjusted by a factor that
  represents the proportion of group size divided by the sum of sampling
  weights within each group. The adjustment factor for
  `rescaled_weights_b` is the sum of sample weights within each group
  divided by the sum of squared sample weights within each group (see
  Carle (2009), Appendix B). In other words, `rescaled_weights_a`
  "scales the weights so that the new weights sum to the cluster sample
  size" while `rescaled_weights_b` "scales the weights so that the new
  weights sum to the effective cluster size".

  Regarding the choice between scaling methods A and B, Carle suggests
  that "analysts who wish to discuss point estimates should report
  results based on weighting method A. For analysts more interested in
  residual between-group variance, method B may generally provide the
  least biased estimates". In general, it is recommended to fit a
  non-weighted model and weighted models with both scaling methods and
  when comparing the models, see whether the "inferential decisions
  converge", to gain confidence in the results.

  Though the bias of scaled weights decreases with increasing group
  size, method A is preferred when insufficient or low group size is a
  concern.

  The group ID and probably PSU may be used as random effects (e.g.
  nested design, or group and PSU as varying intercepts), depending on
  the survey design that should be mimicked.

- `method = "kish"`

  Rescaling is based on scaling the sample weights so the mean value is
  1, which means the sum of all weights equals the sample size. Next,
  the design effect (*Kish 1965*) is calculated, which is the mean of
  the squared weights divided by the squared mean of the weights. The
  scaled sample weights are then divided by the design effect. This
  method is most appropriate when weights are based on additional
  variables beyond the grouping variables in the model (e.g., other
  demographic characteristics), but may also be useful in other
  contexts.

  Some tests on real-world survey-data suggest that, in comparison to
  the Carle-method, the Kish-method comes closer to estimates from a
  regular survey-design using the **survey** package. Note that these
  tests are not representative and it is recommended to check your
  results against a standard survey-design.

## References

- Asparouhov T. (2006). General Multi-Level Modeling with Sampling
  Weights. Communications in Statistics - Theory and Methods 35: 439-460

- Carle A.C. (2009). Fitting multilevel models in complex survey data
  with design weights: Recommendations. BMC Medical Research Methodology
  9(49): 1-13

- Kish, L. (1965) Survey Sampling. London: Wiley.

## Examples

``` r
data(nhanes_sample)
head(rescale_weights(nhanes_sample, "WTINT2YR", "SDMVSTRA"))
#>   total  age RIAGENDR RIDRETH1 SDMVPSU SDMVSTRA WTINT2YR rescaled_weights_a
#> 1     1 2.20        1        3       2       31 97593.68          1.5733612
#> 2     7 2.08        2        3       1       29 39599.36          0.6231745
#> 3     3 1.48        2        1       2       42 26619.83          0.8976966
#> 4     4 1.32        2        4       2       33 34998.53          0.7083628
#> 5     1 2.00        2        1       1       41 14746.45          0.4217782
#> 6     6 2.20        2        4       1       38 28232.10          0.6877550
#>   rescaled_weights_b
#> 1          1.2005159
#> 2          0.5246593
#> 3          0.5439111
#> 4          0.5498944
#> 5          0.3119698
#> 6          0.5155503

# also works with multiple group-variables
head(rescale_weights(nhanes_sample, "WTINT2YR", c("SDMVSTRA", "SDMVPSU")))
#>   total  age RIAGENDR RIDRETH1 SDMVPSU SDMVSTRA WTINT2YR pweight_a_SDMVSTRA
#> 1     1 2.20        1        3       2       31 97593.68          1.5733612
#> 2     7 2.08        2        3       1       29 39599.36          0.6231745
#> 3     3 1.48        2        1       2       42 26619.83          0.8976966
#> 4     4 1.32        2        4       2       33 34998.53          0.7083628
#> 5     1 2.00        2        1       1       41 14746.45          0.4217782
#> 6     6 2.20        2        4       1       38 28232.10          0.6877550
#>   pweight_b_SDMVSTRA pweight_a_SDMVPSU pweight_b_SDMVPSU
#> 1          1.2005159         1.8458164         1.3699952
#> 2          0.5246593         0.8217570         0.5780808
#> 3          0.5439111         0.5034683         0.3736824
#> 4          0.5498944         0.6619369         0.4913004
#> 5          0.3119698         0.3060151         0.2152722
#> 6          0.5155503         0.5858662         0.4121388

# or nested structures.
x <- rescale_weights(
  data = nhanes_sample,
  probability_weights = "WTINT2YR",
  by = c("SDMVSTRA", "SDMVPSU"),
  nest = TRUE
)
head(x)
#>   total  age RIAGENDR RIDRETH1 SDMVPSU SDMVSTRA WTINT2YR rescaled_weights_a
#> 1     1 2.20        1        3       2       31 97593.68          1.6532834
#> 2     7 2.08        2        3       1       29 39599.36          0.5492655
#> 3     3 1.48        2        1       2       42 26619.83          0.8341084
#> 4     4 1.32        2        4       2       33 34998.53          0.7937824
#> 5     1 2.00        2        1       1       41 14746.45          0.4285939
#> 6     6 2.20        2        4       1       38 28232.10          0.6215579
#>   rescaled_weights_b
#> 1          1.3583967
#> 2          0.4875798
#> 3          0.5891700
#> 4          0.5941006
#> 5          0.3068214
#> 6          0.4759935

# \donttest{
# compare different methods, using multilevel-Poisson regression

d <- rescale_weights(nhanes_sample, "WTINT2YR", "SDMVSTRA")
result1 <- lme4::glmer(
  total ~ factor(RIAGENDR) + log(age) + factor(RIDRETH1) + (1 | SDMVPSU),
  family = poisson(),
  data = d,
  weights = rescaled_weights_a
)
result2 <- lme4::glmer(
  total ~ factor(RIAGENDR) + log(age) + factor(RIDRETH1) + (1 | SDMVPSU),
  family = poisson(),
  data = d,
  weights = rescaled_weights_b
)

d <- rescale_weights(
  nhanes_sample,
  "WTINT2YR",
  method = "kish"
)
result3 <- lme4::glmer(
  total ~ factor(RIAGENDR) + log(age) + factor(RIDRETH1) + (1 | SDMVPSU),
  family = poisson(),
  data = d,
  weights = rescaled_weights
)
d <- rescale_weights(
  nhanes_sample,
  "WTINT2YR",
  "SDMVSTRA",
  method = "kish"
)
result4 <- lme4::glmer(
  total ~ factor(RIAGENDR) + log(age) + factor(RIDRETH1) + (1 | SDMVPSU),
  family = poisson(),
  data = d,
  weights = rescaled_weights
)
parameters::compare_parameters(
  list(result1, result2, result3, result4),
  exponentiate = TRUE,
  column_names = c("Carle (A)", "Carle (B)", "Kish", "Kish (grouped)")
)
#> Number of weighted observations differs from number of unweighted
#>   observations.
#> Parameter    |            Carle (A) |            Carle (B)
#> ----------------------------------------------------------
#> (Intercept)  | 12.20 (10.52, 14.14) | 11.95 (10.27, 13.92)
#> RIAGENDR [2] |  0.41 ( 0.40,  0.42) |  0.42 ( 0.40,  0.43)
#> age [log]    |  1.69 ( 1.63,  1.75) |  1.66 ( 1.60,  1.73)
#> RIDRETH1 [2] |  0.90 ( 0.84,  0.97) |  0.90 ( 0.83,  0.98)
#> RIDRETH1 [3] |  1.19 ( 1.14,  1.24) |  1.21 ( 1.16,  1.27)
#> RIDRETH1 [4] |  2.16 ( 2.07,  2.26) |  2.16 ( 2.06,  2.28)
#> RIDRETH1 [5] |  1.01 ( 0.95,  1.07) |  1.05 ( 0.97,  1.12)
#> ----------------------------------------------------------
#> Observations |                 2617 |                 1965
#> 
#> Parameter    |                Kish |       Kish (grouped)
#> ---------------------------------------------------------
#> (Intercept)  | 11.72 (9.89, 13.87) | 11.95 (10.27, 13.92)
#> RIAGENDR [2] |  0.42 (0.41,  0.43) |  0.42 ( 0.40,  0.43)
#> age [log]    |  1.66 (1.59,  1.73) |  1.66 ( 1.60,  1.73)
#> RIDRETH1 [2] |  0.98 (0.90,  1.07) |  0.90 ( 0.83,  0.98)
#> RIDRETH1 [3] |  1.23 (1.17,  1.29) |  1.21 ( 1.16,  1.27)
#> RIDRETH1 [4] |  2.11 (1.99,  2.23) |  2.16 ( 2.06,  2.28)
#> RIDRETH1 [5] |  1.09 (1.01,  1.18) |  1.05 ( 0.97,  1.12)
#> ---------------------------------------------------------
#> Observations |                1903 |                 1965
# }
```
