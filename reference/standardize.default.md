# Re-fit a model with standardized data

Performs a standardization of data (z-scoring) using
[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)
and then re-fits the model to the standardized data.\
\
Standardization is done by completely refitting the model on the
standardized data. Hence, this approach is equal to standardizing the
variables *before* fitting the model and will return a new model object.
This method is particularly recommended for complex models that include
interactions or transformations (e.g., polynomial or spline terms). The
`robust` (default to `FALSE`) argument enables a robust standardization
of data, based on the `median` and the `MAD` instead of the `mean` and
the `SD`.

## Usage

``` r
# Default S3 method
standardize(
  x,
  robust = FALSE,
  two_sd = FALSE,
  weights = TRUE,
  verbose = TRUE,
  include_response = TRUE,
  ...
)
```

## Arguments

- x:

  A statistical model.

- robust:

  Logical, if `TRUE`, centering is done by subtracting the median from
  the variables and dividing it by the median absolute deviation (MAD).
  If `FALSE`, variables are standardized by subtracting the mean and
  dividing it by the standard deviation (SD).

- two_sd:

  If `TRUE`, the variables are scaled by two times the deviation (SD or
  MAD depending on `robust`). This method can be useful to obtain model
  coefficients of continuous parameters comparable to coefficients
  related to binary predictors, when applied to **the predictors** (not
  the outcome) (Gelman, 2008).

- weights:

  If `TRUE` (default), a weighted-standardization is carried out.

- verbose:

  Toggle warnings and messages on or off.

- include_response:

  If `TRUE` (default), the response value will also be standardized. If
  `FALSE`, only the predictors will be standardized.

  - Note that for GLMs and models with non-linear link functions, the
    response value will not be standardized, to make re-fitting the
    model work.

  - If the model contains an
    [`stats::offset()`](https://rdrr.io/r/stats/offset.html), the offset
    variable(s) will be standardized only if the response is
    standardized. If `two_sd = TRUE`, offsets are standardized by one-sd
    (similar to the response).

  - (For `mediate` models, the `include_response` refers to the outcome
    in the y model; m model's response will always be standardized when
    possible).

- ...:

  Arguments passed to or from other methods.

## Value

A statistical model fitted on standardized data

## Generalized Linear Models

Standardization for generalized linear models (GLM, GLMM, etc) is done
only with respect to the predictors (while the outcome remains as-is,
unstandardized) - maintaining the interpretability of the coefficients
(e.g., in a binomial model: the exponent of the standardized parameter
is the OR of a change of 1 SD in the predictor, etc.)

## Dealing with Factors

`standardize(model)` or
`standardize_parameters(model, method = "refit")` do *not* standardize
categorical predictors (i.e. factors) / their dummy-variables, which may
be a different behaviour compared to other R packages (such as
**lm.beta**) or other software packages (like SPSS). To mimic such
behaviours, either use `standardize_parameters(model, method = "basic")`
to obtain post-hoc standardized parameters, or standardize the data with
`standardize(data, force = TRUE)` *before* fitting the model.

## Transformed Variables

When the model's formula contains transformations (e.g. `y ~ exp(X)`)
the transformation effectively takes place after standardization (e.g.,
`exp(scale(X))`). Since some transformations are undefined for none
positive values, such as [`log()`](https://rdrr.io/r/base/Log.html) and
[`sqrt()`](https://rdrr.io/r/base/MathFun.html), the relevel variables
are shifted (post standardization) by `Z - min(Z) + 1` or `Z - min(Z)`
(respectively).

## See also

Other standardize:
[`standardize()`](https://easystats.github.io/datawizard/reference/standardize.md)

## Examples

``` r
model <- lm(Infant.Mortality ~ Education * Fertility, data = swiss)
coef(standardize(model))
#>         (Intercept)           Education           Fertility Education:Fertility 
#>          0.06386069          0.47482848          0.63270919          0.09829777 
```
